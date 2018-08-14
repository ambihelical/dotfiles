#!/bin/bash
# Keep above line to force syntax highlighting

# include sh/dash profile
[ -e ${HOME}/.profile ] && source ${HOME}/.profile

# setup path. These are in reverse order of how they appear in the PATH value.

[ -d ~/extern/gccarm-dev/bin ] && PATH=~/extern/gccarm-dev/bin:"${PATH}"    # arm gcc cross compiler
[ -d /usr/lib/ccache ] && PATH=/usr/lib/ccache:"${PATH}"                    # compile cache
[ -d ~/.local/bin ] && PATH=~/.local/bin:"${PATH}"                            # locally installed execs
[ -d ~/bin ] && PATH=~/bin:"${PATH}"                                        # my utilities
[ -d ~/bin/${OSTYPE} ] && PATH=~/bin/${OSTYPE}:"${PATH}"                    # my utilities, os specific
[ -d ~/Android/Sdk/platform-tools ] && PATH=~/Android/Sdk/platform-tools:"${PATH}"         # adb, fastboot, etc

export ACKRC=${XDG_CONFIG_HOME}/ack
[ -d ~/extern/ChibiOS-RT ] && export CHIBIOS=~/extern/ChibiOS-RT            # chibios development

# If not running interactively, don't do anything more
[ -z "$PS1" ] && return

################ Interactive Portion ###############

# echo only the outside N characters of the string provided, using an ellipsis to
# indicate removed characters.  Strings shorter than N are unmolested.
# $1 - string to echo
# $2 - maximum length (default is 5)
#
# Todo: use "locale charmap" to detect non-UTF8 terminal, and substitute
# an ascii string instead of ellipsis character
#
truncm() {
	local str=$1
	local maxlen=${2:-5}
	local len=${#str}
	if [ ${len} -gt ${maxlen} ]; then
		local mid1=$((${maxlen}/2))
		local mid2=$(((${maxlen}+1)/2-1))
		str=${str:0:$mid1}…${str:${len}-${mid2}}
	fi
	echo $str
}

# if input string starts with home directory, substitute ~
tilde() {
	echo ${*/${HOME}/\~}
}

ccolumn()
{
	 local COL
	 local ROW
	 IFS=';' read -sdR -p $'\E[6n' ROW COL
	 echo "${COL}"
}

# Print fancy git prompt information
# Heavily modified from bash-powerline
# - For speed, invoke git once instead of several times
# - add remote branch and change formating
gprompt() {
	[ -x "$(which git)" ] || return    # git not found
	local branch_changed_symbol='•'
	local need_push_symbol='⇡'
	local need_pull_symbol='⇣'

	local git_eng="env LANG=C git"   # force git output in English to make our work easier

	# get 1st two lines. First will be status line
	# second may have modification status for first file
	declare -a lines
	mapfile -t lines < <($git_eng status --porcelain --branch --untracked-files=no 2>/dev/null|head -2)
	[ -n "${lines[0]}" ] || return  # git branch not found

	# how many commits local branch is ahead/behind of remote?
	# looks like: ## local-branch...remote-branch [ahead 9]
	#         or: ## local-branch...remote-branch [behind 9]
	#         or: ## local-branch...remote-branch
	#         or: ## local-branch
	#         or: ## HEAD (no branch)
	#                1                2      3                4   5
	local regex='^## ([-a-zA-Z0-9/_]+)(\.\.\.([-a-zA-Z0-9/_]+)( \[([^\[]*)\]){0,1}){0,1}$'
	local detachedRe='^## HEAD[\t\ ]+\(no branch\)$'
	local marks aheadN behindN
	if [[ ${lines[0]} =~ $regex ]]; then
		lbranch=${BASH_REMATCH[1]}
		rbranch=${BASH_REMATCH[3]}
		local slippage=${BASH_REMATCH[5]}
		local aheadRe='ahead ([0-9]+)'
		local behindRe='behind ([0-9]+)'
		[[ "$slippage" =~ $aheadRe ]] && aheadN=${BASH_REMATCH[1]}
		[[ "$slippage" =~ $behindRe ]] && behindN=${BASH_REMATCH[1]}
		# check for common form where local branch name is same as remote branch
		# if so, shorten things
		local personalRe="^personal/([-a-zA-Z0-9_]+)/([-a-zA-Z0-9/_]+)$"
		if [[ $lbranch =~ $personalRe ]]; then
			if [[ "${BASH_REMATCH[1]}" == "${USER}" ]]; then
				lbranch="~/${BASH_REMATCH[2]}"
			else
				lbranch="~${BASH_REMATCH[1]}/${BASH_REMATCH[2]}"
			fi
		fi
		local remoteRe='^([-a-zA-Z0-9_]+)/([-a-zA-Z0-9/_]+){0,1}$'
		if [[ $rbranch =~ $remoteRe ]]; then
			local remote=${BASH_REMATCH[1]}
			local branch=${BASH_REMATCH[2]}
			if [[ $branch == $lbranch ]]; then
				rbranch="$remote"
			elif [[ $branch =~ $personalRe ]]; then
				if [[ "${BASH_REMATCH[1]}" == "${USER}" ]]; then
					rbranch="~/${BASH_REMATCH[2]}"
				else
					rbranch="~${BASH_REMATCH[1]}/${BASH_REMATCH[2]}"
				fi
			fi
		fi
	elif [[ ${lines[0]} =~ $detachedRe ]]; then
		lbranch="<detached HEAD>"
	else
		lbranch="<Parse Error>"
	fi
	regex='^ *M'
	[[ "${lines[1]}" =~ $regex ]] && marks+=" $branch_changed_symbol"

	[ -n "$lbranch" ] && lmarks+="$lbranch"
	[ -n "$aheadN" ] && lmarks+="$need_push_symbol$aheadN"
	[ -n "$rbranch" ] && rmarks+=" ➜ $rbranch"
	[ -n "$behindN" ] && rmarks+="$need_pull_symbol$behindN"

	printf "$1$lmarks$rmarks$marks$2"
}

# Build fancy, color PS1
# Heavily modified from bash-powerline
# colors: black 0  red 1  green 2  yellow 3 blue 4 magenta 5 cyan 6 white 7
#         add 8 for brighter colors
ps1() {
	local result=$?
	local bold="\[$(tput bold)\]"
	local reverse="\[$(tput rev)\]"
	local dim="\[$(tput dim)\]"
	local reset="\[$(tput sgr0)\]"
	local fg_base="\[$(tput setaf 15)\]"
	local st_fail="\[$(tput setaf 1)\]"
	local st_succeed="\[$(tput setaf 2)\]"
	local st_host="${fg_base}\[$(tput setab 5)\]"
	local st_git="${fg_base}\[$(tput setab 6)\]"
	local st_path="${fg_base}\[$(tput setab 4)\]"
	local st_exit="$st_succeed"

	# Check the exit code of the previous command and display different
	# colors in the prompt accordingly.
	if [ $result -ne 0 ]; then
		st_exit="$st_fail"
	fi
	local host=" ${USER}@${HOSTNAME} "
	local git=$(gprompt ' ' ' ')
	local path=" $(tilde ${PWD})"
	local cols=$(tput cols)
	local len=$(( ${#path}+${#host}+${#git}+2 ))
	if [ $len -gt $cols ]; then
		 host=''
		 len=$(( ${#path}+${#host}+${#git}+2 ))
	fi
	if [ $len -gt $cols ]; then
		 git=''
		 len=$(( ${#path}+${#host}+${#git}+2 ))
	fi
	if [ $len -gt $cols ]; then
		 path=$(truncm $path $(( $cols-2 )))
	fi
	PS1=""
	[ $(ccolumn) -gt 1 ] && PS1+="\n";
	PS1+="${st_exit}┏━${reset}"
	PS1+="${st_host}${host}${reset}"
	PS1+="${st_git}${git}${reset}"
	PS1+="${st_path}${path}${reset}"
	# This one has better alignment, but symbola
	# font on 14.04 does not have this glyph:
	# PS1+="\n${st_exit}┗━⯈${reset} "
	PS1+="\n${st_exit}┗━▶${reset} "
}

# Output BELL to show urgency hint where supported
urgency() {
	 echo -ne "\a"
}

simple_ps1() {
	 PS1="$(truncm $(tilde ${PWD}) 20)> "
}

# cd relative to current git repo
gcd() {
	local rpath=$1
	local rdir=$(git rev-parse --show-toplevel 2>/dev/null)
	[ -z $rdir ] || cd ${rdir}/${rpath}
}

# completion function for gcd
_gcd() {
	local rpath=$2
	local rdir=$(git rev-parse --show-toplevel 2>/dev/null)
	COMPREPLY=()
	if [ $rdir ]; then
		IFS=$'\n'
		rlen=0
		for dir in $( compgen -d -- "$rdir/$rpath" ); do
			sub=${dir#$rdir/}        # cut off directory
			COMPREPLY[rlen++]=$sub
		done
		IFS=$' \t\n'
		return 0
	fi
	return 1
}
complete -ofilenames -onospace -F _gcd gcd

# colors for less (mainly for man)
less_colors() {
	local colors=''
	colors+='LESS_TERMCAP_mb=$(tput setab 1) '
	colors+='LESS_TERMCAP_md=$(tput bold) '
	colors+='LESS_TERMCAP_me=$(tput sgr0) '
	colors+='LESS_TERMCAP_so=$(tput setab 9) '
	colors+='LESS_TERMCAP_se=$(tput sgr0) '
	colors+='LESS_TERMCAP_us=$(tput setaf 6) '
	colors+='LESS_TERMCAP_ue=$(tput sgr0)'
	echo $colors
}

if [ ${OSTYPE:0:5} == 'linux' ]; then
	# create ssh agent if needed and add private key identities
	export SSH_AGENT_PID=`pgrep -o -u $USER ssh-agent`
	if [ "$SSH_AGENT_PID" != '' ]; then
		export SSH_AUTH_SOCK="$(\ls $(find /tmp -type d -uid $(id -u) -name 'ssh-*' 2>/dev/null | head -n 1)/agent.*)"
		echo "using existing ssh-agent $SSH_AGENT_PID on $SSH_AUTH_SOCK"
	else
		eval `ssh-agent`
		echo "created new ssh-agent"
		ssh-add
	fi
fi

# Enable Bash completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
   . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
elif [ -d /usr/local/etc/bash_completion.d ]; then
	for file in /usr/local/etc/bash_completion.d/*; do
		. $file
	done
fi
# Also pick up completions in /usr/local/share
if [ -d /usr/local/share/bash-completion/completions ]; then
	for file in /usr/local/share/bash-completion/completions/*; do
		. $file
	done
fi


# Enable autojump
if [ -e /usr/share/autojump/autojump.sh ]; then
	. /usr/share/autojump/autojump.sh
elif [ -e /usr/local/etc/autojump.sh ]; then
	. /usr/local/etc/autojump.sh
fi


# be able to find our locally built libraries
[ -d ~/.local/lib ] && export LD_LIBRARY_PATH=~/.local/lib

# use vim as default editor and for command line editting style
export EDITOR=vim
set -o vi

# use less for paging
export PAGER=less

# bash history control
# ignore dups, append, increase size, put history in xdg dir
export HISTCONTROL=ignoreboth
export HISTSIZE=2000
export HISTTIMEFORMAT="%d%b_%H:%M "
shopt -s histappend

#### Store various files in XDG directories
# Anything that might be run from dash is exported in ~/.profile

# avoids ~/.bash_history
export HISTFILE=${XDG_CACHE_HOME}/bash/history
[ -d ${XDG_CACHE_HOME}/bash ] || mkdir -p ${XDG_CACHE_HOME}/bash
# avoids ~/.lesshst
export LESSHISTFILE=${XDG_CACHE_HOME}/less/history
[ -d ${XDG_CACHE_HOME}/less ] || mkdir -p ${XDG_CACHE_HOME}/less
# avoids ~/.inputrc
export INPUTRC=${XDG_CONFIG_HOME}/readline
# avoids ~/.screenrc
export SCREENRC=${XDG_CONFIG_HOME}/screen
# avoids ~/.python_history (pythonrc must have code for this though)
export PYTHONSTARTUP=${XDG_CONFIG_HOME}/pythonrc
# avoids ~/.ipython
export IPYTHONDIR=${XDG_CONFIG_HOME}/ipython
# avoids ~/.mplayer
export MPLAYER_HOME=${XDG_CONFIG_HOME}/mplayer

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# fix minor spelling errors
shopt -s cdspell

# prefixes used to iron certain platform differences
_PRE_=
[ ${OSTYPE:0:6} == 'darwin' ] && _PRE_="g"

# make less more friendly for non-text input files, see lesspipe(1)
which lesspipe > /dev/null && eval "$(SHELL=/bin/sh lesspipe)"
which lesspipe.sh > /dev/null && eval "$(SHELL=/bin/sh lesspipe.sh)"

# check for ECMA-48 terminal
if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
   ecma=1
fi

# enable color support of ls and also add handy aliases
if [ "$ecma" == "1" ]; then
	# define LS_COLORS using dircolors if it exists
	[ -x /usr/bin/dircolors ] && eval "`dircolors -b`"
	alias ls="${_PRE_}ls --color=auto"
	alias dir='${_PRE_}dir --color=auto'
	alias grep='grep --color=auto'
	alias fgrep='fgrep --color=auto'
	alias egrep='egrep --color=auto'
	alias man="$(less_colors) \man"
fi


case "$TERM" in
dumb*)
	PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND ;}simple_ps1"
	;;
xterm*|rxvt*)
# tell aosp not to mess with PROMPT_COMMAND
	STAY_OFF_MY_LAWN=1
# add fancy prompt, don't override PROMPT_COMMAND so autojump still works
	PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND ;}ps1;urgency"
	;;
*)
	;;
esac

##### Aliases & Bindings #####

alias cd..="cd .."
alias ls-x="ls -x"
alias ack='mark && ack-grep'
alias ag='mark && \ag'

##### OS Specific aliases #####

case "$OSTYPE" in
linux-gnuea*)  # raspbian
	alias ap='aptitude'
	;;
linux*)
	export DISPLAY=:0.0   # set display for X

	# Use openjdk 8 by default
	export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

	# simulate the osx commands:
	alias open=gnome-open
	alias pbcopy='xclip -selection clipboard'
	alias pbpaste='xclip -selection clipboard -o'

	# run commands in new window
	gterm() { urxvt -geometry 80x200-0-0 -title "$1 : $2 $3 $4" -e sh -c "$1 $2 $3 $4"; }
	gman() { gterm man $* & }
	ginfo() { gterm info $* & }
	gtail() { gterm less $* & }
	gmore()
	{
		if [[ $# == 0 ]]; then
			file=~/.cache/cat_$$_${RANDOM}
			cat > $file
			( gterm less $file && rm $file )&
		else
			gterm less $* &
		fi
	}
	alias gless=gmore
	say() { echo "(audio_mode 'async)(SayText \"$*\")" | festival --pipe; }
	alias vi='gvim -geometry 120x60-0 2>/dev/null'
	alias lyx='lyx -geometry 800x1075-15+0'
	;;

darwin*)  # OSX
	# emacs
	alias emacs='open -a Emacs.app'
	# open man page in preview
	gman() {
		t=`mktemp -t gman`
		man -t $* | pstopdf -i -o "$t"
		open -f -g -a Preview < "$t"
		rm "$t"
	}
	alias view='qlmanage -p $*> /dev/null'
	alias vi='mvim'

	# Brew installs GNU tools, and the system uses BSD tools. If the coreutils package
	# overwrote the common tools like 'ls' there would be two problems:
	#
	# 1. System tools understand file resources, GNU tools do not
	# 2. Scripts written to use BSD tools may unintentionally use GNU tools because
	#    the GNU tools are first in $PATH
	#
	# By default Macports/Brew prefixes the most common GNU tools with "g".  They can
	# be aliased to the regular name without cause problem #2, but #1 can still occur.
	# Put any such aliases here:
	;;

*)   # Everything else
	;;
esac
