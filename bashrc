#!/bin/bash
# Keep above line to force syntax highlighting

# include system bashrc if it exists
[ -e /etc/bashrc ] && source /etc/bashrc

# include sh/dash profile
[ -e ${HOME}/.profile ] && source ${HOME}/.profile

# include local settings

[ -e ${HOME}/.local/share/site-bashrc ] && source ${HOME}/.local/share/site-bashrc

# setup path. These are in reverse order of how they appear in the PATH value.
[ -d ~/extern/gccarm-dev/bin ] && PATH=~/extern/gccarm-dev/bin:"${PATH}"    # arm gcc cross compiler
[ -d /usr/lib/ccache ] && PATH=/usr/lib/ccache:"${PATH}"                    # compile cache
[ -d ${XDG_DATA_HOME}/npm/bin ] && PATH=${XDG_DATA_HOME}/npm/bin:"${PATH}"  # npm stuff
[ -d ~/dev/go/bin ] && PATH=~/dev/go/bin:"${PATH}"                          # go stuff
[ -d ~/bin/${OSTYPE} ] && PATH=~/bin/${OSTYPE}:"${PATH}"                    # my utilities, os specific
[ -d ~/Android/Sdk/platform-tools ] && PATH=~/Android/Sdk/platform-tools:"${PATH}"         # adb, fastboot, etc
[ -d ~/extern/ChibiOS-RT ] && export CHIBIOS=~/extern/ChibiOS-RT            # chibios development
# We will need the following paths eventually, so make them now
[ -d ${GEM_HOME}/ruby/3.0.0/bin ] || mkdir -p ${GEM_HOME}/ruby/3.0.0/bin    # ruby gems
[ -d ~/.local/bin ] || mkdir -p ~/.local/bin                                # locally installed execs
[ -d ${XDG_DATA_HOME}/cargo/bin/ ] || mkdir -p ${XDG_DATA_HOME}/cargo/bin   # rust cargo
[ -d ~/bin ] || mkdir -p ~/bin                                              # my utilities
PATH=${XDG_DATA_HOME}/cargo/bin/:~/.local/bin:~/bin:${GEM_HOME}/ruby/3.0.0/bin:"${PATH}"

export ACKRC=${XDG_CONFIG_HOME}/ack
export RIPGREP_CONFIG_PATH=${XDG_CONFIG_HOME}/ripgrep/config

# If not running interactively, don't do anything more
[ -z "$PS1" ] && return

################ Interactive Portion ###############

if [ ${OSTYPE:0:5} == 'linux' ]; then
    # create google drive mount point if it doesn't exist
    [ -d ~/GDrive ] || mkdir -p ~/GDrive      # google drive
    [ -e ~/GDrive ] && chmod go-rwx ~/GDrive
    # this should exist for all distros
	. /etc/os-release
	if [ "$ID" == "ubuntu" ]; then
		if [ -e ${XDG_CONFIG_HOME}/environment.d/ssh-agent.conf ]; then
			source ${XDG_CONFIG_HOME}/environment.d/ssh-agent.conf
			export SSH_AUTH_SOCK
		else
			# create ssh agent if needed and add private key identities
			export SSH_AGENT_PID=`pgrep -o -u $USER ssh-agent`
			if [ "$SSH_AGENT_PID" != '' ]; then
				export SSH_AUTH_SOCK="$(\ls $(find /tmp -type d -uid $(id -u) -name 'ssh-*' 2>/dev/null | head -n 1)/agent.*)"
				echo "using existing ssh-agent $SSH_AGENT_PID on $SSH_AUTH_SOCK"
			else
				eval `ssh-agent`
				echo "created new ssh-agent, use ssh-add to setup with keys"
				ssh-add
			fi
		fi
	elif [ "$ID" == "fedora" ]; then
		# I think this is good, time will tell
		if [ "$(pgrep -o -u $USER ssh-agent)" == '' ]; then
			eval `ssh-agent`
			echo "created new ssh-agent, use ssh-add to setup with keys"
		fi
	fi
fi

# make tramp work better hopefully
[[ "${TERM}" == dumb ]] && PS1='$ ' && return

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

# find files

ff() {
	op="name"
	[ "$1" = "-i" ] && op="iname" && shift
	name=$1
	shift
	pat='*'
	pat+=${name}
	pat+='*'
	find . -${op} "$pat" $@
}

# output current column
ccolumn()
{
	 local COL
	 local ROW
	 IFS=';' read -sdR -p $'\E[6n' ROW COL
	 echo "${COL}"
}

# Output BELL to show urgency hint where supported
# also change title when PWD changes
urgency() {
	if [ "$PREVPWD" != "$PWD" ]; then
		local pwd=$(tilde $PWD)
		local conn=""
		if [  "$SSH_CONNECTION" != "" ]; then
			conn="[${USER}@$(hostname)] "
		fi
		xtitle "${conn}${pwd}"
		export PREVPWD="$PWD"
	fi
	echo -ne "\a"
}

# fancy ps1 w/o slow features like git
# colors: black 0  red 1  green 2  yellow 3 blue 4 magenta 5 cyan 6 white 7
#         add 8 for brighter colors
medium_ps1() {
	local result=$?
	local bold="\[$(tput bold)\]"
	local reverse="\[$(tput rev)\]"
	local dim="\[$(tput dim)\]"
	local reset="\[$(tput sgr0)\]"
	local fg_base="\[$(tput setaf 15)\]"
	local st_fail="\[$(tput setaf 1)\]"
	local st_succeed="\[$(tput setaf 2)\]"
	local st_host="${fg_base}\[$(tput setab 5)\]"
	local st_path="${fg_base}\[$(tput setab 4)\]"
	local st_exit="$st_succeed"
	local st_ssh=""
	if [  "$SSH_CONNECTION" != "" ]; then
		st_ssh=$reverse
	fi

	# Check the exit code of the previous command and display different
	# colors in the prompt accordingly.
	if [ $result -ne 0 ]; then
		st_exit="$st_fail"
	fi
	local host=" ${USER}@${HOSTNAME} "
	local path=" $(tilde ${PWD})"
	local cols=$(tput cols)
	local len=$(( ${#path}+${#host}+${#git}+2 ))
	if [ $len -gt $cols ]; then
		 host=''
		 len=$(( ${#path}+${#host}+${#git}+2 ))
	fi
	if [ $len -gt $cols ]; then
		 path=$(truncm $path $(( $cols-2 )))
	fi
	PS1=""
	[ $(ccolumn) -gt 1 ] && PS1+="\n";
	PS1+="${st_exit}┏━${reset}"
	PS1+="${st_ssh}${st_host}${host}${reset}"
	PS1+="${st_path}${path}${reset}"
	# This one has better alignment, but symbola
	# font on 14.04 does not have this glyph:
	# PS1+="\n${st_exit}┗━⯈${reset} "
	PS1+="\n${st_exit}┗━▶${reset} "
}

simple_ps1() {
	 PS1="$(truncm $(tilde ${PWD}) 20)> "
}

xtitle() {
    echo -e "\033]1;$@\007\033]2;$@\007\c"
}

set_title() {
	local pwd=$(tilde $PWD)
	local wat=$(history|tail -1|awk '{s=""; for (i=3;i<=NF;i++) s = s $i " "; print s}')
	local conn=""
	if [  "$SSH_CONNECTION" != "" ]; then
		conn="[${USER}@$(hostname)] "
	fi
	xtitle "${conn}${pwd} ${wat}"
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
# also pickup any local completions in ${XDG_DATA_HOME}/bash-completion/completions/
# N.B. This will be done automatically by bash-completion v2.8, but this should
# do no harm until distros are updated.
if [ -d ${XDG_DATA_HOME}/bash-completion/completions ]; then
	for file in ${XDG_DATA_HOME}/bash-completion/completions/*; do
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

# use emacs in client mode as default editor
# use vim as default editor and for command line editting style

EMACS_FLAGS=
if [  "$SSH_CONNECTION" != "" ]; then
	EMACS_FLAGS="-nw"
	EMACS_CLIENT_FLAGS="-t"
fi
alias emacs="\emacs ${EMACS_FLAGS} "

export EDITOR=vim
if type emacsclient > /dev/null 2>&1;  then
    export ALTERNATE_EDITOR=""
    export EDITOR="emacsclient -t --alternate-editor="
    export VISUAL="emacsclient -c ${EMACS_CLIENT_FLAGS} --alternate-editor="
elif type gvim > /dev/null 2>&1 ; then
     export VISUAL=gvim
fi

# use vi keybinding for readline
set -o vi

# use less for paging
export PAGER=less

# bash history control
# ignore dups, increase size, timestamp, append
export HISTCONTROL=ignoreboth
export HISTSIZE=2000
export HISTTIMEFORMAT="%d-%b-%H:%M "  # DOM-MON-HOUR:MIN
shopt -s histappend

#### Store various files in XDG directories
# Anything that might be run from dash is exported in ~/.profile

# avoids ~/.lesshst
export LESSHISTFILE=${XDG_CACHE_HOME}/less/history
[ -d ${XDG_CACHE_HOME}/less ] || mkdir -p ${XDG_CACHE_HOME}/less
# avoids ~/.inputrc
export INPUTRC=${XDG_CONFIG_HOME}/readline
# avoids ~/.python_history (pythonrc must have code for this though)
export PYTHONSTARTUP=${XDG_CONFIG_HOME}/pythonrc
# avoids ~/.ipython
export IPYTHONDIR=${XDG_CONFIG_HOME}/ipython
# avoids ~/.mplayer
export MPLAYER_HOME=${XDG_CONFIG_HOME}/mplayer
# setup sscache for rust if installed
command -v sccache > /dev/null && export RUSTC_WRAPPER=sccache
# enable rust backtraces
export RUST_BACKTRACE=1

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# fix minor spelling errors
shopt -s cdspell

# prefixes used to iron certain platform differences
_PRE_=
[ ${OSTYPE:0:6} == 'darwin' ] && _PRE_="g"

# make less more friendly for non-text input files, see lesspipe(1)
which lesspipe >& /dev/null && eval "$(SHELL=/bin/sh lesspipe)"
which lesspipe.sh >& /dev/null && eval "$(SHELL=/bin/sh lesspipe.sh)"

# check for ECMA-48 terminal
if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
   ecma=1
fi

# enable color support of ls and also add handy aliases
if [ "$ecma" == "1" ]; then
	# define LS_COLORS using dircolors if it exists
	if [ -x /usr/bin/dircolors ]; then
		eval "`dircolors -b`"
		export LS_COLORS+=':ow=01;33'   # make other-writable dirs readable
	fi
	alias ls="${_PRE_}ls --color=auto"
	alias dir='${_PRE_}dir --color=auto'
	alias grep='grep --color=auto'
	alias fgrep='fgrep --color=auto'
	alias egrep='egrep --color=auto'
	alias man="$(less_colors) \man"
fi


STAY_OFF_MY_LAWN=1   # tell aosp not to mess with PROMPT_COMMAND

if [[ "$TERM" != "dumb" ]]; then
    if type starship > /dev/null 2>&1;  then
        eval "$(starship init bash)"
        starship_precmd_user_func="set_title"
    else
        # set prompt command. First see if it has already been set in case
        # we have sourced ~/.bashrc again, in which case it is better to
        # leave it alone.  medium_ps1 is first so it can pick up the last
        # command's status.
        if [[ $PROMPT_COMMAND =~ ";urgency" ]]; then
            echo "PROMPT_COMMAND appears to already be set up"
        else
            PROMPT_COMMAND="medium_ps1;${PROMPT_COMMAND:+$PROMPT_COMMAND ;}urgency"
        fi
        PS0='$(set_title)'
    fi
fi


##### Aliases & Bindings #####

alias cd..="cd .."
alias ls-x="ls -x"
alias weather="curl -s wttr.in"

# TODO: set this up with proper credentials to automount, see
# https://github.com/astrada/google-drive-ocamlfuse/wiki/Headless-Usage-&-Authorization
if type google-drive-ocamlfuse > /dev/null 2>&1;  then
alias gdfuse="google-drive-ocamlfuse -xdgbd ~/GDrive"
fi

if type mark > /dev/null 2>&1; then
    alias ag='mark && \ag'
    alias rg='mark && \rg'
    alias gg='mark && git grep'
fi

##### OS Specific aliases #####

case "$OSTYPE" in
linux-gnuea*)  # raspbian
	alias ap='aptitude'
	;;
linux*)

	# if DISPLAY isn't set common value
	[ "$DISPLAY" ] || export DISPLAY=:0.0

	# Use openjdk 8 by default
	export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

	# open files via xdg-open
	open() {
		for file in "$@"; do
			xdg-open "$file" &
		done
	}

	alias pbcopy='xclip -selection clipboard'
	alias pbpaste='xclip -selection clipboard -o'

	# run commands in new window
	gterm() { xterm -geometry 80x200-0-0 -title "$1 : $2 $3 $4" -e sh -c "$1 $2 $3 $4"; }
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
	# make desktop control centers work better for i3, etc
	if [ "$(which gnome-control-center)" != "" ]; then
		alias xdg-control-center='XDG_CURRENT_DESKTOP=GNOME gnome-control-center'
	fi
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
