# vim:ft=zsh

#umask 007   # disallow others access

case "$OSTYPE" in
linux*)
    ;;
darwin*)
  export PATH=/opt/local/bin:/opt/local/sbin:$PATH
  export MANPATH=/opt/local/man:/opt/local/share/man:$MANPATH
    ;;
*)
    ;;
esac

# set XDG dirs to their defaults.  Not strictly necessary for cache, config and data, but 
# set in case apps are poorly written.
export XDG_CACHE_HOME=~/.cache
export XDG_CONFIG_HOME=~/.config
export XDG_DATA_HOME=~/.local/share
[ ! -d $XDG_DATA_HOME ] && mkdir -p $XDG_DATA_HOME
[ ! -d $XDG_CACHE_HOME ] && mkdir -p $XDG_CACHE_HOME
[ ! -d $XDG_CONFIG_HOME ] && mkdir -p $XDG_CONFIG_HOME

# locations of things
[ -d ~/extern/ChibiOS-RT ] && export CHIBIOS=~/extern/ChibiOS-RT            # chibios development

# setup path. These are in reverse order of how they appear in the PATH value. 

[ -d ~/extern/gccarm-dev/bin ] && PATH=~/extern/gccarm-dev/bin:"${PATH}"    # arm gcc cross compiler
[ -d /usr/lib/ccache ] && PATH=/usr/lib/ccache:"${PATH}"                    # compile cache
[ -d ~/local/bin ] && PATH=~/local/bin:"${PATH}"                            # locally installed execs
[ -d ~/bin ] && PATH=~/bin:"${PATH}"                                        # my utilities
[ -d ~/bin/${OSTYPE} ] && PATH=~/bin/${OSTYPE}:"${PATH}"                    # my utilities, os specific

export ACKRC=${XDG_CONFIG_HOME}/ack/config

# If not running interactively, don't do anything more
[ -z "$PS1" ] && return

################ Interactive Portion ###############

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

[ -e /usr/share/autojump/autojump.sh ] && . /usr/share/autojump/autojump.sh

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
	echo ${1/${HOME}/\~}
}

# return git root directory
gprompt() {
	local out=
	local branch=$(__git_ps1 "%s")
	# shorten certain personal branch conventions
	for pat in "personal/${USER}" "personal/"; do
		if [[ "${branch}" =~ ^${pat} ]]; then
			branch=${branch/${pat}/\~}
			break
		fi
	done
	if [[ -n "${branch}" ]]; then
		local rdir=$(git rev-parse --show-toplevel 2>/dev/null)
		local rbase=${rdir##*/}
		local tbase=$(truncm "$rbase" 8)
		local tbranch=$(truncm "$branch" 15)
		out=":${tbase}@${tbranch}"
	fi
	echo $out
}

# be able to find our locally built libraries
[ -d ~/local/lib ] && export LD_LIBRARY_PATH=~/local/lib

# use vim as default editor and for command line editting style
export EDITOR=vim
set -o vi

# bash history control
# ignore dups, append, increase size, put history in xdg dir
export HISTCONTROL=ignoreboth
export HISTSIZE=2000
shopt -s histappend
export HISTFILE=${XDG_CACHE_HOME}/bash/history
[ ! -d ${XDG_CACHE_HOME}/bash ] && mkdir -p ${XDG_CACHE_HOME}/bash

# store less history in xdg dir
export LESSHISTFILE=${XDG_CACHE_HOME}/less/history
[ ! -d ${XDG_CACHE_HOME}/less ] && mkdir -p ${XDG_CACHE_HOME}/less

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# fix minor spelling errors
shopt -s cdspell 

# prefixes used to iron certain platform differences
case "$OSTYPE" in
  linux*)
    _PRE_=
  ;;
  darwin*)
    _PRE_="g"
  ;;
esac

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
fi

# Enable Bash completion 
if [ -f /usr/share/bash-completion/bash_completion ]; then
   . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
elif [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

# primary prompt. Color only when ECMA-48 capable terminal
PS1='\[\033[01;32m\]$(truncm \u)@$(truncm \h)\[\033[00m\]$(gprompt):\[\033[01;34m\]$(truncm $(tilde \w) 20)\[\033[00m\]\$ '
[ "$ecma" != "1" ] && PS1='\u@\h$(gprompt):\w\$ ' && PROMPT_DIRTRIM=2

case "$TERM" in
xterm*|rxvt*)
   # set window title
	PROMPT_COMMAND=$PROMPT_COMMAND';echo -ne "\033]0; [${HOSTNAME}] $(tilde $PWD)\007"'
    ;;
*)
    ;;
esac

##### Aliases & Bindings #####

alias m="make -k"
alias cd..="cd .."
alias ls-x="ls -x"
alias ack='mark && ack-grep'

bind Space:magic-space      # expand !$,!^,!*,!!,etc on spacebar

##### OS Specific aliases #####

case "$OSTYPE" in
linux-gnuea*)  # raspbian
    alias ap='aptitude'
    ;;
linux*) 
    export DISPLAY=:0.0   # set display for X
    alias open=gnome-open
    gterm() { gnome-terminal --window-with-profile=man-small --geometry 80x200-0-0 --title="$1: $2" --command "$1 $2"; }
    gman() { gterm man $1 & }
    ginfo() { gterm info $1 & }
    gtail() { gterm less $1 & }
    # Run commands in a new, temporary terminal window
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
    say() { echo "(audio_mode 'async)(SayText \"$*\")" | festival --pipe; }
    alias vi='gvim -geometry 120x60-0 2>/dev/null'
    alias lyx='lyx -geometry 800x1075-15+0'
    alias ap='aptitude'
    alias gless=gmore
    # allow core dumps
    #ulimit -c unlimited
    ;;

darwin*)  # OSX
    # use bwana plugin to open man page in browser
    hman() { open -a Safari man:$*; }
    # use preview (ps2pdf keeps preview from having a file to save)
    # gman() { man -t $* | ps2pdf - - | open -g -f -a Preview; }
    # this version works oob on osx 
    gman() { 
      t=`mktemp -t gman`
      man -t $* | pstopdf -i -o "$t"
      open -f -g -a Preview < "$t"
      rm "$t"
    }
    alias view='qlmanage -p $*> /dev/null'
    alias vi='mvim'
    # Macports uses GNU tools, and the system uses BSD tools. If the coreutils package
    # overwrote the common tools like 'ls' there would be two problems:
    #
    # 1. System tools understand file resources, GNU tools do not
    # 2. Scripts written to use BSD tools may unintentionally use GNU tools because 
    #    the GNU tools are first in $PATH
    # 
    # By default Macports prefixes the most common GNU tools with "g".  They can 
    # be aliased to the regular name without cause problem #2, but #1 can still occur. 
    # Put any such aliases here:
    ;;
*)   # Everything else
    ;;
esac

