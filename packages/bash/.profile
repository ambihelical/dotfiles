
# Set environment vars for programs running under dash/sh.
# This is also sourced from ~/.bash_profile


# if /data/cache exists, use it for XDG_CACHE_HOME
# Generally this will some kind of fast drive at /data
[ -d /data/cache ] && export XDG_CACHE_HOME=/data/cache

# set XDG dirs to their defaults.  Not strictly necessary for cache, config, state and data, but
# set in case apps are poorly written.
[ "$XDG_CACHE_HOME" ] || export XDG_CACHE_HOME=${HOME}/.cache
[ "$XDG_CONFIG_HOME" ] || export XDG_CONFIG_HOME=${HOME}/.config
[ "$XDG_DATA_HOME" ] || export XDG_DATA_HOME=${HOME}/.local/share
[ "$XDG_STATE_HOME" ] || export XDG_STATE_HOME=${HOME}/.local/state
[ -d $XDG_DATA_HOME ] || mkdir -p $XDG_DATA_HOME
[ -d $XDG_CACHE_HOME ] || mkdir -p $XDG_CACHE_HOME
[ -d $XDG_CONFIG_HOME ] || mkdir -p $XDG_CONFIG_HOME
[ -d $XDG_STATE_HOME ] || mkdir -p $XDG_STATE_HOME


# load site settings
[ -e ${HOME}/.local/share/site-profile ] && source ${HOME}/.local/share/site-profile

# Override certain utilities and programs to use XDG directories
# Some of these are invoked only via dash, others are invoked
# by gnome, via .desktop files.  These also circumvent ~/.bashrc
# Overrides that for programs only run from bash shell
# (interactive or otherwise) are in bashrc.

# avoid ~/.gnome2
# (something keeps recreating this dir, but it is empty)
export GNOME22_USER_DIR=${XDG_CONFIG_HOME}/gnome2.2

# avoid ~/.gtkrc-2.0
export GTK2_RC_FILES="${XDG_CONFIG_HOME}/gtk-2.0/gtkrc"

# avoid ~/.gimp-2.8 (or later)
export GIMP2_DIRECTORY=${XDG_CONFIG_HOME}/gimp2

# avoid ~/.globalrc, skip unreadables files by default
export GTAGSCONF=${XDG_CONFIG_HOME}/globalrc
export GTAGS_OPTIONS=--skip-unreadable

# avoid ~/.gnupg
export GNUPGHOME=${XDG_DATA_HOME}/gnupg

# avoid ~/.nv
export __GL_SHADER_DISK_CACHE_PATH=${XDG_CACHE_HOME}/nv
export CUDA_CACHE_PATH=${XDG_CACHE_HOME}/nv 
[ -d ${XDG_CACHE_HOME}/nv ] || mkdir -p ${XDG_CACHE_HOME}/nv

# avoid ~/.ICEauthority
export ICEAUTHORITY=${XDG_CACHE_HOME}/ICEauthority

# avoid ~/.ccache, and also set max size
# can override this with ccache -M ##G
if [ "${XDG_CACHE_HOME}" = "/data/cache" ]; then
   export CCACHE_MAXSIZE=75G
else
   export CCACHE_MAXSIZE=25G
fi
export CCACHE_DIR=${XDG_CACHE_HOME}/ccache
[ -d ${CCACHE_DIR} ] || mkdir -p ${CCACHE_DIR}

# avoids ~/.android
export ANDROID_SDK_HOME=${XDG_DATA_HOME}/android
[ -d ${ANDROID_SDK_HOME} ] || mkdir -p ${ANDROID_SDK_HOME}
[ -d ${ANDROID_SDK_HOME}/.android ] || mkdir -p ${ANDROID_SDK_HOME}/.android

# avoids ~/.gradle
export GRADLE_USER_HOME=${XDG_CACHE_HOME}/gradle

# avoids ~/aws/config
export AWS_CONFIG_FILE=${XDG_CONFIG_HOME}/aws/config

# this doesn't work under mint, suspect mdm is hard coding it
# it also doesn't work under Ubuntu 15.10, probably lightdm is hard coding it
#export XAUTHORITY="${XDG_CACHE_HOME}/Xauthority"

# This disables startup messages about inability to connect to accessibility bus
export NO_AT_BRIDGE=1

# Default man path includes both /usr/local/man as well as /usr/local/share/man
# which the former is a symlink to.  This leads to duplication of man pages in
# e.g. emacs woman, so we will overide all of that:
export MANPATH=/usr/local/share/man:/usr/share/man

# avoids ~/.subversion
export SUBVERSION_HOME=${XDG_CONFIG_HOME}/subversion

# avoids ~/.pip
export PIP_CONFIG_FILE=${XDG_CONFIG_HOME}/pip/config
export PIP_LOG_FILE=${XDG_CACHE_HOME}/pip/log
[ -d ${XDG_CACHE_HOME}/pip/http ] || mkdir -p ${XDG_CACHE_HOME}/pip/http

# avoids ~/.gem
export GEM_HOME=${XDG_DATA_HOME}/gem
export GEM_SPEC_CACHE=${XDG_CACHE_HOME}/gem

# avoids ~/.screenrc
export SCREENRC=${XDG_CONFIG_HOME}/screen

# avoid ~/.cargo and ~/.rustup
export RUSTUP_HOME=${XDG_DATA_HOME}/rustup
export CARGO_HOME=${XDG_DATA_HOME}/cargo

# put go dir in ~/dev/go
export GOPATH=${HOME}/dev/go

# bash history
export HISTFILE=${XDG_STATE_HOME}/bash/history
[ -d ${XDG_CACHE_HOME}/bash ] || mkdir -p ${XDG_CACHE_HOME}/bash
[ -d ${XDG_STATE_HOME}/bash ] || mkdir -p ${XDG_STATE_HOME}/bash

# vimrcs
export GVIMINIT='let $MYGVIMRC="${XDG_CONFIG_HOME}/vim/gvimrc" | source $MYGVIMRC'
export VIMINIT='let $MYVIMRC="${XDG_CONFIG_HOME}/vim/vimrc" | source $MYVIMRC'

# npm
export NPM_CONFIG_USERCONFIG=${XDG_CONFIG_HOME}/npm/npmrc
[ -d ${XDG_CACHE_HOME}/npm ] || mkdir -p ${XDG_CACHE_HOME}/npm
[ -d ${XDG_DATA_HOME}/npm ] || mkdir -p ${XDG_DATA_HOME}/npm