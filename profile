
# Set environment vars for programs running under dash/sh.
# This is also sourced from ~/.bash_profile

# set XDG dirs to their defaults.  Not strictly necessary for cache, config and data, but
# set in case apps are poorly written.

[ "$XDG_CACHE_HOME" ] || export XDG_CACHE_HOME=${HOME}/.cache
[ "$XDG_CONFIG_HOME" ] || export XDG_CONFIG_HOME=${HOME}/.config
[ "$XDG_DATA_HOME" ] || export XDG_DATA_HOME=${HOME}/.local/share
[ -d $XDG_DATA_HOME ] || mkdir -p $XDG_DATA_HOME
[ -d $XDG_CACHE_HOME ] || mkdir -p $XDG_CACHE_HOME
[ -d $XDG_CONFIG_HOME ] || mkdir -p $XDG_CONFIG_HOME

# Override certain utilities and programs to use XDG directories
# These are only things that may be invoked via dash. 
# Overrides that for programs only run from bash shell
# (interactive or otherwise) are in bashrc.
export GNOME22_USER_DIR=${XDG_CONFIG_HOME}/gnome2.2
export GTK2_RC_FILES="${XDG_CONFIG_HOME}/gtk-2.0/gtkrc"
export GIMP2_DIRECTORY=${XDG_CONFIG_HOME}/gimp2
export GTAGSCONF=${XDG_CONFIG_HOME}/globalrc
export GTAGS_OPTIONS=--skip-unreadable
export GNUPGHOME=${XDG_CONFIG_HOME}/gnupg
export __GL_SHADER_DISK_CACHE_PATH=${XDG_CACHE_HOME}/nv
export CUDA_CACHE_PATH=${XDG_CACHE_HOME}/nv 
[ -d ${XDG_CACHE_HOME}/nv ] || mkdir -p ${XDG_CACHE_HOME}/nv
export ICEAUTHORITY=${XDG_CACHE_HOME}/ICEauthority
# this doesn't work under mint, suspect mdm is hard coding it
# it also doesn't work under Ubuntu 15.10, probably lightdm is hard coding it
#export XAUTHORITY="${XDG_CACHE_HOME}/Xauthority"

# This disables startup messages about inability to connect to accessibility bus
export NO_AT_BRIDGE=1
