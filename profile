
# Set environment vars for programs running under dash/sh.
# This is also sourced from ~/.bash_profile

# set XDG dirs to their defaults.  Not strictly necessary for cache, config and data, but
# set in case apps are poorly written.

[ "$XDG_CACHE_HOME" ] || export XDG_CACHE_HOME=${HOME}/.cache
[ "$XDG_CONFIG_HOME" ] || export XDG_CONFIG_HOME=${HOME}/.config
[ "$XDG_DATA_HOME" ] || export XDG_DATA_HOME=${HOME}/.local/share
[ ! -d $XDG_DATA_HOME ] && mkdir -p $XDG_DATA_HOME
[ ! -d $XDG_CACHE_HOME ] && mkdir -p $XDG_CACHE_HOME
[ ! -d $XDG_CONFIG_HOME ] && mkdir -p $XDG_CONFIG_HOME

# Override certain utilities and programs to use XDG directories
export GNOME22_USER_DIR=${XDG_CONFIG_HOME}/gnome2.2
export GIMP2_DIRECTORY=${XDG_CONFIG_HOME}/gimp2
export GTAGSCONF=${XDG_CONFIG_HOME}/globalrc
export IPYTHONDIR=${XDG_CONFIG_HOME}/ipython
export GNUPGHOME=${XDG_CONFIG_HOME}/gnupg
