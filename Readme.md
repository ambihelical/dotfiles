This is my repository used to set up new Linux home directories. This includes
"dot" files, but also a bit more.  This is not intended to be used by anyone
else but me, but it might be a useful starting place for someone wanting
something similar for themselves.  It is also a work in progress.

I like to keep all my git repositories together in a certain directory, so
rather than setting up a git repository containing the home directory dot files
directly, this repository can be elsewhere.  A Makefile is used to make sure
that the appropriate dot files in the home directory are symbolically linked to
the appropriate files in the repository. 

Git submodules are used for vim plugins. Ideally, you cloned this repository like this:

    git clone --recursive https::/github.com/ambihelical/homerc

If you didn''t use --recursive with the git clone command, you need to do the following in the 
this repository:

    git submodule init
    git submodule update

To refresh the plugins with the latest, just use:

	git submodule update

There is a Makefile that drives everything. Type `make` to get help on the available targets. 

