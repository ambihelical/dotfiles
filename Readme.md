Introduction
------------

This is my repository used to set up new home directories. This includes "dot"
files, but also some useful configuration.  This is not intended to be used by
anyone else but me, but it might be a useful starting place for someone wanting
something similar for themselves.  It is also a work in progress.

There is a Makefile that drives everything. Type `make` to get help on the
available targets.  There are also separate scripts in the directory scripts
for installing packages. These can take some time to run, so they are separate.
In theory these can be run at any time, but it is possible that a package
installation will overwrite a configuration file, so it is best to run these
scripts before running make.

A note about submodules
-----------------------

Git submodules are used for vim plugins. Ideally, you cloned this repository like this:

    git clone --recursive https::/github.com/ambihelical/homerc

If you didn''t use --recursive with the git clone command, you need to do the following in the 
this repository:

    git submodule init
    git submodule update

To refresh the plugins with the latest, just use:

	git submodule update


