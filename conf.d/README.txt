---+ ClioPatria configuration directory

The main configuration file is run.pl  in the toplevel directory. Run.pl
loads all *.pl files in  the   directory  =|conf.d|=.  This directory is
searched for in two  places:  the   current  working  directory  and the
toplevel directory of ClioPatria. The one in ClioPatria is loaded first,
allowing for local overrides.

Config files are named  *.pl  and   loaded  in  alphabetical order. Only
readable file are loaded: files  that   cannot  be  opended are silently
ignored.

The system example files are located   in examples/conf.d. Some of these
files are merely options that can be selected while others are skeletons
that    can    be    copied    and      edited.    The    overview    in
../examples/conf.d/README.txt summarises the purpose and  whether or not
edit  is  always  required.   On    systems   that  support  file-links,
configuration files that need not be edited   can be included by linking
them. For example, to include  the   development  tools, use the command
below in this directory.

    ==
    ln -s ../examples/conf.d/debug.pl
    ==

---++ Configuration and upgrading ClioPatria

The motivation for this  scheme  is  similar   to  that  of  the  Apache
web-server project. Apache started with  a large monolitic configuration
file, which made merging local  configuration   into  an upgraded Apache
release quite a challenge. Modern  Apache   configurations  have a small
overall config file and load many   small config files from directories.
This simplifies the process of merging changes considerably.
