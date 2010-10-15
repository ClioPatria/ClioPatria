---+ Installation and Administration

Basic installation merely  requires  installing   a  recent  version  of
[[SWI-Prolog][http://www.swi-prolog.org]] and unpacking  ClioPatria (see
_Downloading_ below).  Next, you can deploy it in two ways:

---+++ Installation as application

Running ClioPatria as an application requires a few steps:

    1. Create the start-file run.pl from run.pl.in.  On Unix, this
    is achieved by running =|./configure|= from the top directory.  On
    Windows by double-clicking setup.pl.

    2. Optionally, pull in configuration files according to
    conf.d/README.txt.  This can also be done later.

    3. Start run.pl as =|./run.pl|= on Unix or double-clicking run.pl
    on Windows and direct your browser to the indicated address (default
    is http://localhost:3020/.  If this is the first time, you will be
    asked to enter an _admin_ password. This protects some
    web-operations, such as managing settings and loading/unloading RDF.

If you want to extend the application, there  are two options. One is to
put a Prolog file with the desired  extensions in =|conf.d|=. Another is
to run configure/setup from another directory.   This creates run.pl and
conf.d in the current working directory,  after which the extensions can
be placed in =|conf.d|=.


---+++ Use as library

To use ClioPatria as a library,   one  can simply compile cliopatria.pl.
The file run.pl as generated from   installation as an application gives
additional actions you may want to setup.   The web-server is started by
cp_server/0, but the system runs happily if   the  Prolog HTTP server is
started in another way, as long as   the server uses http_dispatch/1 for
dispatching HTTP requests.
