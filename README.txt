---+ The ClioPatria Semantic Web Toolkit

---++ About

ClioPatria is an extension of the SWI-Prolog RDF infratructure (`semweb'
package) that provides you with a   ready-to-run  web-server that can be
extended into a  full-fledged  Semantic   Web  application.  The  semweb
package provides reading and writing RDF   (XML and Turtle), storage and
querying by means of rdf(Subject,   Predicate,  Object). ClioPatria adds
the following:

    $ A SPARQL server :
    This processes HTTP SPARQL requests.  The server also includes
    support for SeRQL and the Sesame (www.openrdf.org) HTTP protocol.

    $ Reasoning libraries :
    Called entailment modules.  See entailment/README.txt

    $ User administration :
    Create users, provide OpenID services, use external OpenID
    authorization and connect users to right-tokens.

    $ A web-based developers front-end :
    This provides provides browsing the RDF, loading and unloading
    graphs, testing queries interactively, browsing the documentation of
    HTTP services and source-code.

    $ Web-page generation components :
    Server-side components to render an RDF resource or literals with
    a link to the development UI, render simple graphs, etc.

    $ Additional libraries :
    These are additional components to the http and semweb libraries
    that may become part of SWI-Prolog in the future.  Examples are
    lib/semweb/rdf_optimise.pl to optimise rdf-control-structures and
    lib/semweb/rdf_abstract.pl to transform graphs represented as
    rdf(S,P,O) terms.

---++ Installation

Basic installation merely  requires  installing   a  recent  version  of
[[SWI-Prolog][http://www.swi-prolog.org]] and unpacking  ClioPatria (see
_Downloading_ below).  Next, you can deploy it in two ways:

---+++ Installation as application

Running ClioPatria as an application requires a few steps:

    1. Create the start-file run.pl from run.pl.in.  On Unix, this
    is achieved by running =|./configure|= from the top directory.  On
    Windows by double-clicking setup.pl.

    2. Optionally, pull in configuration files according to
    config-enabled/README.txt.  This can also be done later.

    3. Start run.pl as =|./run.pl|= on Unix or double-clicking run.pl
    on Windows and direct your browser to the indicated address (default
    is http://localhost:3020/.  If this is the first time, you will be
    asked to enter an _admin_ password. This protects some
    web-operations, such as managing settings and loading/unloading RDF.

If you want to extend the application, there  are two options. One is to
put a Prolog file with  the   desired  extensions in =|config-enabled|=.
Another is to run configure/setup from   another directory. This creates
run.pl and config-enabled in the current  working directory, after which
the extensions can be placed in =|config-enabled|=.


---+++ Use as library

To use ClioPatria as a library,   one  can simply compile cliopatria.pl.
The file run.pl as generated from   installation as an application gives
additional actions you may want to setup.   The web-server is started by
cp_server/0, but the system runs happily if   the  Prolog HTTP server is
started in another way, as long as   the server uses http_dispatch/1 for
dispatching HTTP requests.


---++ Downloading

Currently,    ClioPatria    can    only      be    downloaded    through
[[GIT][http://www.git-scm.com]]. The command for  downloading the system
is:

    ==
    git clone git://www.swi-prolog.org/home/pl/git/ClioPatria.git
    ==

---++ Further reading

Many of the design issues around handling the Semantic Web in Prolog are
described and motivated in Jan Wielemaker's PhD thesis
[[(PDF)][http://www.swi-prolog.org/download/publications/jan-phd.pdf]]
as well as various
[[articles][http://www.swi-prolog.org/Publications.html]].  RoadMap.txt
explains the basic layout of the sources.

@author	Jan Wielemaker
@author Jacco van Ossenbruggen
@author Michiel Hildebrand

