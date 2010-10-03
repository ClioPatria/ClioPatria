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

ClioPatria can be used as a   library  by loading cliopatria.pl together
with any other  relevant  code  into   SWI-Prolog.  The  file  run.pl.in
provides a skeleton  startup  script   for  loading  a  ClioPatria-based
application.  Please check the comments in the file.


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

