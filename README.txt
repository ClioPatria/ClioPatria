---+ SWI-Prolog SPARQL/SeRQL Engine

---++ About

This directory provides an implementation  of   RDF  query languages. It
started as an implementation of SeRQL   (www.openrdf.org)  and has later
been extended to include SPARQL.

Query languages are compiled into  a   Prolog  query, then optimised and
executed in the context of the SWI-Prolog   RDF store and runtime module
for the query language.  The engine is packaged as an HTTP service based
on standard HTTP access methods.

---++ Installation

The  configure  and  make  are   only    required   to  build  the  HTML
documentation. Further documentation is in serql.html

---++ Downloading

The server is distributed as  part   of  the  ClioPatria semantic search
web-server.     See     the      ClioPatria       home      page      at
http://e-culture.multimedian.nl/software/ClioPatria.shtml            for
downloading instructions.

---++ Further reading

Many of the design issues around handling the Semantic Web in Prolog are
described and motivated in Jan  Wielemaker's   PhD  thesis, which can be
downloaded from

	http://www.swi-prolog.org/download/publications/jan-phd.pdf

@author	Jan Wielemaker, J.Wielemaker@cs.vu.nl

