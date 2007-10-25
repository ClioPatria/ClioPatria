---+ SWI-Prolog SeRQL Engine

---++ About

This directory contains an implementation of the SeRQL (Sesame RDF Query
Language) in SWI-Prolog based on the  SWI-Prolog Semweb package. It also
contains a partial implementation of the Sesame HTTP access protocol.

---++ Installation

The  configure  and  make  are   only    required   to  build  the  HTML
documentation. Further documentation is in serql.html

---++ CVS Access

The SWI-Prolog SeRQL engine can be  extracted through the SWI-Prolog CVS
server using the following commands:

==
	% cvs -d :pserver:pl@gollem.swi.psy.uva.nl:/usr/local/cvspl login
	Password: prolog
	% cvs -d :pserver:pl@gollem.swi.psy.uva.nl:/usr/local/cvspl co SeRQL
==

Due to a problem in CVS update   command concerning the module structure
used to share some parts of Triple20   in  this server, running a normal
"cvs update" will pull in all Triple20 files.   If  you are an expert in
CVS modules and you think  this  can   be  avoided,  please  contact me.
Otherwise use the command below  to   avoid  recursion into the Triple20
subdirectory.

==
	% cvs update -l
==

@author	Jan Wielemaker, wielemak@science.uva.nl

