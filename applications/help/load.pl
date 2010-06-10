/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
		   VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(cp_help, []).
:- use_module(library(doc_http)).       % Load pldoc
:- use_module(library(http/http_hook)). % Get hook signatures
:- use_module(http_help).		% Help on HTTP server

/** <module> Support PlDoc

Integrates PlDoc from /help/source/
*/

%       http:location(pldoc, Location, Options) is det.
%
%       Rebase PlDoc to <prefix>/help/source/

http:location(pldoc, root('help/source'), [priority(10)]).

:- multifile
	http_user:menu_item/2.

http_user:menu_item(help/http_help,	'HTTP API').
http_user:menu_item(help/pldoc_root,	'Source code').
