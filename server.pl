/*  $Id$

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(serql_http,
	  [
	  ]).
:- use_module(library(settings)).
:- use_module(api(sesame)).
:- use_module(http_sparql).
:- use_module(http_user).
:- use_module(http_admin).
:- use_module(user_db).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(thread_pool)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(library(settings)).

:- use_module(library(http/http_log)).
