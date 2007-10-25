/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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

:- module(prolog_version,
	  [ check_prolog_version/1
	  ]).

                 /*******************************
                 *            VERSION           *
                 *******************************/

%%	check_prolog_version(+Required)
%	
%	Validate the program is running under Prolog version Required or
%	newer. Required is in numeric notation (e.g. 50317 for 5.3.17)

check_prolog_version(Required) :-
        current_prolog_flag(version, MyVersion),
        (   MyVersion >= Required
        ->  true
        ;   print_message(error,
			  required_prolog_version(Required)),
	    format(user_error, '~nPress any key to exit> ', []),
	    get_single_char(_), nl(user_error),
	    halt(1)
        ).
                                                                                
:- multifile
	prolog:message/3.

prolog:message(required_prolog_version(Required)) -->
	{ current_prolog_flag(version, MyVersion),
	  user_version(MyVersion, MyV),
	  user_version(Required, Req)
	},
	[ 'This program requires SWI-Prolog ~w'-[Req], nl,
	  'while you are running version ~w.'-[MyV], nl,
	  'Please visit http://www.swi-prolog.org and', nl,
	  'upgrade your version of SWI-Prolog.'
	].


user_version(N, Version) :-
        Major is N // 10000,
        Minor is (N // 100) mod 100,
        Patch is N mod 100,
        concat_atom([Major, Minor, Patch], '.', Version).
