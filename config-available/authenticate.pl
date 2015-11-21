:- module(conf_authenticate, []).
:- use_module(library(http/http_wrapper), []).
:- use_module(library(http/http_authenticate)).
:- use_module(user(user_db)).

/** <module> Protect entire server

This  config  file  protects  the  entire    server   using  basic  HTTP
authentication. This may be combined with configuring HTTPS as described
in https.pl.

ClioPatria will not force login if  no   users  are yet defined. In this
case you will be redirected to the `create admin user page'.  If youn do
not want that, delete the second clause.

@tbd	If you use this login mechanism, you cannot logout.  Maybe we
	should implement http://stackoverflow.com/questions/233507
*/

:- multifile
	http:request_expansion/2.

http:request_expansion(Request, Request) :-
	memberchk(authorization(Text), Request), !,
	(   http_authorization_data(Text, basic(User, Password)),
	    validate_password(User, Password)
	->  true
	;   throw(http_reply(authorise(basic, 'ClioPatria')))
	).
http:request_expansion(Request, Request) :-
	\+ current_user(_), !.			% allow if no users are defined
http:request_expansion(Request, Request) :-
	throw(http_reply(authorise(basic, 'ClioPatria'))).
