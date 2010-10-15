:- module(mbox, []).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).

:- rdf_register_ns(foaf, 'http://xmlns.com/foaf/0.1/').

		 /*******************************
		 *	      QUERIES		*
		 *******************************/

%%	name_mbox(?Name, ?MBox) is nondet.
%
%	True if MBox is the URL for sending mail to Name.

name_mbox(Name,	MBox) :-
	rdf(X, foaf:name, literal(Name)),
	rdf(X, foaf:mbox, MBox).


		 /*******************************
		 *	      SIMPLE		*
		 *******************************/

:- http_handler('/mbox/simple', simple_mbox_table, []).

%%	simple_mbox_table(+HTTPRequest)
%
%	Write a table the naive way.

simple_mbox_table(_Request) :-
	format('Content-type: text/html~n~n'),
	format('<table>~n'),
	forall(name_mbox(Name, MBox),
	       format('<tr><td>~w<td>~w~n', [Name, MBox])),
	format('</table>~n').
	    

		 /*******************************
		 *	       GOOD		*
		 *******************************/

:- use_module(library(http/html_write)).
:- http_handler('/mbox', mbox_table, []).

%%	mbox_table(+HTTPRequest)
%
%	Write a table the proper way. This code is guaranteed to produce
%	legal HTML and can, based on a   global switch, be configured to
%	produce XHTML or plain HTML.

mbox_table(_Request) :-
	findall(Name-MBox, name_mbox(Name, MBox), Pairs),
	reply_html_page(title('Mailboxes'),
			table(\mailboxes(Pairs))).

mailboxes([]) -->
	[].
mailboxes([Name-MBox|T]) -->
	html(tr([td(Name), td(MBox)])),
	mailboxes(T).


		 /*******************************
		 *	       JSON		*
		 *******************************/

:- use_module(library(http/http_json)).
:- http_handler('/mbox/json', json_mbox_table, []).

%%	json_mbox_table(+HTTPRequest)
%
%	Write a table as a  JSON  object,   leaving  the  rendering to a
%	Javascript client library.

json_mbox_table(_Request) :-
	findall(json([ name=Name,
		       mbox=MBox
		     ]),
		name_mbox(Name, MBox), Objects),
	reply_json(Objects).
