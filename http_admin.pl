/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(http_admin,
	  [ reload_attr/2		% +Window, -Attribute
	  ]).
:- use_module(user_db).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_session')).
:- use_module(library('http/html_write')).
:- use_module(library('http/mimetype')).
:- use_module(library('http/http_dispatch')).
:- use_module(library(url)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(http_settings)).


:- http_handler(serql('admin/tasks'),		      tasks,		       []).
:- http_handler(serql('admin/listUsers'),	      list_users,	       []).
:- http_handler(serql('admin/form/createAdmin'),      create_admin,	       []).
:- http_handler(serql('admin/form/addUser'),	      add_user_form,	       []).
:- http_handler(serql('admin/form/addOpenIDServer'),  add_openid_server_form,  []).
:- http_handler(serql('admin/addUser'),		      add_user,		       []).
:- http_handler(serql('admin/addOpenIDServer'),	      add_openid_server,       []).
:- http_handler(serql('admin/form/editUser'),	      edit_user_form,	       []).
:- http_handler(serql('admin/editUser'),	      edit_user,	       []).
:- http_handler(serql('admin/delUser'),		      del_user,		       []).
:- http_handler(serql('admin/form/editOpenIDServer'), edit_openid_server_form, []).
:- http_handler(serql('admin/editOpenIDServer'),      edit_openid_server,      []).
:- http_handler(serql('admin/delOpenIDServer'),	      del_openid_server,       []).
:- http_handler(serql('admin/form/changePassword'),   change_password_form,    []).
:- http_handler(serql('admin/changePassword'),	      change_password,	       []).
:- http_handler(serql('user/form/login'),	      login_form,	       []).
:- http_handler(serql('user/login'),		      user_login,	       []).
:- http_handler(serql('user/logout'),		      user_logout,	       []).
:- http_handler(serql('admin/settings'),	      settings,		       []).
:- http_handler(serql('admin/save_settings'),	      save_settings,	       []).
:- http_handler(serql('css/settings.css'),
		http_reply_file(library('settings.css'), []),
		[id(settings_css)]).

%%	tasks(+Request)
%
%	Present menu with administrative tasks.

tasks(_Request) :-
	reply_page('Administrative tasks',
		   [ \action(location_by_id(list_users), 'List users')
		   ]).


action(URL, Label) -->
	html([a([target(main), href(URL)], Label), br([])]).

%%	list_users(+Request)
%
%	HTTP Handler listing registered users.

list_users(_Request) :-
	authorized(admin(list_users)),
	reply_page('Users',
		   [ h1('Users'),
		     \user_table,
		     p([ \action(location_by_id(add_user_form), 'Add user')
		       ]),
		     h1('OpenID servers'),
		     \openid_server_table,
		     p([ \action(location_by_id(add_openid_server_form), 'Add OpenID server')
		       ])
		   ]).

%%	user_table//
%
%	HTML DCG generating a table of registered users.

user_table -->
	{ setof(U, current_user(U), Users)
	},
	html([ table([ border(1)
		     ],
		     [ tr([ th('UserID'),
			    th('RealName'),
			    th('On since'),
			    th('Idle')
			  ])
		     | \list_users(Users)
		     ])
	     ]).

list_users([]) -->
	[].
list_users([User|T]) -->
	{ user_property(User, realname(Name)),
	  findall(Idle-Login,
		  user_property(User, connection(Login, Idle)),
		  Pairs0),
	  keysort(Pairs0, Pairs),
	  (   Pairs == []
	  ->  OnLine = (-)
	  ;   length(Pairs, N),
	      Pairs = [Idle-Login|_],
	      OnLine = online(Login, Idle, N)
	  )
	},
	html(tr([ td(User),
		  td(Name),
		  td(\on_since(OnLine)),
		  td(\idle(OnLine)),
		  td(a(href(location_by_id(edit_user_form)+'?user='+encode(User)), 'Edit'))
		])),
	list_users(T).

on_since(online(Login, _Idle, _Connections)) --> !,
	{ format_time(string(Date), '%+', Login)
	},
	html(Date).
on_since(_) -->
	html(-).

idle(online(_Login, Idle, _Connections)) -->
	{ mmss_duration(Idle, String)
	},
	html(String).
idle(_) -->
	html(-).


mmss_duration(Time, String) :-		% Time in seconds
	Secs is round(Time),
	Hour is Secs // 3600,
	Min  is (Secs // 60) mod 60,
	Sec  is Secs mod 60,
	format(string(String), '~`0t~d~2|:~`0t~d~5|:~`0t~d~8|', [Hour, Min, Sec]).



		 /*******************************
		 *	      ADD USERS		*
		 *******************************/

%%	create_admin(+Request)
%
%	Create the administrator login.

create_admin(_Request) :-
	(   current_user(_)
	->  throw(error(permission_error(create, user, admin),
			context(_, 'Already initialized')))
	;   true
	),
	reply_page('Create administrator',
		   [ h1(align(center), 'Create administrator'),

		     p('No accounts are available on this server. \
		        This form allows for creation of an administrative \
			account that can subsequently be used to create \
			new users.'),

		     form([ action(location_by_id(add_user)),
			    method('GET')
			  ],
			  [ \hidden(read, on),
			    \hidden(write, on),
			    \hidden(admin, on),

			    table([ border(1),
				    align(center)
				  ],
				  [ \input(user, 'Name',
					   [value('admin')]),
				    \input(realname, 'Realname',
					   [value('Administrator')]),
				    \input(pwd1,     'Password',
					   [type(password)]),
				    \input(pwd2,     'Retype',
					   [type(password)]),
				    tr(td([ colspan(2),
					    align(right)
					  ],
					  input([ type(submit),
						  value('Create')
						])))
				  ])
			  ])
		   ]).


%%	add_user_form(+Request)
%
%	Form to register a user.

add_user_form(_Request) :-
	authorized(admin(add_user)),
	reply_page('Add new user',
		   [ \new_user_form
		   ]).

new_user_form -->
	html([ h1('Add new user'),
	       form([ action(location_by_id(add_user)),
		      method('GET')
		    ],
		    table([ border(1)
			  ],
			  [ \input(user,     'Name',
				   []),
			    \input(realname, 'Realname',
				   []),
			    \input(pwd1,     'Password',
				   [type(password)]),
			    \input(pwd2,     'Retype',
				   [type(password)]),
			    \permissions(-),
			    tr(td([ colspan(2),
				    align(right)
				  ],
				  input([ type(submit),
					  value('Create')
					])))
			  ]))
	     ]).


input(Name, Label, Options) -->
	html(tr([ td(align(right), Label),
		  td(input([name(Name),size(40)|Options]))
		])).

%%	add_user(+Request)
%
%	API  to  register  a  new  user.  The  current  user  must  have
%	administrative rights.

add_user(Request) :-
	(   \+ current_user(_)
	->  true
	;   authorized(admin(add_user))
	),
	http_parameters(Request,
			[ user(User, 	     [ length > 2 ]),
			  realname(RealName, [ length > 2 ]),
			  pwd1(Password,     [ length > 5 ]),
			  pwd2(Retype,       [ length > 5 ]),
			  read(Read),
			  write(Write),
			  admin(Admin)
			],
			[ attribute_declarations(attribute_decl)
			]),
	(   current_user(User)
	->  throw(error(permission_error(create, user, User),
			context(_, 'Already present')))
	;   true
	),
	(   Password == Retype
	->  true
	;   throw(password_mismatch)
	),
	password_hash(Password, Hash),
	phrase(allow(Read, Write, Admin), Allow),
	user_add(User,
		 [ realname(RealName),
		   password(Hash),
		   allow(Allow)
		 ]),
	(   User == admin
	->  user_add(anonymous,
		     [ realname('Define rights for not-logged in users'),
		       allow([read(_,_)])
		     ]),
	    reply_login([user(User), password(Password)])
	;   list_users(Request)
	).

%%	edit_user_form(+Request)
%
%	Form to edit user properties

edit_user_form(Request) :-
	authorized(admin(user(edit))),
	http_parameters(Request,
			[ user(User, [])
			]),

	user_property(User, realname(RealName)),

	reply_page('Edit user',
		   [ h4(['Edit user ', User, ' (', RealName, ')']),

		     form([ action(location_by_id(edit_user)),
			    method('GET')
			  ],
			  [ \hidden(user, User),
			    table([ border(1),
				    align(center)
				  ],
				  [ \user_property(User,
						   realname,
						   'Realname',
						   []),
				    \permissions(User),
				    tr(td([ colspan(2),
					    align(right)
					  ],
					  input([ type(submit),
						  value('Modify')
						])))
				  ])
			  ]),

		     p([ \action(location_by_id(del_user)+'?user='+encode(User),
				 [ 'Delete ',
				   b(User),
				   ' (', b(RealName), ')'
				 ])
		       ])
		   ]).

user_property(User, Name, Label, Options) -->
	{  Term =.. [Name, Value],
	   user_property(User, Term)
	-> O2 = [value(Value)|Options]
	;  O2 = Options
	},
	html(tr([ td(align(right), Label),
		  td(input([name(Name),size(40)|O2]))
		])).

permissions(User) -->
	html(tr([ td(align(right), 'Permissions'),
		  td([ \permission_checkbox(User, read,  'Read'),
		       \permission_checkbox(User, write, 'Write'),
		       \permission_checkbox(User, admin, 'Admin')
		     ])
		])).

permission_checkbox(User, Name, Label) -->
	{ (   User \== (-),
	      (	  user_property(User, allow(Actions))
	      ->  true
	      ;	  openid_server_property(User, allow(Actions))
	      ),
	      pterm(Name, Action),
	      memberchk(Action, Actions)
	  ->  Opts = [checked]
	  ;   Opts = []
	  )
	},
	html([ input([ type(checkbox),
		       name(Name)
		     | Opts
		     ]),
	       Label
	     ]).

%%	edit_user(Request)
%
%	Handle reply from edit user form.

edit_user(Request) :-
	authorized(admin(user(edit))),
	http_parameters(Request,
			[ user(User, []),
			  realname(RealName,
				   [ optional(true),
				     length > 2
				   ]),
			  read(Read),
			  write(Write),
			  admin(Admin)
			],
			[ attribute_declarations(attribute_decl)
			]),
	modify_user(User, realname(RealName)),
	modify_permissions(User, Read, Write, Admin),
	list_users(Request).


modify_user(User, Property) :-
	Property =.. [_Name|Value],
	(   (   var(Value)
	    ;	Value == ''
	    )
	->  true
	;   set_user_property(User, Property)
	).

modify_permissions(User, Read, Write, Admin) :-
	phrase(allow(Read, Write, Admin), Allow),
	set_user_property(User, allow(Allow)).

allow(Read, Write, Admin) -->
	allow(read, Read),
	allow(write, Write),
	allow(admin, Admin).

allow(Access, on) -->
	{ pterm(Access, Allow)
	}, !,
	[ Allow
	].
allow(_Access, off) --> !,
	[].

pterm(read,  read(_Repositiory, _Action)).
pterm(write, write(_Repositiory, _Action)).
pterm(admin, admin(_Action)).


%%	del_user(+Request)
%
%	Delete a user

del_user(Request) :- !,
	authorized(admin(del_user)),
	http_parameters(Request,
			[ user(User, [])
			]),
	(   User == admin
	->  throw(error(permission_error(delete, user, User), _))
	;   true
	),
	user_del(User),
	list_users(Request).


%%	change_password_form(+Request)
%
%	Allow user to change the password

change_password_form(_Request) :-
	logged_on(User),
	user_property(User, realname(RealName)),
	reply_page('Change password',
		   [ h4(['Change password for ', User, ' (', RealName, ')']),

		     form([ action(location_by_id(change_password)),
			    method('GET')
			  ],
			  [ table([ border(1),
				    align(center)
				  ],
				  [ \user_or_old(User),
				    \input(pwd1,     'New Password',
					   [type(password)]),
				    \input(pwd2,     'Retype',
					   [type(password)]),
				    tr(td([ align(right),
					    colspan(2)
					  ],
					  input([ type(submit),
						  value('Change password')
						])))
				  ])
			  ])
		   ]).

user_or_old(admin) --> !,
	input(user, 'User', []).
user_or_old(_) -->
	input(pwd0, 'Old password', [type(password)]).


%%	change_password(+Request)
%
%	Actually change the password.  The user must be logged on.

change_password(Request) :-
	logged_on(Login),
	http_parameters(Request,
			[ user(User,     [ optional(true) ]),
			  pwd0(Password, [ optional(true) ]),
			  pwd1(New,      [ length > 5 ]),
			  pwd2(Retype,   [ length > 5 ])
			]),
	(   Login == admin
	->  (   current_user(User)
	    ->	true
	    ;	throw(error(existence_error(user, User), _))
	    )
	;   Login = User,
	    validate_password(User, Password)
	),
	(   New == Retype
	->  true
	;   throw(password_mismatch)
	),
	password_hash(New, Hash),
	set_user_property(User, password(Hash)),
	reply_page('Password changed',
		   [ h1(align(center), 'Password changed'),
		     p([ 'Your password has been changed successfully' ])
		   ]).


		 /*******************************
		 *	       LOGIN		*
		 *******************************/

%%	login_form(+Request)
%
%	HTTP handler that presents a form to login.

login_form(_Request) :-
	reply_page('Login',
		   [ h1(align(center), 'Login'),
		     form([ action(location_by_id(user_login)),
			    method('GET')
			  ],
			  table([ tr([ th(align(right), 'User:'),
				       td(input([ name(user),
						  size(40)
						]))
				     ]),
				  tr([ th(align(right), 'Password:'),
				       td(input([ type(password),
						  name(password),
						  size(40)
						]))
				     ]),
				  tr([ td([ align(right), colspan(2) ],
					  input([ type(submit),
						  value('Login')
						]))
				     ])
				])
			 )
		   ]).

%%	user_login(+Request)
%
%	Handle  =user=  and  =password=.  If    there   is  a  parameter
%	=return_to= or =|openid.return_to|=, reply using   a redirect to
%	the given URL. Otherwise display a welcome page.

user_login(Request) :- !,
	http_parameters(Request,
			[ user(User, []),
			  password(Password, []),
			  'openid.return_to'(ReturnTo, [optional(true)]),
			  'return_to'(ReturnTo, [optional(true)])
			]),
	(   var(ReturnTo)
	->  Extra = []
	;   Extra = [ return_to(ReturnTo) ]
	),
	reply_login([ user(User),
		      password(Password)
		    | Extra
		    ]).


reply_login(Options) :-
	option(user(User), Options),
	option(password(Password), Options),
	validate_password(User, Password), !,
	login(User),
	(   option(return_to(ReturnTo), Options)
	->  throw(http_reply(moved_temporary(ReturnTo)))
	;   reload_attr(sidebar, OnLoad),
	    reply_page('Login ok',
		       body([ OnLoad
			    ],
			    [ h1(align(center), ['Welcome ', User])
			    ]))
	).
reply_login(_) :-
	reply_page('Login failed',
		   [ h1(align(center), 'Login failed'),
		     p(['Password incorrect'])
		   ]).

%%	user_logout(+Request)
%
%	Logout the current user

user_logout(_Request) :-
	logged_on(User),
	logout(User),
	reload_attr(sidebar, OnLoad),
	reply_page('Logout',
		   body([ OnLoad
			],
			[ h1(align(center), ['Logged out ', User])
			])).

reload_attr(Frame, onLoad(Script)) :-
	concat_atom([ 'top.frames[\'', Frame, '\'].location=top.frames[\'',
		      Frame, '\'].location.href'
		    ], Script).


attribute_decl(read,
	       [ description('Provide read-only access to the RDF store')
	       | Options])   :- bool(off, Options).
attribute_decl(write,
	       [ description('Provide write access to the RDF store')
	       | Options])   :- bool(off, Options).
attribute_decl(admin,
	       [ description('Provide administrative rights')
	       | Options])   :- bool(off, Options).

bool(Def,
     [ default(Def),
       oneof([on, off])
     ]).


		 /*******************************
		 *	    OPENID ADMIN	*
		 *******************************/

%%	add_openid_server_form(+Request)
%
%	Return an HTML page to add a new OpenID server.

add_openid_server_form(_Request) :-
	authorized(admin(add_openid_server)),
	reply_page('Add OpenID server',
		   [ \new_openid_form
		   ]).


%%	new_openid_form// is det.
%
%	Present form to add a new OpenID provider.

new_openid_form -->
	html([ h1('Add new OpenID server'),
	       form([ action(location_by_id(add_openid_server)),
		      method('GET')
		    ],
		    table([ border(1)
			  ],
			  [ \input(openid_server, 'Server homepage', []),
			    \input(openid_description, 'Server description',
				   []),
			    \permissions(-),
			    tr(td([ colspan(2),
				    align(right)
				  ],
				  input([ type(submit),
					  value('Create')
					])))
			  ])),
	       p([ 'Use this form to define access rights for users of an ',
		   a(href('http://www.openid.net'), 'OpenID'), ' server.',
		   'The special server', code(*), ' specifies access for all OpenID servers.',
		   'Here are some examples of servers:'
		 ]),
	       ul([ li(code('http://myopenid.com')),
		    li(code('http://videntity.org'))
		  ])
	     ]).


%%	add_openid_server(+Request)
%
%	Allow access from an OpenID server

add_openid_server(Request) :-
	authorized(admin(add_openid_server)),
	http_parameters(Request,
			[ openid_server(Server0,
					[ description('URL of the server to allow')]),
			  openid_description(Description,
					     [ optional(true),
					       description('Description of the server')
					     ]),
			  read(Read),
			  write(Write)
			],
			[ attribute_declarations(attribute_decl)
			]),
	phrase(allow(Read, Write, off), Allow),
	canonical_url(Server0, Server),
	Options = [ description(Description),
		    allow(Allow)
		  ],
	remove_optional(Options, Properties),
	openid_add_server(Server, Properties),
	list_users(Request).

remove_optional([], []).
remove_optional([H|T0], [H|T]) :-
	arg(1, H, A),
	nonvar(A), !,
	remove_optional(T0, T).
remove_optional([_|T0], T) :-
	remove_optional(T0, T).


canonical_url(Var, Var) :-
	var(Var), !.
canonical_url(*, *) :- !.
canonical_url(URL0, URL) :-
	parse_url(URL0, Parts),
	parse_url(URL, Parts).


%%	edit_openid_server_form(+Request)
%
%	Form to edit user properties

edit_openid_server_form(Request) :-
	authorized(admin(openid(edit))),
	http_parameters(Request,
			[ openid_server(Server, [])
			]),

	reply_page('Edit OpenID server',
		   [ h4(['Edit OpenID server ', Server]),

		     form([ action(location_by_id(edit_openid_server)),
			    method('GET')
			  ],
			  [ \hidden(openid_server, Server),
			    table([ border(1)
				  ],
				  [ \openid_property(Server, description, 'Description', []),
				    \permissions(Server),
				    tr(td([ colspan(2),
					    align(right)
					  ],
					  input([ type(submit),
						  value('Modify')
						])))
				  ])
			  ]),

		     p([ \action(location_by_id(del_openid_server) +
				 '?openid_server=' + encode(Server),
				 [ 'Delete ', b(Server) ]) ])
		   ]).


openid_property(Server, Name, Label, Options) -->
	{  Term =.. [Name, Value],
	   openid_server_property(Server, Term)
	-> O2 = [value(Value)|Options]
	;  O2 = Options
	},
	html(tr([ td(align(right), Label),
		  td(input([name(Name),size(40)|O2]))
		])).


%%	openid_server_table//
%
%	List registered openid servers

openid_server_table -->
	{ setof(S, openid_current_server(S), Servers), !
	},
	html([ table([ border(1)
		     ],
		     [ tr([ th('Server'),
			    th('Description')
			  ])
		     | \openid_list_servers(Servers)
		     ])
	     ]).
openid_server_table -->
	[].

openid_list_servers([]) -->
	[].
openid_list_servers([H|T]) -->
	openid_list_server(H),
	openid_list_servers(T).

openid_list_server(Server) -->
	html(tr([td(\openid_server(Server)),
		 td(\openid_field(Server, description)),
		 td(a(href(location_by_id(edit_openid_server_form) +
			   '?openid_server='+encode(Server)
			  ), 'Edit'))
		])).

openid_server(*) --> !,
	html(*).
openid_server(Server) -->
	html(a(href(Server), Server)).

openid_field(Server, Field) -->
	{ Term =.. [Field, Value],
	  openid_server_property(Server, Term)
	}, !,
	html(Value).
openid_field(_, _) -->
	[].


%%	edit_openid_server(Request)
%
%	Handle reply from OpenID server form.

edit_openid_server(Request) :-
	authorized(admin(openid(edit))),
	http_parameters(Request,
			[ openid_server(Server, []),
			  description(Description,
				      [ optional(true),
					length > 2
				      ]),
			  read(Read),
			  write(Write),
			  admin(Admin)
			],
			[ attribute_declarations(attribute_decl)
			]),
	modify_openid(Server, description(Description)),
	openid_modify_permissions(Server, Read, Write, Admin),
	list_users(Request).


modify_openid(User, Property) :-
	Property =.. [_Name|Value],
	(   (   var(Value)
	    ;	Value == ''
	    )
	->  true
	;   openid_set_property(User, Property)
	).


openid_modify_permissions(Server, Read, Write, Admin) :-
	phrase(allow(Read, Write, Admin), Allow),
	openid_set_property(Server, allow(Allow)).


%%	del_openid_server(+Request)
%
%	Delete an OpenID Server

del_openid_server(Request) :- !,
	authorized(admin(openid(delete))),
	http_parameters(Request,
			[ openid_server(Server, [])
			]),
	openid_del_server(Server),
	list_users(Request).


		 /*******************************
		 *	       SETTINGS		*
		 *******************************/

%%	settings(+Request)
%
%	Show current settings. Is user is =admin=, allow editing the
%	settings.

settings(_Request) :-
	(   catch(authorized(admin(edit_settings)), _, fail)
	->  Edit = true
	;   authorized(read(admin, settings)),
	    Edit = false
	),
	phrase(page([ title('Settings'),
		      link([ rel(stylesheet),
			     type('text/css'),
			     href(location_by_id(settings_css))
			   ])
		    ],
		    [ \http_show_settings([ edit(Edit),
					    hide_module(false),
					    action('save_settings')
					  ])
		    ]), HTML),
	format('Content-type: text/html~n~n'),
	print_html(HTML).

%%	save_settings(+Request)
%
%	Save modified settings.

save_settings(Request) :-
	authorized(admin(edit_settings)),
	phrase(page([ title('Save settings'),
		      link([ rel(stylesheet),
			     type('text/css'),
			     href(location_by_id(settings_css))
			   ])
		    ],
		    [ \http_apply_settings(Request, [save(true)])
		    ]), HTML),
	format('Content-type: text/html~n~n'),
	print_html(HTML).


		 /*******************************
		 *		EMIT		*
		 *******************************/

%%	hidden(+Name, +Value)
%
%	Create a hidden input field with given name and value

hidden(Name, Value) -->
	html(input([ type(hidden),
		     name(Name),
		     value(Value)
		   ])).


reply_page(Title, Content) :-
	phrase(page(title(Title), Content), HTML),
	format('Content-type: text/html~n~n'),
	print_html(HTML).

%	Support Cross-Referencer and PceEmacs.

:- multifile
	emacs_prolog_colours:goal_colours/2,
	prolog:called_by/2.


emacs_prolog_colours:goal_colours(reply_page(_, HTML),
				  built_in-[classify, Colours]) :-
	catch(html_write:html_colours(HTML, Colours), _, fail).
prolog:called_by(reply_page(_, HTML), Called) :-
	catch(phrase(html_write:called_by(HTML), Called), _, fail).
