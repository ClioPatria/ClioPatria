:- module(conf_fix_menu, []).
:- use_module(library(http/html_head)).

/** <module> Fix the position of the menu and search box

This module applies an additional style-file  that changes the behaviour
of the menu and  search-box,  such  that   it  stays  on  the  page when
scrolling.

Note that the effect is established by   executing  a directive and thus
cannot be undone by unloading this file.
*/

:- html_resource(cliopatria,
		 [ virtual(true),
		   requires([ css('fix_menu.css')
			    ])
		 ]).

