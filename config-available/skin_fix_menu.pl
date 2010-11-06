:- module(conf_fix_menu, []).
:- use_module(library(http/html_head)).

/** <module> Fix the position of the menu and search box

This module applies an additional style-file  that changes the behaviour
of the menu and  search-box,  such  that   it  stays  on  the  page when
scrolling.

Note  that  the  order  of  resources  inserted  by  html_resource/2  is
undefined. Ordering can be  forced  by   making  one  resource depend on
another. In this case, the dependent resource  will always be later than
the dependee. E.g., if fix_menu.css must be loaded after cliopatria.css,
add a declaration like this:

    ==
    :- html_resource(css('fix_menu.css'),
		     [ requires([css('cliopatria.css')])
		     ]).
    ==
*/

:- html_resource(cliopatria,
		 [ virtual(true),
		   requires([ css('fix_menu.css')
			    ])
		 ]).

