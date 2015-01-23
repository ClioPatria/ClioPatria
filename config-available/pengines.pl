:- module(conf_pengines, []).
:- if(exists_source(library(pengines))).
:- use_module(api(pengines)).

/** <module> Configure Pengines access

Provide access to ClioPatria's RDF store using pengines.

@see http://www.swi-prolog.org/pldoc/package/pengines.html
@see http://cliopatria.swi-prolog.org/packs/swish
*/

% Maximum time a pengine may execute
% :- set_setting_default(pengines:time_limit, 300).

% Only allow connections from localhost.  Use `[*]` to allow access from
% anywhere.  Note that the cpack `swish` also depends on this setting.
:- set_setting_default(pengines:allow_from, ['127.0.0.1']).

% Deny hosts/networks.  Deny rules are applied after allow rules, i.e.,
% you are granted access if you are allowed and not denied.
% :- set_setting_default(pengines:deny_from,  []).

% DO NOT REMOVE THIS :- endif.
:- endif.
