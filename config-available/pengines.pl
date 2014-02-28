:- module(conf_pengines, []).
:- if(exists_source(library(pengines))).
:- use_module(api(pengines)).

/** <module> Configure Pengines access

Provide access to ClioPatria's RDF store using pengines.

@see http://www.swi-prolog.org/pldoc/package/pengines.html
*/

% :- set_setting_default(pengine:time_limit, 60).
:- set_setting_default(pengine:allow_from, ['127.0.0.1']).
% :- set_setting_default(pengine:deny_from,  []).

% DO NOT REMOVE THIS :- endif.
:- endif.
