% The load file for DataHives.

:- multifile(user:prolog/3).
:- dynamic(user:prolog/3).
user:project('DataHives', 'Where agents travel across the Semantic Web', dh).

:- use_module(load_project).
:- load_project([
    plc-'Prolog-Library-Collection',
    plHtml,
    plServer,
    plRdf,
    plDev,
    plTabular
]).

% Load the Web-based development environment and some tests.
:- use_module(plDev(plDev)).
:- use_module(dh_web(dh_web_agent)).
:- use_module(dh_web(dh_web_graph)).
:- use_module(dh_test(dh_test)).

