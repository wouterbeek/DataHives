% The load file the DataHives project.

:- dynamic(user:prolog/3).
:- multifile(user:prolog/3).
   user:project('DataHives', 'Where agents travel across the Semantic Web',
       dh).

:- use_module(load_project).
:- load_project([
    lodCache,
    plc-'Prolog-Library-Collection',
    plHtml,
    plServer,
    plRdf,
    plDev,
    plGraphViz,
    plTabular
]).

% Load the Web-based development environment and some tests.
:- ensure_loaded(plTabular(set_default_http_handler)).
:- use_module(dh_web(dh_web_agent)).
:- use_module(dh_web(dh_web_graph)).
:- use_module(dh_test(dh_test)).

