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
    plSparql,
    plRdf,
    plDev,
    plGraphViz,
    plTabular
]).

:- use_module(dh_test(dh_test)).

:- use_module(dh_agent(dh_agent)).
:- initialization(list_agents).

