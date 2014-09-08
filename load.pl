% The load file the DataHives project.

:- dynamic(user:prolog/3).
:- multifile(user:prolog/3).
   user:project('DataHives', 'Where agents travel across the Semantic Web',
       dh).

:- use_module(load_project).
:- load_project([
     plc-'Prolog-Library-Collection',
     plDcg,
     lodCache,
     plGraph,
     plXsd,
     plRdf,
     plHtml,
     plSparql,
     plGraphViz,
     plTabular
]).

% Allow tests to be run from the top-level.
:- user:use_module(dh_test(dh_test)).

