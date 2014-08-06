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
    plGraph,
    plRdf,
    plDev,
    plGraphViz,
    plTabular
]).

% Load the agent definitions.
:- ensure_loaded(dh_agent(dh_agent_init)).

% Allow tests to be run from the top-level.
:- user:use_module(dh_test(dh_test)).

% List the loaded agent definitions.
:- use_module(dh_agent(dh_agent_doc)).
:- initialization(list_agent_definitions).

