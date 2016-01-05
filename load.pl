% The load file the DataHives project.

:- use_module(library(rdf/rdf_prefix)).

init_prefixes:-
  Prefix1 = 'http://localhost:8888/',
  rdf_register_prefix(dh, Prefix1),
  atomic_concat(Prefix1, 'ontology/', Prefix2),
  rdf_register_prefix(dho, Prefix2),
  atomic_concat(Prefix1, 'stats/', Prefix3),
  rdf_register_prefix(dhs, Prefix3).
:- init_prefixes.


% Register plTabular Web handler.
:- use_module(library(tab/tab)).

% Register DataHives Agent Web handler.
:- use_module(dh(agent/dh_agent)).

% Register DataHives Agent Definition Web handler.
:- use_module(dh(agent/def/dh_agent_definition)).

% Register DataHives Graphic Web handler.
:- use_module(dh(web/dh_agent_graphic)).

% Register DataHives Statistics  Web handler.
:- use_module(dh(stats/dh_stats_web)).

:- ensure_loaded(dh(agent/def/dh_agent_definition_init)).
