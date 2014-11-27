% The load file the DataHives project.



% Register RDF prefixes.
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

init_prefixes:-
  Prefix1 = 'http://localhost:8888/',
  
  % dh
  rdf_register_prefix(dh, Prefix1),
  
  % dho
  atomic_concat(Prefix1, 'ontology/', Prefix4),
  rdf_register_prefix(dho, Prefix4),
  
  % dh-stats
  atomic_concat(Prefix1, 'stats/', Prefix5),
  rdf_register_prefix('dh-stats', Prefix5).
:- init_prefixes.



% Project name setting.
:- dynamic(user:project/3).
:- multifile(user:project/3).
   user:project('DataHives', 'Where agents travel across the Semantic Web',
       dh).



% Load modules.
:- use_module(load_project).
:- load_project([
     lodCache,
     mt-'ModelTheory',
     plc-'Prolog-Library-Collection',
     plDcg,
     plGraph,
     plGraphDraw,
     plGraphViz,
     plHtml,
     plHttp,
     plLangTag,
     plLattice,
     plLatticeDraw,
     plRdf,
     plServer,
     plSet,
     plSparql,
     plSvg,
     plTabular,
     plTms,
     plTree,
     plTreeDraw,
     plUri,
     plXml,
     plXsd
]).



% Register plTabular Web handler.
:- use_module(plTabular(rdf_tabular)).

% Register DataHives Agent Web handler.
:- use_module(dh(agent/dh_agent)).

% Register DataHives Agent Definition Web handler.
:- use_module(dh(agent/def/dh_agent_definition)).

% Register DataHives Graphic Web handler.
:- use_module(dh(web/dh_agent_graphic)).

% Register DataHives Statistics  Web handler.
:- use_module(dh(stats/dh_stats_web)).
