% The load file the DataHives project.

:- dynamic(user:project/3).
:- multifile(user:project/3).
   user:project('DataHives', 'Where agents travel across the Semantic Web',
       dh).

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

% Allow tests to be run from the top-level.
:- user:use_module(dh(test/dh_test)).

