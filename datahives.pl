:- module(
  datahives,
  [
    start_dh_web/1 % -DOM:list
  ]
).

/** <module> DATAHIVES

Bzzzzzzzzz... DataHives!

@author Wouter Beek
@version 2013/08
*/

:- use_module(graph_theory(graph_traversal)).
:- use_module(gv(gv_file)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf_graph(rdf_graph_theory)).
:- use_module(server(dev_server)).
:- use_module(server(web_console)).

% Set the global stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(global, limit(2*10**9)).
% Set the local stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(local, limit(2*10**9)).

:- debug(datahives).

:- register_module(datahives).

:- use_module(datahives(hives_test)).
:- initialization(run_tests(hives)).



init_dh:-
/*
  absolute_file_name(
    data('STCN_Agents'),
    File,
    [access(read),file_type(turtle)]
  ),
  rdf_load2(File, [format(turtle),graph(dh)]).
*/
  absolute_file_name(rdfs(rdfs), File, [access(read),file_type(rdf)]),
  rdf_load2(File, [graph(dh)]).

start_dh_web(SVG):-
  rdf_random_term(dh, V1),
  rdf_random_term(dh, V2),
gtrace,
  traverse(
    [deb_vertex_name(rdf_term_name)],
    dh,
    rdf_edges,
    rdf_neighbor,
    V1,
    V2,
    Dist,
    Vs,
    Es,
    Hist
  ),
  format(user_output, '~w\n~w\n~w\n~w\n', [Dist,Vs,Es,Hist]),
  export_rdf_graph(
    [
      colorscheme(svg),
      edge_labels(replace),
      language(en),
      literals(preferred_label),
      uri_desc(uri_only)
    ],
    dh,
    GIF
  ),
  graph_to_svg_dom([], GIF, sfdp, SVG).

