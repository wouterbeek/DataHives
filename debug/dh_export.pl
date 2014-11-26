:- module(
  dh_export,
  [
    dh_export_graph/1, % ?File:atom
    dh_export_graph/2, % ?File:atom
                       % +Options:list(nvpair)
    dh_export_resource/3 % +Resource:iri
                         % ?File:atom
                         % +Options:list(nvpair)
  ]
).

/** <module> DataHives export

Predicates for exporting DataHives content.

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(os(datetime_ext)).

:- use_module(plGraphViz(gv_file)).

:- use_module(dh(web/dh_gif)).



%! dh_export_graph(+File:atom) is det.
% Wrapper around dh_export_graph/2 which exports to
% a minimally marked up DOT file.
%
% This can e.g. be used to export the DataHives graph for use in
% a tool for graph analysis.

dh_export_graph(File):-
  dh_export_graph(
    File,
    [
      edge_label(false),
      edge_penwidth(true),
      graph_directed(true),
      to_file_type(dot),
      vertex_image(false),
      vertex_label(false)
    ]
  ).

%! dh_export_graph(+File:atom, +Options:list(nvpair)) is det.
% The following options are supported:
%   * =|edge_label(+DrawLabel:boolean)|=
%     Default: `true`.
%   * =|edge_penwidth(+DrawPenwidth:boolean)|=
%     Default: `true`.
%   * =|graph_directed(+IsDirected:boolean)|=
%     Default: `true`.
%   * =|method(+Method:oneof([dot,sfdp])|=
%     The algorithm used by GraphViz for positioning the tree nodes.
%     Either =dot= (default) or =sfdp=.
%   * =|to_file_type(+FileType:oneof([dot,jpeg,pdf,svg,xdot])|=
%     The file type of the generated GraphViz file.
%     Default: `pdf`.
%   * =|vertex_image(+DrawImage:boolean)|=
%     Default: `true`.
%   * =|vertex_label(+DrawLabel:boolean)|=
%     Default: `true`.

dh_export_graph(File, Options):-
  ensure_file(File, Options),
  dh_graph(Gif, Options),
  graph_to_gv_file(Gif, File, Options).


dh_export_resource(Resource, File, Options):-
  ensure_file(File, Options),
  findall(
    S-P-O,
    rdf(S, P, O, Resource),
    Es
  ),
  dh_gif:dh_graph(0, Es, Gif, Options),
  graph_to_gv_file(Gif, File, Options).



% Helpers.

%! ensure_file(?File:atom, +Options:list(nvpair)) is det.

ensure_file(File, _):-
  nonvar(File), !.
ensure_file(File, Options):-
  current_date_time(DateTime),
  option(to_file_type(FileType), Options, pdf),
  absolute_file_name(
    data(DateTime),
    File,
    [access(write),file_type(FileType)]
  ).

