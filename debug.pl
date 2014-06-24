% Debug file for the DataHives project.

:- [load].



% Debug flag.
:- use_module(library(debug)).
:- debug(dh).



% Thread monitor.
%%%%:- use_module(library(swi_ide)).
%%%%:- prolog_ide(thread_monitor).



% Population-wide predicates for analytics.
:- use_module(dh_core(dh_population)).



% Print graph to PDF.

:- use_module(library(semweb/rdf_db)).

:- use_module(os(datetime_ext)).

:- use_module(plGraphViz(gv_file)).

:- use_module(dh_web(dh_gif)).



dh_graph_print(File):-
  dh_graph(Gif),
  dh_graph_print(Gif, File).


dh_graph_print(Gif, File):-
  current_date_time(DateTime),
  absolute_file_name(data(DateTime), File, [access(write),file_type(pdf)]),
  gif_to_gv_file([method(sfdp),to_file_type(pdf)], Gif, File).


print_resource(Resource, File):-
  findall(
    S-P-O,
    rdf(S, P, O, Resource),
    Es
  ),
  dh_gif:dh_graph(0, Es, Gif),
  dh_graph_print(Gif, File).

