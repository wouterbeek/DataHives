module(
  datahives,
  [
    start_datahives//0
  ]
).

/** <module> DATAHIVES

Bzzzzzzzzz... DataHives!

@author Wouter Beek
@version 2013/06
*/

:- use_module(generics(cowspeak)).
:- use_module(library(debug)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_serial)).
:- use_module(server(wallace)).
:- use_module(xml(xml_namespace)).

% Set the global stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(global, limit(2*10**9)).
% Set the local stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(local, limit(2*10**9)).

% Root
http:location(root, '/prasem/', []).

:- http_handler(root(.), index, [prefix]).
:- http_handler(root(hive), hive, [prefix]).

:- debug(datahives).



hive(Request):-
  %cowspeak('~w', [Request]),
  member(search(Search), Request),
  cowspeak('~w', [Search]).

index(_Request):-
  reply_html_page(datahives, [], []).

start_datahives:-
  cowspeak('Bzzzzzzzzz... DataHives!'),
  absolute_file_name(
    data('STCN_Agents'),
    File,
    [access(read), file_type(turtle)]
  ),
  rdf_load2(File, [format(turtle), graph(test)]).

user:body(datahives, Body) -->
  html(body(Body)).

user:head(datahives, Head) -->
  html(head(Head)).

