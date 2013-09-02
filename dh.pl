:- module(
  dh,
  [
    start_dh/2 % +HomeHive:atom
               % +Hives:list(compound)
  ]
).

/** <module> DATAHIVES

Bzzzzzzzzz... DataHives!

@author Wouter Beek
@version 2013/08-2013/09
*/

:- use_module(dh(dh_net)).
:- use_module(dh(dh_samp)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(logic(rdf_axiom)).
% Loads the development server.
:- use_module(server(dev_server)).
% Sends debug statements to the development server.
:- use_module(server(web_message)).

% Set the global stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(global, limit(2*10**9)).
% Set the local stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(local, limit(2*10**9)).



start_dh(HomeHive, Hives):-
  register_home_hive(HomeHive),
  connect_hives(Hives),
  rdf_create_graph(stash),
  start_materializer(stash, 10),
  multi(start_sampler(stash, 30), 5).

