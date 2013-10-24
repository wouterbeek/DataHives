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
@version 2013/08-2013/10
*/

:- use_module(dh(dh_net)).
:- use_module(dh(dh_samp)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_mat)).
:- use_module(rdf(rdf_web)).
% Loads the development server.
:- use_module(server(dev_server)).
% Sends debug statements to the development server.
:- use_module(server(web_console)).
:- use_module(tms(tms_web)).

% Set the global stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(global, limit(2*10**9)).
% Set the local stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(local, limit(2*10**9)).

:- register_module(dh_net).
:- register_module(rdf_web).
:- register_module(tms_web).

:- initialization(start_dev_server).



start_dh(HomeHive, Hives):-
  register_home_hive(HomeHive),
  connect_hives(Hives),
  rdf_create_graph(stash),
  start_materializer(stash, se, 60),
  multi(start_sampler(stash, 1), 5).

