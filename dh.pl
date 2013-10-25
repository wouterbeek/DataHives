:- module(dh, []).

/** <module> DataHives

Bzzzzzzzzz... DataHives!

@author Wouter Beek
@version 2013/08-2013/10
*/

:- use_module(dh(dh_network)).
:- use_module(dh(dh_program)).
:- use_module(rdf(rdf_web)).
:- use_module(server(dev_server)).
% Sends debug statements to the development server.
:- use_module(server(web_console)).
:- use_module(tms(tms_web)).

% Set the global stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(global, limit(2*10**9)).
% Set the local stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(local, limit(2*10**9)).

:- register_module(dh_network).
:- register_module(dh_program).
:- register_module(rdf_web).
:- register_module(tms_web).

:- initialization(start_dev_server).

