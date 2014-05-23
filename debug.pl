% Debug file for the DataHives project.

:- [load].

% Debug flag.
:- use_module(library(debug)).
:- debug(dh).

% Thread monitor.
%%%%:- use_module(library(swi_ide)).
%%%%:- prolog_ide(thread_monitor).

% DataHives debug tools.
:- use_module(dh_web(dh_web_agent)).
:- use_module(dh_web(dh_web_graph)).

% DataHives example run.
:- use_module(dh_test(dh_test)).

