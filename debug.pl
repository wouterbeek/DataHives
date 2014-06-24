% Debug file for the DataHives project.

:- [load].

% Debug flag.
:- use_module(library(debug)).
:- debug(dh).

% Thread monitor.
:- use_module(library(swi_ide)).
:- prolog_ide(thread_monitor).

% Population-wide predicates for analytics.
:- use_module(dh_core(dh_population)).

