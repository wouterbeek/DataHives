% Debug file for the DataHives project.

% Debug flag.
:- use_module(library(debug)).
:- debug(dh).

% Thread monitor.
:- use_module(library(swi_ide)).
:- prolog_ide(thread_monitor).

% DataHives example run.
:- use_module(dh(dh_test)).

