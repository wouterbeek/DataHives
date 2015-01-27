:- module(conf_dh, []).

/*
:- use_module(library(settings)).

:- set_setting_default(http:public_host, 'localhost.localdomain').
:- set_setting_default(http:public_port, setting(http:port)).
*/



% DataHives HTTP root location.
:- dynamic(http:location/3).
:- multifile(http:location/3).

http:location(dh, cliopatria(dh), []).



% Current HTML style set to ClioPatria default.
user:current_html_style(cliopatria(default)).



% ClioPatria menu items.
:- multifile(cliopatria:menu_item/2).

% Register plTabular Web handler.
cliopatria:menu_item(600=dh/plTabular, 'plTabular').

% Register DataHives Agent Web handler.
cliopatria:menu_item(600=dh/dhAgent, 'DH Agent').

% Register DataHives Agent Definition Web handler.
cliopatria:menu_item(600=dh/dhAgentDef, 'DH Agent Definition').

% Register DataHives Graphic Web handler.
cliopatria:menu_item(600=dh/dhGraphic, 'DH Graphic').

% Register DataHives Statistics  Web handler.
cliopatria:menu_item(600=dh/dhStats, 'DH Statistics').



% Load modules.
:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded('../debug').
:- else.
  :- ensure_loaded('../load').
:- endif.



% In ClioPatria mode the plServer is not used, but the HTML resources
% it defines are needed.
:- ensure_loaded(plServer(style)).
