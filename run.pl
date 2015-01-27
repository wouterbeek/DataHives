% Standalone startup file for DataHives.



% Load modules.
:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.



% Start a plServer.

:- use_module(load_project).
:- load_subproject(dh, plServer).

:- use_module(plServer/app_server).
:- use_module(plServer/web_modules). % Web module registration.

:- start_app_server([port(8888)]).



% DataHives HTTP root location.
:- dynamic(http:location/3).
:- multifile(http:location/3).

http:location(dh, root(dh), []).



% Current HTML style set to plServer style.
user:current_html_style(menu_page).



% Register plServer Web modules.
:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).

% Register plTabular Web handler.
user:web_module(plTabular, rdf_tabular).

% Register DataHives Agent Web handler.
user:web_module('DH Agents', dhAgent).

% Register DataHives Agent Definition Web handler.
user:web_module('DH Agent Definitions', dhAgentDef).

% Register DataHives Graphic Web handler.
user:web_module('DH Agent Graphic', dh_agent_graphic).

% Register DataHives Statistics  Web handler.
user:web_module('DH Statistics', dh_stats_web).
