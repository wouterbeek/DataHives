% Standalone startup file for DataHives.

:- [load].

:- use_module(library(http/http_server)).

:- start_server([port(8888)]).

% DataHives HTTP root location.
:- dynamic(http:location/3).
:- multifile(http:location/3).

http:location(dh, root(dh), []).



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
