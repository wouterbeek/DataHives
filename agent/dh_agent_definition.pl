:- module(
  dh_agent_definition,
  [
    dh_agent_definition/2, % +Request:list(nvpair)
                           % +HtmlStyle
    dh_agent_definition_db/2 % ?AgentDefinition:url
                             % ?Predicates:list
  ]
).

/** <module> DataHives: Agent Definition

Implements agent definitions in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/js_write)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(request_ext)).
:- use_module(pl(pl_log)).

:- use_module(dh_web(dh_web_generics)).

:- dynamic(agent_definition_db0/2).
:- multifile(agent_definition_db0/2).



% GET text/html PATH
dh_agent_definition(Request, HtmlStyle):-
  request_filter(Request, get, _-html, AgentDefinition), !,
  rdfs_label(AgentDefinition, Label),
  reply_html_page(
    HtmlStyle,
    \dh_agent_definition_head([Label]),
    html([
      \html_requires(js(jquery)),
      div(id=agentDefinitionsContainer, []),
      \js_script({|javascript(Location)||
$(document).ready(function() {
  $.ajax({
    "dataType": "json",
    "success": function(data) {
        var select = $("<select id=aliases></select>")
            .appendTo("#agentDefinitionsContainer");
        $.each(data, function(key, value) {
          select
              .append($("<option value=" + key + ">" + value + "</option>"));
        });
      },
    "url": Location
  });
});
      |})
    ])
  ).
% GET text/html *
dh_agent_definition(Request, HtmlStyle):-
  request_filter(Request, get, _-html, _), !,
  http_location_by_id(dh_agent, AgentLocation),
  http_location_by_id(dh_agent_definition, AgentDefinitionLocation),
  reply_html_page(
    HtmlStyle,
    \dh_agent_definition_head([]),
    html([
      \html_requires(js(jquery)),
      div(id=agentDefinitionsContainer, []),
      div(id=agentDefinitionContainer, []),
      button(id=createBtn, ['Create agent']),
      \js_script({|javascript(AgentLocation,AgentDefinitionLocation)||
function pairsToDescriptionList(data) {
  var string = "<dl>";
  $.each(data, function(term, description) {
    string += "<dt>" + term + "</dt><dd>" + description + "</dd>";
  });
  return string + "</dl>";
}
$(document).ready(function() {
  $.ajax({
    "dataType": "json",
    "success": function(data) {
        var select = $("<select id=aliases></select>")
            .appendTo("#agentDefinitionsContainer");
        $.each(data, function(key, value) {
          select
              .append($("<option value=" + key + ">" + value + "</option>"));
        });
      },
    "url": AgentDefinitionLocation
  });
});
$("#agentDefinitionsContainer").on("change", "select", function() {
  $.ajax({
    "data": {"alias": $(this).find("option:selected").attr("value")},
    "dataType": "json",
    "success": function() {
        $("#createBtn").text("Create " + data["alias"] + " agent");
        $("#agentDefinitionContainer")
            .html(pairsToDescriptionList(data["description"]));
      },
    "url": AgentDefinitionLocation
  });
});
$("#createBtn").click(function() {
  $.ajax({
    "data": {
        "alias": $("#agentDefinitionsContainer option:selected").attr("value")
    },
    "dataType": "json",
    "url": AgentLocation
  });
});
      |})
    ])
  ).
% GET application/json PATH
% Returns type/label pairs in JSON format.
% This can e.g. be used to populate a <select> element in HTML.
dh_agent_definition(Request, _):-
  request_filter(Request, get, _-json, AgentDefinition, _-_), !,
  aggregate_all(
    set(AgentDefinition-AgentDefinition),
    agent_definition_db0(AgentDefinition, _),
    Pairs
  ),
  dict_create(Dict, agent_definitions, Pairs),
  reply_json_dict(Dict).
% GET application/json *
dh_agent_definition(Request, _):-
  request_filter(Request, get, _-json, AgentDefinition),
  agent_definition_db0(AgentDefinition, Predicates1),
  def_pairs(Predicates1, Predicates2),
  dict_create(Pairs, json, Predicates2),
  dict_create(Dict, json, [alias-AgentDefinition,description-Pairs]),
  reply_json_dict(Dict).


%! dh_agent_definition_db(+AgentDefinition:url, +Predicates:list) is det.
%! dh_agent_definition_db(+AgentDefinition:url, -Predicates:list) is det.
%! dh_agent_definition_db(-AgentDefinition:url, -Predicates:list) is nondet.

dh_agent_definition_db(AgentDefinition, Predicates):-
  maplist(nonvar, [AgentDefinition,Predicates]), !,
  assert(agent_definition_db0(AgentDefinition, Predicates)).
dh_agent_definition_db(AgentDefinition, Predicates):-
  agent_definition_db0(AgentDefinition, Predicates).



% Helpers

def_pairs([], []):- !.
def_pairs([X1-Y|T1], [X2-Y|T2]):- !,
  with_output_to(atom(X2), write_canonical_blobs(X1)),
  def_pairs(T1, T2).
def_pairs([H1|T1], [H2-""|T2]):-
  with_output_to(atom(H2), write_canonical_blobs(H1)),
  def_pairs(T1, T2).


dh_agent_definition_head(Dcg) -->
  html(\dh_head(['Agent definition',Dcg])).

