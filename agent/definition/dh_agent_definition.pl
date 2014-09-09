:- module(
  dh_agent_definition,
  [
    dh_agent_definition/2, % +Request:list(nvpair)
                           % +HtmlStyle
    dh_agent_definition_db/1, % ?AgentDefinition:url
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
:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/js_write)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(request_ext)).
:- use_module(pl(pl_log)).

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdfs_build)).

:- use_module(dh_core(dh_generics)).
:- use_module(dh_web(dh_web_generics)).

:- dynamic(agent_definition_db0/2).
:- multifile(agent_definition_db0/2).



% GET text/html *
dh_agent_definition(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, AgentDefinitionLocation),
  http_absolute_uri(dh_agent_definition(.), AgentDefinitionLocation), !,
  http_absolute_uri(dh_agent(.), AgentLocation),
  http_absolute_location(sparql(.), SparqlLocation, []),
  reply_html_page(
    HtmlStyle,
    \dh_agent_definition_head(html('')),
    \dh_body([
      form(class=['pure-form','pure-form-stacked'],
        fieldset([
          legend('Agent definitions'),
          div(class='pure-g', [
            div([
              class=['pure-u-1','pure-u-md-1-3'],
              id=agentDefinitionsContainer
            ], []),
            div([
              class=['pure-u-1','pure-u-md-1-3'],
              id=agentDefinitionContainer
            ], [])
          ]),
          button(
            [
              class=['pure-button','pure-button-primary'],
              id=createBtn,
              type=submit
            ],
            ['Create agent']
          )
        ])
      ),
      \js_script({|javascript(AgentLocation,AgentDefinitionLocation,SparqlLocation)||
function indexString(index) {
  switch (index) {
    case 0:
      return "NAVIGATE";
    case 1:
      return "ACT";
    case 2:
      return "COMMUNICATE";
    case 3:
      return "EVALUATE";
    case 4:
      return "END-OF-LIFE";
  }
}
function pairsToDescriptionList(data) {
  var string = "<dl>";
  var index = 0;
  $.each(data, function(term, description) {
    string += "<dt>[" + indexString(index++) + "] " + term + "</dt><dd>" + description + "</dd>";
  });
  return string + "</dl>";
}
$(document).ready(function() {
  $.ajax({
    "dataType": "json",
    "success": function(data) {
      var select = $("<select class=\"pure-input-1-2\" id=aliases></select>").appendTo("#agentDefinitionsContainer");
      $.each(data, function(key, value) {
        select.append($("<option value=" + key + ">" + value + "</option>"));
      });
    },
    "type": "get",
    "url": AgentDefinitionLocation
  });
});
$("#agentDefinitionsContainer").on("change", "select", function() {
  $.ajax({
    "dataType": "json",
    "success": function(data) {
      var query = "SELECT ?label WHERE { <" +
          data["agentDefinition"] +
          "> <http://www.w3.org/2000/01/rdf-schema#label> ?label . }";
      $.ajax({
        "accepts": { "json": "application/sparql-results+json" },
        "data": [{ "name": "query", "value": query }],
        "dataType": "json",
        "success": function(data) {
          $("#createBtn").text(
            "Create " + data["results"]["bindings"][0]["label"]["value"] + " agent"
          );
        },
        "type": "get",
        "url": SparqlLocation
      });
      $("#agentDefinitionContainer").html(pairsToDescriptionList(data["predicates"]));
    },
    "type": "get",
    "url": $(this).find("option:selected").attr("value")
  });
});
$("#createBtn").click(function() {
  var agentDefinition = $("#agentDefinitionsContainer option:selected").attr("value");
  $.ajax({
    "contentType" : "application/json",
    "data": JSON.stringify({ "agentDefinition": agentDefinition }),
    "dataType": "json",
    "type": "post",
    "url": AgentLocation
  });
});
      |})
    ])
  ).
% GET text/html PATH
%
% Returns 404 if the given agent definition is not known.
dh_agent_definition(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, AgentDefinition), !,
  
  % Unknown agent definition.
  (   dh_agent_definition_db(AgentDefinition, _)
  ->  true
  ;   http_404([], Request)
  ),
  
  rdfs_label(AgentDefinition, Label),
  http_absolute_uri(dh_agent(.), AgentLocation),
  reply_html_page(
    HtmlStyle,
    \dh_agent_definition_head([Label]),
    \dh_body([
      h1(['Specification of the ',Label,' agent']),
      div(
        [
          class=['pure-u-1','pure-u-md-1-3'],
          id=agentDefinitionContainer
        ], []),
      button(
        [
          class=['pure-button','pure-button-primary'],
          id=createBtn,
          type=submit
        ],
        ['Create ',Label,' agent']
      ),
      \js_script({|javascript(AgentDefinition,Label,AgentLocation)||
function indexString(index) {
  switch (index) {
    case 0:
      return "NAVIGATE";
    case 1:
      return "ACT";
    case 2:
      return "COMMUNICATE";
    case 3:
      return "EVALUATE";
    case 4:
      return "END-OF-LIFE";
  }
}
function pairsToDescriptionList(data) {
  var string = "<dl>";
  var index = 0;
  $.each(data, function(term, description) {
    string += "<dt>[" + indexString(index++) + "] " + term + "</dt><dd>" + description + "</dd>";
  });
  return string + "</dl>";
}
$(document).ready(function() {
  $.ajax({
    "dataType": "json",
    "success": function(data) {
      $("#agentDefinitionContainer").html(pairsToDescriptionList(data["predicates"]));
    },
    "type": "get",
    "url": AgentDefinition
  });
});
$("#createBtn").click(function() {
  $.ajax({
    "contentType" : "application/json",
    "data": JSON.stringify({ "agentDefinition": AgentDefinition }),
    "dataType": "json",
    "type": "post",
    "url": AgentLocation
  });
});
      |})
    ])
  ).
% GET application/json *
% Returns type/label pairs in JSON format.
% This can e.g. be used to populate a <select> element in HTML.
dh_agent_definition(Request, _):-
  cors_enable,
  request_filter(Request, get, _/json, Root),
  http_absolute_uri(dh_agent_definition(.), Root), !,
  aggregate_all(
    set(AgentDefinition-Label),
    (
      agent_definition_db0(AgentDefinition, _),
      rdfs_label(AgentDefinition, Label)
    ),
    Pairs
  ),
  dict_create(Dict, agent_definitions, Pairs),
  reply_json_dict(Dict).
% GET application/json PATH
dh_agent_definition(Request, _):-
  cors_enable,
  request_filter(Request, get, _/json, AgentDefinition),
  agent_definition_db0(AgentDefinition, Predicates1),
  def_pairs(Predicates1, Predicates2),
  dict_create(Pairs, json, Predicates2),
  dict_create(Dict, json, [agentDefinition-AgentDefinition,predicates-Pairs]),
  reply_json_dict(Dict).


%! dh_agent_definition_db(+AgentDefinition:url) is semidet.
%! dh_agent_definition_db(-AgentDefinition:url) is nondet.

dh_agent_definition_db(AgentDefinition):-
  rdfs_individual_of(AgentDefinition, dh:'AgentDefinition').


%! dh_agent_definition_db(+AgentDefinition:url, +Predicates:list) is det.
%! dh_agent_definition_db(+AgentDefinition:url, -Predicates:list) is det.
%! dh_agent_definition_db(-AgentDefinition:url, -Predicates:list) is nondet.

dh_agent_definition_db(AgentDefinition, Predicates):-
  maplist(ground, [AgentDefinition,Predicates]), !,
  assert(agent_definition_db0(AgentDefinition, Predicates)).
dh_agent_definition_db(AgentDefinition, Predicates):-
  agent_definition_db0(AgentDefinition, Predicates),
  rdf_assert_instance(AgentDefinition, dh:'AgentDefinition', dh),
  rdfs_assert_subclass(AgentDefinition, dh:'Agent', dh).



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
