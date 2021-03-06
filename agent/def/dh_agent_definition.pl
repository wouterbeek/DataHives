:- module(
  dh_agent_definition,
  [
    dh_agent_definition/1, % ?AgentDefinition:url
    dh_agent_definition/2, % ?AgentDefinition:url
                           % ?Predicates:list
    dh_agent_definition_rest/1, % +Request:list(nvpair)
    dh_agent_definition_rest_path/1, % +Request:list(nvpair)
    register_dh_agent_definition/2 % +AgentDefinitionName:atom
                                   % +Predicates:list
  ]
).

/** <module> DataHives: Agent Definition

Implements agent definitions in DataHives.

@author Wouter Beek
@version 2014/09, 2014/11-2014/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/js_write)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).

:- use_module(pl(pl_log)).

:- use_module(plDcg(dcg_atom)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generics)).

:- use_module(plHttp(request_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- use_module(dh(core/dh_generics)).
:- use_module(dh(web/dh_web_generics)).

:- dynamic(agent_definition0/2).
:- multifile(agent_definition0/2).

:- dynamic(http:location/3).
:- multifile(http:location/3).

http:location(dh_agent_definition, dh('AgentDefinition'), []).

:- http_handler(
     dh_agent_definition(.),
     dh_agent_definition_rest,
     [id(dhAgentDef),prefix,priority(-1)]
   ).





%! dh_agent_definition(+AgentDefinition:url) is semidet.
%! dh_agent_definition(-AgentDefinition:url) is nondet.

dh_agent_definition(AgentDefinition):-
  rdfs_individual_of(AgentDefinition, dho:'AgentDefinition').


%! dh_agent_definition(+AgentDefinition:url, -Predicates:list) is det.
%! dh_agent_definition(-AgentDefinition:url, -Predicates:list) is nondet.

dh_agent_definition(AgentDefinition, Predicates):-
  agent_definition0(AgentDefinition, Predicates).


% GET html *
dh_agent_definition_rest(Request):-
  cors_enable,
  request_filter(Request, get, _/html, AgentDefinitionLocation),
  http_absolute_uri(dh_agent_definition(.), AgentDefinitionLocation), !,
  http_absolute_uri(dh_agent(.), AgentLocation),
  http_absolute_location(sparql(.), SparqlLocation, []),
  user:current_html_style(HtmlStyle),
  reply_html_page(
    HtmlStyle,
    \dh_agent_definition_head([]),
    \dh_body([
      form(
        [
          action=AgentLocation,
          class=['pure-form','pure-form-stacked'],
          id=agentDefinitionForm,
          method=post
        ],
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
function showPredicates(predicates) {
  var string = "<dl>";
  var prefixes = ["NAVIGATE","ACT","COMMUNICATE","EVALUATE","END-OF-LIFE"];
  $.each(predicates, function(index, element) {
    string += "<dt>[" + prefixes[index] + "] " + element["predicate"] +
              "</dt><dd>" + element["documentation"] + "</dd>";
  });
  return string + "</dl>";
}
$(document).ready(function() {
  $.ajax({
    "dataType": "json",
    "success": function(data) {
      var select = $("<select class=\"pure-input-1-2\" id=aliases></select>")
          .appendTo("#agentDefinitionsContainer");
      $.each(data["agentDefinitions"], function(index, element) {
        select.append($("<option value=" + element["agentDefinition"] +
            ">" + element["label"] + "</option>"));
      });
    },
    "type": "get",
    "url": AgentDefinitionLocation
  });
});
// Replace the outdated HTML5 <form> encoding practice with a modern REST+JSON approach.
$("#agentDefinitionForm").submit(function(event) {
  event.preventDefault(); // Keep the form from submitting
  $.ajax({
    "contentType": "application/json",
    "data": JSON.stringify({
        "agentDefinition":
        $("#agentDefinitionsContainer option:selected").attr("value")
    }),
    "dataType": "json",
    // Reuse <form> attribute: method -> type.
    "type": $(event.currentTarget).attr("method"),
    // Reuse <form> attribute: action -> url.
    "url": $(event.currentTarget).attr("action")
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
      $("#agentDefinitionContainer").html(showPredicates(data["predicates"]));
    },
    "type": "get",
    "url": $(this).find("option:selected").attr("value")
  });
});
      |})
    ])
  ).
% GET json *
% Returns type/label pairs in JSON format.
% This can e.g. be used to populate a <select> element in HTML.
dh_agent_definition_rest(Request):-
  cors_enable,
  request_filter(Request, get, _/json, Root),
  http_absolute_uri(dh_agent_definition(.), Root), !,
  findall(
    json{agentDefinition:AgentDefinition, label:Label},
    (
      dh_agent_definition(AgentDefinition),
      rdfs_label(AgentDefinition, Label)
    ),
    Dicts
  ),
  reply_json_dict(json{agentDefinitions:Dicts}).


% GET html PATH
%
% Returns 404 if the given agent definition is not known.
dh_agent_definition_rest_path(Request):-
  cors_enable,
  request_filter(Request, get, _/html, AgentDefinition), !,

  % Unknown agent definition.
  (   dh_agent_definition(AgentDefinition)
  ->  true
  ;   http_404([], Request)
  ),

  rdfs_label(AgentDefinition, Label),
  http_absolute_uri(dh_agent(.), AgentLocation),
  user:current_html_style(HtmlStyle),
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
function showPredicates(predicates) {
  var string = "<dl>";
  var prefixes = ["NAVIGATE","ACT","COMMUNICATE","EVALUATE","END-OF-LIFE"];
  $.each(predicates, function(index, element) {
    string += "<dt>[" + prefixes[index] + "] " + element["predicate"] +
              "</dt><dd>" + element["documentation"] + "</dd>";
  });
  return string + "</dl>";
}
$(document).ready(function() {
  $.ajax({
    "dataType": "json",
    "success": function(data) {
      $("#agentDefinitionContainer").html(showPredicates(data["predicates"]));
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
% GET json PATH
dh_agent_definition_rest_path(Request):-
  cors_enable,
  request_filter(Request, get, _/json, AgentDefinition),
  agent_definition0(AgentDefinition, Preds),
  def_pairs(Preds, PredDicts),
  reply_json_dict(
    json{agentDefinition:AgentDefinition, predicates:PredDicts}
  ).


%! register_dh_agent_definition(
%!   +AgentDefinitionName:atom,
%!   +Predicates:list
%! ) is det.

register_dh_agent_definition(Name1, Predicates):-
  % Construct the agent definition-denoting IRI.
  dcg_phrase(atom_capitalize, Name1, Name2),
  atomic_list_concat(['Agent',Name2], '/', Path),
  rdf_global_id(dh:'', Prefix),
  uri_normalized(Path, Prefix, AgentDefinition),

  % rdfs:label
  rdfs_assert_label(AgentDefinition, Name1, dho),

  % Register predicates in Prolog DB.
  assert(agent_definition0(AgentDefinition, Predicates)),

  % rdf:type
  rdf_assert_instance(AgentDefinition, dho:'AgentDefinition', dho),

  % rdfs:subClassOf
  rdfs_assert_subclass(AgentDefinition, dho:'Agent', dho).





% HELPERS

def_pairs([], []):- !.
def_pairs([Predicate0-Documentation|T], [Dict|Dicts]):- !,
  with_output_to(atom(Predicate), write_canonical_blobs(Predicate0)),
  dict_create(Dict, json, [predicate-Predicate,documentation-Documentation]),
  def_pairs(T, Dicts).
def_pairs([Predicate0|T], [Dict|Dicts]):-
  with_output_to(atom(Predicate), write_canonical_blobs(Predicate0)),
  dict_create(Dict, json, [predicate-Predicate,documentation-""]),
  def_pairs(T, Dicts).


dh_agent_definition_head(Substrings) -->
  html(\dh_head(['Agent definition'|Substrings])).

