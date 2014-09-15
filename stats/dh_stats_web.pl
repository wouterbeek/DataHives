:- module(
  dh_stats_web,
  [
    dh_stats_web/2 % +Request:list(nvpair)
                   % +HtmlStyle
  ]
).

/** <module> DataHives Statistics: Web

Web-based front-end for statistics in DataHives.

~~~{.sparql}
PREFIX dho: <http://localhost.localdomain:3020/dh/ontology/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?property ?label WHERE {
  ?property rdfs:subPropertyOf dho:agentProperty .
  FILTER NOT EXISTS {
    ?property0 rdfs:subPropertyOf ?property .
    FILTER (?property0 != ?property)
  }
  ?property rdfs:label ?label .
}
~~~

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/js_write)).
:- use_module(library(lambda)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(request_ext)).

:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_stats(dh_stats_core)).
:- use_module(dh_web(dh_web_generics)).



% GET html *
dh_stats_web(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, Root),
  http_absolute_uri(dh_stats(.), Root), !,
  http_absolute_location(sparql(.), SparqlLocation, []),
  
  % @tbd Unify REST and RDFS classes.
  http_absolute_uri(dh_agent_definition(.), _AgentDefinitionLocation),
  http_absolute_uri(dh_agent(.), _AgentLocation),
  
  rdf_current_prefix(dho, DhoNamespace),
  rdf_current_prefix(rdf, RdfNamespace),
  rdf_current_prefix(rdfs, RdfsNamespace),
  reply_html_page(
    HtmlStyle,
    \dh_stats_head(['']),
    \dh_body([
      form(
        [action=Root,class=['pure-form'],id=statisticsForm,method=post],
        fieldset([
          legend('Perform measurement for statistics'),
          'Show',
          select(id=yProperties, []),
          'per',
          select(id=xProperties, []),
          'for',
          select(
            id=scopes,
            [
              option(value='Agent', agent),
              option(value='AgentDefinition', 'agent definition'),
              option(value='Population', population)
            ]
          ),
          select(id=subscopes, []),
          button(
            [
              class=['pure-button','pure-button-primary'],
              id=submitBtn,
              type=submit
            ],
            ['Submit']
          )
        ])
      ),
      \js_script({|javascript(DhoNamespace,RdfNamespace,RdfsNamespace,SparqlLocation)||
$(document).ready(function() {
  var query = "\
PREFIX dho: <" + DhoNamespace + ">\n\
PREFIX rdfs: <" + RdfsNamespace + ">\n\
SELECT ?property ?label\n\
WHERE {\n\
  ?property rdfs:subPropertyOf dho:agentProperty .\n\
  FILTER NOT EXISTS {\n\
    ?property0 rdfs:subPropertyOf ?property .\n\
    FILTER (?property0 != ?property)\n\
  }\n\
  ?property rdfs:label ?label .\n\
}\n";
  $.ajax({
    "accepts": { "json": "application/sparql-results+json" },
    "data": [
        { "name": "entailment", "value": "rdfs"  },
        { "name": "query",      "value": query   }
      ],
    "dataType": "json",
    "success": function(data) {
      $.each(data["results"]["bindings"], function(index, element) {
        $("#xProperties").append($("<option value=" +
            element["property"]["value"] + ">" +
            element["label"]["value"] + "</option>"));
        $("#yProperties").append($("<option value=" +
            element["property"]["value"] + ">" +
            element["label"]["value"] + "</option>"));
      });
    },
    "type": "get",
    "url": SparqlLocation
  });
});
$("#scopes").on("change", function() {
  var query = "\
PREFIX dho: <" + DhoNamespace + ">\n\
PREFIX rdf: <" + RdfNamespace + ">\n\
PREFIX rdfs: <" + RdfsNamespace + ">\n\
SELECT ?entry ?label\n\
WHERE {\n\
  ?entry rdf:type dho:" + $(this).find("option:selected").attr("value") + " .\n\
  ?entry rdfs:label ?label .\n\
}\n";
  $.ajax({
    "accepts": { "json": "application/sparql-results+json" },
    "data": [
        { "name": "entailment", "value": "rdfs"  },
        { "name": "query",      "value": query   }
      ],
    "dataType": "json",
    "success": function(data) {
      $("#subscopes").html("");
      $.each(data["results"]["bindings"], function(index, element) {
        $("#subscopes").append($("<option value=" +
            element["entry"]["value"] + ">" +
            element["label"]["value"] + "</option>"));
      });
    },
    "type": "get",
    "url": SparqlLocation
  });
});
// Replace the outdated HTML5 <form> encoding practice with a modern REST+JSON approach.
$("#statisticsForm").submit(function(event) {
  event.preventDefault(); // Keep the form from submitting
  $.ajax({
    "contentType": "application/json",
    "data": JSON.stringify({
        "yProperty":  $("#yProperties option:selected").attr("value"),
        "xProperty":  $("#xProperties option:selected").attr("value"),
        "scope":      $("#scopes option:selected").attr("value"),
        "subscope":   $("#subscopes option:selected").attr("value")
    }),
    "dataType": "json",
    // Reuse <form> attribute: method -> type.
    "type": $(event.currentTarget).attr("method"),
    // Reuse <form> attribute: action -> url.
    "url": $(event.currentTarget).attr("action")
  });
});
      |})
    ])
  ).
% POST json *
dh_stats_web(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, post, _/json, _), !,
  catch(
    (
      http_read_json_dict(Request, Dict),
      atom_string(YProperty, Dict.yProperty),
      atom_string(XPorperty, Dict.xProperty),
      atom_string(Scope, Dict.scope),
      atom_string(Subscope, Dict.subscope)
    ),
    Exception,
    throw(http_reply(bad_request(Exception)))
  ),
  dh_perform_measurement(YProperty, XProperty, Scope, Subscope),
  reply_json_dict(json{}).

% @tbd Allow other XProperties than time.
dh_perform_measurement(YProperty, _XProperty, Scope, Subscope):-
  scoped_agents(Scope, Subscope, Agents),
  dh_stats_loop(
    \Agent^Value^dh_agent_property(Agent, YProperty, Value),
    Agents,
    100, % @tbd Set this via UI if XProperty is time.
    _Dataset
  ).

% All agents belonging to the current population.
scoped_agents('Population', _, Agents):-
  aggregate_all(
    set(Agent),
    rdfs_individual_of(Agent, dho:'Agent'),
    Agents
  ).
% A specific/single agent.
scoped_agents('Agent', Agent, [Agent]):-
  nonvar(Agent), !.
% All agents (whatsoever).
scoped_agents('Agent', _, Agents):- !,
  aggregate_all(
    set(Agent),
    rdfs_individual_of(Agent, dho:'Agent'),
    Agents
  ).
% All agents of a specific agent definition.
scoped_agents('AgentDefinition', AgentDefinition, Agents):- !,
  aggregate_all(
    set(Agent),
    rdfs_individual_of(Agent, AgentDefinition),
    Agents
  ).



% Helpers

dh_stats_head(Substrings) -->
  html(\dh_head(['Statistics'|Substrings])).

