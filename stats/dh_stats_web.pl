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

:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_path)).
:- use_module(library(http/js_write)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(request_ext)).

:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_web(dh_web_generics)).



% GET html *
dh_stats_web(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, Root),
  http_absolute_uri(dh_stats(.), Root), !,
  http_absolute_location(sparql(.), SparqlLocation, []),
  rdf_current_prefix(dho, DhoNamespace),
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
              option(value=agent, agent),
              option(value=agentDefinition, 'agent definition'),
              option(value=population, population)
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
      \js_script({|javascript(DhoNamespace,RdfsNamespace,SparqlLocation)||
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
      |})
    ])
  ).



% Helpers

dh_stats_head(Substrings) -->
  html(\dh_head(['Statistics'|Substrings])).

