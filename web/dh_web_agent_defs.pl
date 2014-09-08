:- module(
  dh_web_agent_defs,
  [
    dh_web_agent_defs/2 % +Request:list(nvpair)
                        % +HtmlStyle
  ]
).

/** <module> DataHives Web agent

Web-based interface to agents in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/js_write)).

:- use_module(generics(request_ext)).
:- use_module(pl(pl_log)).

:- use_module(dh_web(dh_web_generics)).

% JS
:- if(predicate_property(user:debug_mode, visible)).
  :- html_resource(
       js(jquery),
       [requires([js('jquery-debug-2.1.1.js')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       js(jquery),
       [requires([js('jquery-min-2.1.1.js')]),virtual(true)]
     ).
:- endif.

:- http_handler(dh_web(agentAliases), agentAliases, []).
:- http_handler(dh_web(agentDescription), agentDescription, []).
:- http_handler(dh_web(createAgent), createAgent, []).



%! agentAliases(+Request:list(nvpair)) is det.
% Returns type/label pairs in JSON format.
%
% This can be used to populate a <select> element in HTML.

agentAliases(_):-
  cors_enable,
  aggregate_all(
    set(Alias-Alias),
    dh:agent_definition(Alias, _),
    Pairs
  ),
  dict_create(Dict, aliases, Pairs),
  reply_json_dict(Dict).


agentDescription(Request):-
  cors_enable,
  request_query_nvpair(Request, alias, Alias),
  dh:agent_definition(Alias, Def),
  def_pairs(Def, Pairs0),
  dict_create(Pairs, json, Pairs0),
  dict_create(Dict, json, [alias-Alias,description-Pairs]),
  reply_json_dict(Dict).

def_pairs([], []):- !.
def_pairs([X1-Y|T1], [X2-Y|T2]):- !,
  with_output_to(atom(X2), write_canonical_blobs(X1)),
  def_pairs(T1, T2).
def_pairs([H1|T1], [H2-""|T2]):-
  with_output_to(atom(H2), write_canonical_blobs(H1)),
  def_pairs(T1, T2).


dh_web_agent_defs(_, HtmlStyle):-
  reply_html_page(
    HtmlStyle,
    \dh_web_head(['Agent definitions']),
    \dh_agent_defs_body
  ).

dh_agent_defs_body -->
  {
    http_location_by_id(agentAliases, AliasesLocation),
    http_location_by_id(agentDescription, DescriptionLocation),
    http_location_by_id(createAgent, CreateLocation)
  },
  html([
    \html_requires(js(jquery)),
    div(id=aliasesContainer, []),
    div(id=descriptionContainer, []),
    button(id=createBtn, ['Create agent']),
    \js_script({|javascript(AliasesLocation,DescriptionLocation,CreateLocation)||
function changeDescription(data) {
  $("#createBtn").text("Create " + data["alias"] + " agent");
  $("#descriptionContainer").html(descriptionList(data["description"]));
}
function descriptionList(data) {
  var string = "<dl>";
  $.each(data, function(predicate, documentation) {
    string += "<dt>" + predicate + "</dt><dd>" + documentation + "</dd>";
  });
  return string + "</dl>";
}
function populateAliases(data) {
  var select = $("<select id=aliases></select>")
      .appendTo("#aliasesContainer");
  $.each(data, function(key, value) {
    select.append($("<option value=" + key + ">" + value + "</option>"));
  });
}
$(document).ready(function() {
  $.ajax({
    "dataType": "json",
    "fail": function(data) {
        console.log("Failed to populate agent definition list.", data);
      },
    "success": populateAliases,
    "url": AliasesLocation
  });
});
$("#aliasesContainer").on("change", "select", function() {
  $.ajax({
    "data": {"alias": $(this).find("option:selected").attr("value")},
    "dataType": "json",
    "fail": function(data) {
        console.log("Failed to change agent definition.", data);
      },
    "success": changeDescription,
    "url": DescriptionLocation
  });
});
$("#createBtn").click(function() {
  $.ajax({
    "data": {"alias": $("#aliasesContainer option:selected").attr("value")},
    "dataType": "json",
    "fail": function(data) {console.log("Failed to create agent.", data);},
    "url": CreateLocation
  });
});
    |})
  ]).
