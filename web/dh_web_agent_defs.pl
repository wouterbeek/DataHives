:- module(
  dh_web_agent_defs,
  [
    dh_web_agent_defs/2 % +Request:list(nvpair)
                        % +Style
  ]
).

/** <module> DataHives Web agent

Web-based interface to agents in DataHives.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02, 2014/04, 2014/06, 2014/08-2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/js_write)).

:- use_module(generics(request_ext)).

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

:- http_handler(dh_web(agentDefList), agentDefList, []).



%! agentDefList(+Request:list(nvpair)) is det.
% Returns type/label pairs in JSON format.
%
% This can be used to populate a <select> element in HTML.

agentDefList(_):-
  aggregate_all(
    set(Alias-Alias),
    dh:agent_definition(Alias, _),
    Pairs
  ),
  dict_create(Dict, agentDefs, Pairs),
  reply_json_dict(Dict).


dh_web_agent_defs(_, HtmlStyle):-
  reply_html_page(
    HtmlStyle,
    \dh_web_head(['Agent definitions']),
    \dh_agent_defs_body
  ).

dh_agent_defs_body -->
  {
    http_location_by_id(agentDefList, ListLocation)
  },
  html([
    \html_requires(js(jquery)),
    div(id=agentDefList, []),
    \js_script({|javascript(ListLocation)||
      function populateList(data) {
        var select = $("<select></select>").appendTo("#agentDefList");
        $.each(data, function(key, value) {
          select.append(
              $("<option value=" + key + ">" + value + "</option>")
          );
        });
      }
      $(document).ready(function() {
        $.ajax({ //getJSON
          "dataType": "json",
          "fail": function(data) {
              console.log("Failed to populate agent definition list.", data);
            },
          "success": populateList,
          "url" :ListLocation
        });
      });
    |})
  ]).

/*
dh_agent_defs_body -->
  {
    % Retrieve the location of the processor for this form's contents.
    http_location_by_id(agentDefList, Location)
  },
  html([
    % @see Post HTML form data using JSON:
    %      http://stackoverflow.com/questions/1184624/convert-form-data-to-js-object-with-jquery
    \html_requires(js(jquery)),
    \js_script({|javascript(AgentDefList)||
// When the DOM loads we insert a listbox of agent definitions,
// and we register the behavior of making a selection in that list.
$(document).ready(function() {
  $("#agentDefList").html(listBox($.ajax({
    "accept": "application/json",
    "type": "get",
    "url": AgentDefList
  })));
  ("#agentDefList select").change(function() {
    var btn = $("#agentDefBtn");
    btn.disabled = false;
    btn.html("Add a/an " + $(this).text() + "agent");
    $("#agentDefContainer").html(agentDefBox($.ajax({
      "accept": "application/json",
      "data": JSON.stringify({ "agentDef": $(this).attr("value") }), // jQuery will add this as the URL query string.
      "type": "get",
      "url": // REST access to agent definition.
    })));
  });
});
function listBox(content) {
}
// Replace the outdated HTML5 <form> encoding practice with a modern
// REST+JSON approach.
$(function() {
  var form = $("#agentDefs");
  form.submit(function(e) {
    e.preventDefault(); // Keep the form from submitting
    $.ajax({
      "contentType": "application/json",
      "data": JSON.stringify({
          "agentDef": $("#agentDef option:selected").attr("value")
      }),
      "dataType": "json",
      "type": form.attr("method")   // Reuse <form> attribute.
      "url": form.attr("action")   // Reuse <form> attribute.
    });
  });
  function changeAgentDefName() {
    $.ajax({
      "data": {
          "agentDefName": $("#agentDefName option:selected").text()
      },
      "type": "get",
      "url": Location
    });
  }
  $(function() {
    var form = $("#agentDefs");
    form.submit(function(e) {
      e.preventDefault(); // Keep the form from submitting
      $.ajax({
        "contentType": "application/json",
        "data": JSON.stringify({
            "agentDefName": $("#agentDefName option:selected").text()
        }),
        "dataType": "json",
        "type": "post",
        "url": form.attr('action')
      });
    });
  });
|}),
    form(
      [
        action=Location,
        class=['pure-form','pure-form-stacked'],
        id=agentDefs,
        method=post
      ],
      fieldset([
        legend('Agent definitions'),
        div(class='pure-g', [
          div([class=['pure-u-1','pure-u-md-1-3'],id=agentDefList], []),
          div([class=['pure-u-1','pure-u-md-1-3'],id=agentDefDescr], [])
        ]),
        div(class=['pure-u-1','pure-u-md-1-3'],
          button(
            [
              class=['pure-button','pure-button-primary'],
              disabled=true,
              id=agentDefBtn,
              type=submit
            ],
            'Create an agent'
          )
        )
      ])
    )
  ]).
*/
