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
:- use_module(library(http/js_write)).

:- use_module(generics(request_ext)).

:- use_module(plHtml(html_listbox)).
:- use_module(plHtml(html_list)).
:- use_module(plHtml(html_pl_term)).

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



dh_web_agent_defs(Request, HtmlStyle):-
  request_query_nvpair(Request, agentDefName, Selection, _NoAlias),
  reply_html_page(
    HtmlStyle,
    \dh_web_head(['Agent definitions']),
    \dh_agent_defs_table(Selection)
  ).


%! dh_agent_defs_table(?Selection)// is det.

dh_agent_defs_table(Selection) -->
  {
    aggregate_all(
      set(Alias-AgentDef),
      dh:agent_definition(Alias, AgentDef),
      Pairs
    ),
    pairs_keys(Pairs, AgentDefNames),

    % Make sure the agent alias exists, if given.
    (   nonvar(Selection)
    ->  memberchk(Selection-SelectionDef, Pairs)
    ;   SelectionDef = []
    ),

    % Retrieve the location of the processor for this form's contents.
    http_location_by_id(dh_web_agent_defs, Location)
  },
  html([
    % @see Post HTML form data using JSON:
    %      http://stackoverflow.com/questions/1184624/convert-form-data-to-js-object-with-jquery
    \html_requires(js(jquery)),
    \js_script({|javascript(Location)||
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
          div(class=['pure-u-1','pure-u-md-1-3'],
            \agent_def_list(Selection, AgentDefNames)
          ),
          div(class=['pure-u-1','pure-u-md-1-3'],
            \agent_def_descr(SelectionDef)
          )
        ]),
        div(class=['pure-u-1','pure-u-md-1-3'],
          \agent_def_create(Selection)
        )
      ])
    )
  ]).


%! agent_def_create(+AgentDefinitionName:atom) -->

agent_def_create(AgentDefName) -->
  {var(AgentDefName)}, !,
  agent_def_create('Create an agent', true).
agent_def_create(AgentDefName) -->
  {format(atom(Label), 'Create a ~a agent', [AgentDefName])},
  agent_def_create(Label, false).

agent_def_create(Label, Disabled) -->
  html(
    button(
      [
        class=['pure-button','pure-button-primary'],
	disabled=Disabled,
        name=createAgent,
        type=submit
      ],
      Label
    )
  ).


%! agent_def_descr(+AgentDef:list)// is det.

agent_def_descr(Preds) -->
  html(
    div(name=agentDefinitionDescription,
      \html_list(Preds, agent_pred, [ordered(false)])
    )
  ).


%! agent_def_list(?Selection, +AgentDefNames:list(atom))// is det.

agent_def_list(Selection, AgentDefNames) -->
  % Construct the `Selections` argument.
  {(  var(Selection)
  ->  Selections = []
  ;   Selections = [Selection]
  )},
  html_listbox(
    Selections,
    AgentDefNames,
    [class='pure-input-1-3',id=agentDefName,onChange='changeAgentDefName();']
  ).


%! agent_pred(+Pred)// is det.

agent_pred(Pred-Doc) --> !,
  html([\html_pl_term(dh,Pred),': ',Doc]).
agent_pred(Pred) -->
  html(\html_pl_term(dh,Pred)).

