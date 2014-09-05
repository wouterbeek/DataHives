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
  request_query_nvpair(Request, agent_def, Selection, _NoAlias),
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
    http_location_by_id(dh_web_agents, Location)
  },
  html([
    % @see Post HTML form data using JSON:
    %      http://stackoverflow.com/questions/1184624/convert-form-data-to-js-object-with-jquery
    \html_requires(js(jquery)),
    \js_script({|javascript(_)||
      $.fn.serializeObject = function() {
        var o = {};
        var a = this.serializeArray();
        $.each(a, function() {
          if (o[this.name] !== undefined) {
            if (!o[this.name].push) {
              o[this.name] = [o[this.name]];
            }
            o[this.name].push(this.value || '');
          } else {
            o[this.name] = this.value || '';
          }
        });
        return o;
      };
      $(function() {
        var form = $("#agentDefinitions");
        form.submit(function(e) {
          e.preventDefault(); // Keep the form from submitting
          $.ajax({
            "contentType": "application/json",
            "data": JSON.stringify(form.serializeObject()),
            "dataType": "json",
            "type": "post",
            "url": form.attr('action')
          });
        });
      });
    |}),
    form([action=Location,id=agentDefinitions,method=post], [
        \agent_def_list(Selection, AgentDefNames),
        output(name=agent_def_descr,
            \html_list(SelectionDef, agent_pred, [ordered(false)])
        ),
        button([name=create_agent,type=submit],
            'Create agent'
        ),
        div(id=result, [])
    ])
  ]).


%! agent_def_list(?Selection, +AgentDefNames:list(atom))// is det.

agent_def_list(Selection, AgentDefNames) -->
  % Construct the `Selections` argument.
  {(  var(Selection)
  ->  Selections = []
  ;   Selections = [Selection]
  )},

  html_listbox(
    html('Agent definitions'),
    Selections,
    AgentDefNames,
    [name=agentDefinition]
  ).


%! agent_pred(+Pred)// is det.

agent_pred(Pred-Doc) --> !,
  html([b(Pred),': ',Doc]).
agent_pred(Pred) -->
  html(b(Pred)).

