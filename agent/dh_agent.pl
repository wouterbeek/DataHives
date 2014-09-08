:- module(
  dh_agent,
  [
    dh_agent/2 % +Request:list(nvpair)
               % +HtmlStyle
  ]
).

/** <module> DataHives Agent

Interface to agents in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(request_ext)).

:- use_module(plHtml(html)).
:- use_module(plHtml(html_pl_term)).
:- use_module(plHtml(html_table)).

:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_core(dh_population)).
:- use_module(dh_web(dh_web_generics)).



% GET html PATH
dh_agent(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, Agent),
  \+ http_absolute_uri(dh_agent(.), Agent), !,
  findall(
    [Name,Value],
    dh_agent_property(Agent, Name, Value),
    Rows
  ),
  rdfs_label(Location, Label),
  reply_html_page(
    HtmlStyle,
    \dh_agent_head([Label]),
    html(
      \html_table(
        html(['Overview of agent ', \html_link(Location-Label), '.']),
        Rows,
        [header_row(false),index(false)]
      )
    )
  ).
% GET html *
dh_agent(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, _), !,
  findall(
    [Alias,Agent,Status,CPU_Time,Cycles,Steps,Effectiveness],
    (
      dh_agent_thread(Agent),
      thread_property(Agent, alias(Alias)),
      thread_property(Agent, status(Status)),
      dh_agent_age(Agent, Age),
      dh_agent_cycles(Agent, Cycles),
      dh_agent_steps(Agent, Steps),
      Effectiveness is Steps / Age,
      (   memberchk(Status, [exception(_),false])
      ->  CPU_Time = 0
      ;   thread_statistics(Agent, cputime, CPU_Time)
      )
    ),
    Rows
  ),
  number_of_agents(N),
  reply_html_page(
    HtmlStyle,
    \dh_head(['Agents']),
    html(
      \html_table(
        html([
          'Overview of the ',
          \html_pl_term(dh, N),
          ' currently running agents in DataHives.'
        ]),
        [['Alias','ThreadId','Status','CPU time','Cycles','Steps','Effectiveness']|Rows],
        [header_row(true),index(true)]
      )
    )
  ).
% POST json PATH
dh_agent(Request, _):-
  cors_enable,
  request_filter(Request, post, _-json, AgentDefinition), !,
  dh_agent_create(AgentDefinition, graph(visum)),
  reply_json_dict(json{}).



% Helpers

dh_agent_head(Dcg) -->
  html(\dh_head(['Agent',Dcg])).

