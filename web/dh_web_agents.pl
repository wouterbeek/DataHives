:- module(
  dh_web_agents,
  [
    agents_table//0,
    dh_web_agents/2 % +Request:list(nvpair)
                    % +Style
  ]
).

/** <module> DataHives Web agent

Web-based interface to agents in DataHives.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02, 2014/04, 2014/06, 2014/08-2014/09
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- use_module(plHtml(html_pl_term)).
:- use_module(plHtml(html_table)).

:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_core(dh_population)).
:- use_module(dh_web(dh_web_generics)).

:- http_handler(dh_web(agents), dh_web_agents, [prefix]).



% POST
dh_web_agents(Request, _):-
  memberchk(method(post), Request), !,

  % Extract the agent definition value.
  catch(
    (
      http_read_json_dict(Request, D),
      atom_string(AgentDefinitionName, D.agentDefName)
    ),
    E,
    throw(http_reply(bad_request(E)))
  ),
  dh_create_agent(AgentDefinitionName, graph(visum)),

  reply_json(json{}, [status(200)]).
% GET
dh_web_agents(_, HtmlStyle):-
  reply_html_page(
    HtmlStyle,
    \dh_web_head(['Agents']),
    \agents_table
  ).


agents_table -->
  {
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
        (
          memberchk(Status, [exception(_),false])
        ->
          CPU_Time = 0
        ;
          thread_statistics(Agent, cputime, CPU_Time)
        )
      ),
      Rows
    ),
    number_of_agents(N)
  },
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
  ).

