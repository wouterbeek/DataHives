:- module(
  dh_web_agent,
  [
    dh_web_agent/2 % +Request:list(nvpair)
                   % +Style:atom
  ]
).

/** <module> DataHives Web agent

Web-based interface to agents in DataHives.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02, 2014/04, 2014/06, 2014/08
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(plHtml(html_pl_term)).
:- use_module(plHtml(html_table)).

:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_core(dh_population)).



dh_web_agent(_, Style):-
  findall(
    [Alias,Id,Status,CPU_Time,Cycles,Steps,Effectiveness],
    (
      agent_thread(Agent),
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
        thread_statistics(Id, cputime, CPU_Time)
      )
    ),
    Rows
  ),
  number_of_agents(N),
  reply_html_page(
    Style,
    title('DataHives - Agents'),
    html(
      \html_table(
        html([
          'Overview of the ',
          \html_pl_term(dh, N),
          ' currently running agents in DataHives.'
        ]),
        [['Alias','Id','Status','CPU time','Cycles','Steps','Effectiveness']|Rows],
        [header_row(true),index(true)]
      )
    )
  ).

