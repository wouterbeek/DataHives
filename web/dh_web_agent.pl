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
@version 2013/09-2013/10, 2014/02, 2014/04, 2014/06
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(plHtml(html_table)).

:- use_module(dh_core(dh_population)).



dh_web_agent(_, Style):-
  findall(
    [Alias,Id,Status,CPU_Time],
    (
      agent_thread(Id),
      thread_property(Id, alias(Alias)),
      thread_property(Id, status(Status)),
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
  reply_html_page(
    Style,
    title('DataHives - Agents'),
    html(
      \html_table(
        html('Overview of the currently running agents in DataHives.'),
        [['Alias','Id','Status','CPU time']|Rows],
        [header_row(true),index(true)]
      )
    )
  ).

