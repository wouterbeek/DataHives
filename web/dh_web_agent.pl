:- module(dh_web_agent, []).

/** <module> DataHives Web agent

Web-based interface to agents in DataHives.

@author Wouter Beek
@version 2013/09-2013/10, 2014/02, 2014/04
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(plHtml(html_table)).

:- use_module(plServer(web_modules)).

http:location(dh_web, root(dh), []).
:- http_handler(dh_web(agent), dh_web_agent, []).

user:web_module('DH Agent', dh_web_agent).



dh_web_agent(_Request):-
  findall(
    [Alias,Id,Status,CPU_Time],
    (
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
    app_style,
    title('DataHives - Agents'),
    html(
      \html_table(
        [header_row(true),index(true)],
        html('Overview of the currently running agents in DataHives.'),
        [['Alias','Id','Status','CPU time']|Rows]
      )
    )
  ).

