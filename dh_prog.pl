:- module(
  dh_prog,
  [
    dh_prog/2, % +Name:atom
               % :Goal
    dh_progs/0,
    dh_progs_web/1 % -DOM:list
  ]
).

/** <module> DH_PROG

@author Wouter Beek
@version 2013/09
*/

:- use_module(generics(print_ext)).
:- use_module(html(html_table)).
:- use_module(library(semweb/rdf_db)).
:- use_module(server(web_console)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dh, 'http://www.dh.com/agent#').

:- register_module(dh_prog).

:- meta_predicate(dh_prog(+,:)).



dh_prog(Name, Goal):-
  \+ ((
    thread_property(Id, alias(Name)),
    thread_property(Id, status(running))
  )),
  rdf_create_graph(Name),
  thread_create(Goal, Id, [alias(Name)]).

dh_progs_web([HTML_Table]):-
  findall(
    [Alias,Id,Status,CPU_Time],
    (
      thread_property(Id, alias(Alias)),
      thread_property(Id, status(Status)),
      thread_statistics(Id, cputime, CPU_Time)
    ),
    Tuples
  ),
  html_table(
    [
      caption('Currently running DataHives programs'),
      header(true),
      index(true)
    ],
    [['Alias','Id','Status','CPU time']|Tuples],
    HTML_Table
  ).

