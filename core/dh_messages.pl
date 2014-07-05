:- module(
  dh_messages,
  [
    process_messages/0
  ]
).

/** <module> DataHives: messages

Where message queue are being processed.

@author Wouter Beek
@version 2014/06
*/

:- use_module(dh_core(dh_cycle)). % Meta-calls.
:- use_module(dh_core(dh_population)). % Meta-calls.
:- use_module(dh_nav(dh_navigate)). % Meta-calls.

:- meta_predicate(answer_question(1,+)).
:- meta_predicate(execute_command(0)).



answer_question(Goal, X):-
  call(Goal, X).


execute_command(Goal):-
  call(Goal).


process_messages:-
  thread_peek_message(command(Caller,Command)), !,
  thread_get_message(command(Caller,Command)),
  execute_command(Command).
process_messages:-
  thread_peek_message(question(Caller,Question)), !,
  thread_get_message(question(Caller,Question)),
  answer_question(Question, Answer),
  thread_self(Me),
  thread_send_message(Caller, answer(Me,Answer)).
process_messages.

