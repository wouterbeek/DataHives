:- module(
  dh_messages,
  [
    dh_agent_ask/3, % +Agent:url
                    % +Question:compound
                    % -Answer
    dh_agent_command/2, % +Agent:url
                        % +Command:compound
    process_messages/0
  ]
).

/** <module> DataHives: messages

Where message queue are being processed.

@author Wouter Beek
@version 2014/06-2014/09
*/

:- use_module(library(semweb/rdfs)).

:- use_module(generics(vox_populi)).

:- use_module(dh_act(dh_entailment)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_nav(dh_nav)).



%! dh_agent_ask(+Agent:url, +Question:compound, -Answer) is det.

dh_agent_ask(Agent, Question, Answer):-
  ask_thread(Agent, Question, Answer).


%! dh_agent_command(+Agent:url, +Command:compound) is det.

dh_agent_command(Agent, Command):-
  command_thread(Agent, Command).


process_messages:-
  thread_peek_message(command(Caller,Command)), !,
  thread_get_message(command(Caller,Command)),
  call(Command).
process_messages:-
  thread_peek_message(question(Caller,Question)), !,
trace,
  thread_get_message(question(Caller,Question)),
  call(Question, Answer),
  thread_self(Me),
  thread_send_message(Caller, answer(Me,Answer)).
process_messages.

