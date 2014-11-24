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

:- use_module(library(debug)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(vox_populi)).

:- use_module(dh(agent/dh_agent_property)).
:- use_module(dh(agent/dh_agent_property_global)).
:- use_module(dh(agent/dh_agent_property_local)).
:- use_module(dh(beh/act/dh_entailment)).
:- use_module(dh(beh/nav/dh_nav)).
:- use_module(dh(core/dh_cycle)).



%! dh_agent_ask(+Agent:url, +Question:compound, -Answer) is det.

dh_agent_ask(Agent, Question, Answer):-
  ask_thread(Agent, Question, Answer).


%! dh_agent_command(+Agent:url, +Command:compound) is det.

dh_agent_command(Agent, Command):-
  command_thread(Agent, Command).


%! process_messages is det.
% Loops until all staged messages are answered.
%
% Messages are either command for the thread to execute
% or questions for the command to answer.

% Process a command.
process_messages:-
  thread_peek_message(command(Caller,Command)), !,
  thread_get_message(command(Caller,Command)),
  
  % DEB
  debug(db_message, '[C] ~w', [Command]),
  
  % Process command.
  call(Command),
  
  % Loop.
  process_messages.
% Process a question.
process_messages:-
  thread_peek_message(question(Caller,Question)), !,
  thread_get_message(question(Caller,Question)),
  
  % DEB
  debug(dh_message, '[Q] ~w', [Question]),
  
  % Process.
  call(Question, Answer),
  
  % Reply.
  thread_self(Me),
  thread_send_message(Caller, answer(Me,Answer)),
  
  % Loop.
  process_messages.
% No more messages to process.
process_messages.

