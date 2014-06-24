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

:- use_module(dh_core(dh_navigate)).



process_messages:-
  thread_peek_message(get_lifetime(Caller)), !,
  thread_self(Me),
  number_of_steps(Lifetime),
  thread_send_message(Caller, lifetime(Me,Lifetime)).
process_messages.

