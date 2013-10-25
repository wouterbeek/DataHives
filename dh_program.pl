:- module(
  dh_program,
  [
    programs_web/1, % -DOM:list
    start_program/6, % +Name:atom
                     % :InitialState
                     % :Exec
                     % :Move
                     % +Interval:positive_integer
                     % +HasStore:boolean
    start_programs/7 % +BaseName:atom
                     % +Copies:positive_integer
                     % :InitialState
                     % :Exec
                     % :Move
                     % +Interval:positive_integer
                     % +HasStore:boolean
  ]
).

/** <module> DataHives program invocation

Allows programs to be run inside the DataHives architecture.

@author Wouter Beek
@version 2013/09-2013/10
*/

:- use_module(dh(dh_network)).
:- use_module(html(html_table)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_name)).
:- use_module(server(web_console)).

:- register_module(dh_program).

:- meta_predicate(run_program(+,2,2,+,+)).
:- meta_predicate(start_program(+,1,2,2,+,+)).
:- meta_predicate(start_programs(+,+,1,2,2,+,+)).

:- debug(dh_program).



programs_web([HTML_Table]):-
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
  html_table(
    [
      caption('The programs that are currently running in DataHives.'),
      header(true),
      index(true)
    ],
    [['Alias','Id','Status','CPU time']|Rows],
    HTML_Table
  ).

%! run_program(
%!   +FromState:compound,
%!   :Execution,
%!   :Move,
%!   +Interval:positive_integer,
%!   ?Store:atom
%! ) is det.
% Executes one iteration of a program.
%
% @param FromState A compound term representing a state in DataHives.
% @param Exection The goal that implements the execution of this program
%        on the data it encounters.
% @param Move A goal that implements the relocation of the program
%        from one state to the next.
% @param Interval A positive integer representing
%        the sampling interval in seconds.
% @param Store The atomic name of the graph that is used to store results.

run_program(FromState, Exec, Move, I, Store):-
  FromState = state(_H1,_G1,Triple1),
  
  call(Exec, Triple1, Store),
  
  call(Move, FromState, ToState),
  
  % DEB
  (
    debugging(dh_samp, false), !
  ;
    thread_self(ThreadId),
    maplist(state_display, [FromState,ToState], [FromStateName,ToStateName]),
    debug(
      dh_samp,
      'Program ~w moved: ~w ----> ~w',
      [ThreadId,FromStateName,ToStateName]
    )
  ),
  
  sleep(I),
  run_program(ToState, Exec, Move, I, Store).

%! send_to_store(
%!   +Hive:atom,
%!   +Graph:atom,
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   +ToGraph:atom
%! ) is det.
% Sends a sampled triple to the given sampling store.
%
% @param State A compound term representing a DataHives state.
% @param Store The atomic name of the graph where results are stored.

send_to_store(State, Store):-
  State = state(_H,_G,rdf(S,P,O)),
  
  rdf_assert(S, P, O, Store),
  
  % DEB
  thread_self(ThreadId),
  state_display(State, StateName),
  debug(
    dh_samp,
    'Program ~w sent to store ~w: ~w',
    [ThreadId,Store,StateName]
  ).

%! start_program(
%!   +Name:atom,
%!   :InitialState,
%!   :Exec,
%!   :Move,
%!   +Interval:positive_integer,
%!   +HasStore:boolean
%! ) is det.

start_program(Name, _IS_Goal, _Exec, _Move, _I, _HasStore):-
  thread_property(_Id, alias(Name)), !,
  debug(
    dh_program,
    'Cannot create program: A thread named ~w already exists.',
    [Name]
  ).
start_program(Name, _IS_Goal, _Exec, _Move, _I, _HasStore):-
  rdf_graph(Name), !,
  debug(
    dh_program,
    'Cannot create program: An RDF graph named ~w already exists.',
    [Name]
  ).
start_program(Name, IS_Goal, Exec, Move, I, HasStore):-
  % Retrieve the initial state.
  call(IS_Goal, IS),
  
  % Create the RDF graph that is used for storage, if any.
  (
    HasStore == true
  ->
    rdf_create_graph(Name)
  ;
    true
  ),
  
  thread_create(run_program(IS, Exec, Move, I, Name), Id, [alias(Name)]),
  
  % DEB
  state_display(IS, IS_Name),
  debug(dh_samp, 'Program ~w started from state ~w.', [Id,IS_Name]).

%! start_programs(
%!   +BaseName:atom,
%!   +Copies:positive_integer,
%!   :InitialState,
%!   :Exec,
%!   :Move,
%!   +Interval:positive_integer,
%!   +HasStore:boolean
%! ) is det.
% Starts multiple programs of the same type.

start_programs(Name1, Copies, IS, Exec, Move, I, HasStore):-
  forall(
    between(1, Copies, N),
    (
      format(atom(Name2), '~w_~w', [Name1,N]),
      start_program(Name2, IS, Exec, Move, I, HasStore)
    )
  ).

