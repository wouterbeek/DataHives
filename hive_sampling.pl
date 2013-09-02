:- module(
  hive_sampling,
  [
    start_sampler/2 % +DestinationGraph:atom
                    % +Interval:positive_integer
  ]
).

/** <module> HIVE_SAMPLING

@author Wouter Beek
@version 2013/09
*/

:- use_module(datahives(hives)).
:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_name)).

:- dynamic(location/5).

:- rdf_meta(rdf_next_triple(+,+,r,r,r,-,-,r,r,r)).
:- rdf_meta(sampling(+,r)).

:- debug(hive_sampling).



rdf_next_triple(H1, G1, S1, _P1, _O1, H2, G2, S2, P2, O2):-
  rdf(S2, P2, S1, G2), connected(H1, G1, S1, H2, G2), O2 = S1.
rdf_next_triple(H1, G1, _S1, _P1, O1, H2, G2, S2, P2, O2):-
  rdf(O1, P2, O2, G2), connected(H1, G1, O1, H2, G2), S2 = O1.
rdf_next_triple(H1, G1, _S1, P1, _O1, H2, G2, S2, P2, O2):-
  rdf(S2, P2, P1, G2), connected(H1, G1, P1, H2, G2), O2 = P1.
rdf_next_triple(H1, G1, _S1, P1, _O1, H2, G2, S2, P2, O2):-
  rdf(P1, P2, O2, G2), connected(H1, G1, P1, H2, G2), S2 = P1.

start_sampler(G, I):-
  rdf_graph(G),
  initial_state(IS),
  thread_create(sampling(IS, I, G), _Id, []).

sampling(state(H1,G1,rdf(S1,P1,O1)), I, ToG):-
  rdf_next_triple(H1, G1, S1, P1, O1, H2, G2, S2, P2, O2),
  
  send_to_store(H2, G2, S2, P2, O2, ToG),
  
  % DEB
  thread_self(ThreadId),
  rdf_triple_name(S2, P2, O2, TripleName),
  hive_graph_name(H2, G2, HG_Name),
  debug(
    hive_sampling,
    'Moved to state [~w/~w/~w]',
    [ThreadId,HG_Name,TripleName]
  ),
  
  sleep(I),
  sampling(state(H2,G2,rdf(S2,P2,O2)), I, ToG).

send_to_store(H, G, S, P, O, ToG):-
  rdf_assert(S, P, O, ToG),
  assert(location(H, G, S, P, O)).

