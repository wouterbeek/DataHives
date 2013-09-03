:- module(
  dh_samp,
  [
    start_sampler/2 % +DestinationGraph:atom
                    % +Interval:positive_integer
  ]
).

/** <module> DataHives Sampling

@author Wouter Beek
@version 2013/09
*/

:- use_module(dh(dh_net)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(math(random_ext)).
:- use_module(rdf(rdf_name)).

:- dynamic(location/5).

:- rdf_meta(rdf_next_triple(+,+,r,r,r,-,-,r,r,r)).
:- rdf_meta(rdf_next_triple_random(+,+,r,r,r,-,-,r,r,r)).
:- rdf_meta(sampling(+,r)).

:- debug(dh_samp).



rdf_next_triple(H1, G1, S1, _P1, _O1, H2, G2, S2, P2, O2):-
  rdf(S2, P2, S1, G2), connected(H1, G1, S1, H2, G2), O2 = S1.
rdf_next_triple(H1, G1, _S1, _P1, O1, H2, G2, S2, P2, O2):-
  rdf(O1, P2, O2, G2), connected(H1, G1, O1, H2, G2), S2 = O1.
rdf_next_triple(H1, G1, _S1, P1, _O1, H2, G2, S2, P2, O2):-
  rdf(S2, P2, P1, G2), connected(H1, G1, P1, H2, G2), O2 = P1.
rdf_next_triple(H1, G1, _S1, P1, _O1, H2, G2, S2, P2, O2):-
  rdf(P1, P2, O2, G2), connected(H1, G1, P1, H2, G2), S2 = P1.

rdf_next_triple_random(H1, G1, S1, P1, O1, H2, G2, S2, P2, O2):-
  findall(
    H2-G2-S2-P2-O2,
    rdf_next_triple(H1, G1, S1, P1, O1, H2, G2, S2, P2, O2),
    Tuples
  ),
  length(Tuples, L),
  random_betwixt(1, L, I),
  nth1(I, Tuples, H2-G2-S2-P2-O2).

start_sampler(G, I):-
  rdf_graph(G),
  initial_state(IS),
  thread_create(sampling(IS, I, G), _Id, []),
  debug(dh_samp, 'A sampler was started in graph ~w.', [G]).

sampling(state(H1,G1,rdf(S1,P1,O1)), I, ToG):-
  rdf_next_triple_random(H1, G1, S1, P1, O1, H2, G2, S2, P2, O2),

  send_to_store(H2, G2, S2, P2, O2, ToG),

  % DEB
  thread_self(ThreadId),
  rdf_triple_name(S2, P2, O2, TripleName),
  hive_graph_name(H2, G2, HG_Name),
  debug(
    dh_samp,
    'Moved to state [~w/~w/~w]',
    [ThreadId,HG_Name,TripleName]
  ),

  sleep(I),
  sampling(state(H2,G2,rdf(S2,P2,O2)), I, ToG).

send_to_store(H, G, S, P, O, ToG):-
  rdf_assert(S, P, O, ToG),
  assert(location(H, G, S, P, O)),

  % DEB
  hive_graph_name(H, G, HG_Name),
  rdf_triple_name(S, P, O, TripleName),
  debug(
    dh_samp,
    'A sampler has sent to the store: [~w] ~w',
    [HG_Name,TripleName]
  ).

