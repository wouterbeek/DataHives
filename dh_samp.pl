:- module(
  dh_samp,
  [
    location/6, % ?Hive:atom
                % ?Graph:atom
                % ?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Object:or([bnode,iri,literal])
                % ?Count:positive_integer
    start_sampler/2, % +DestinationGraph:atom
                     % +Interval:positive_integer
    top_position_locations/1, % -AscendingPairs:list(pair(integer,compound))
    toplocs_web/1 % -DOM:list
  ]
).

/** <module> DataHives Sampling

@author Wouter Beek
@version 2013/09-2013/10
*/

:- use_module(dh(dh_net)).
:- use_module(html(html_table)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(math(random_ext)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_read)).

%! location(
%!   ?Hive:atom,
%!   ?Graph:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?Count:positive_integer
%! ) is nondet.
%
% In line with the DataHives metaphor,
% we do not register triples we have not seen before.
%
% @param Hive The atomic name of a hive.
% @param Graph The atomic name of a graph.
% @param Subject An RDF subject term;
%        either a blank node or an IRI.
% @param Predicate An RDF predicate term;
%        an IRI.
% @param Object An RDF object term;
%        either a blank node, a literal, or an IRI.
% @param Count The number of occurrences of a triple.

:- dynamic(location/6).

%! locations(?Pairs:list(nvpair)) is det.

:- dynamic(locations/1).

:- rdf_meta(rdf_next_triple(+,+,r,r,r,-,-,r,r,r)).
:- rdf_meta(rdf_next_triple_random(+,+,r,r,r,-,-,r,r,r)).
:- rdf_meta(sampling(+,r)).

:- debug(dh_samp).

:- initialization(init_locations).



init_locations:-
  assert(locations([])).

%! rdf_next_triple(
%!   +FromHive:atom,
%!   +FromGraph:atom,
%!   +FromSubject:or([bnode,iri]),
%!   +FromPredicate:iri,
%!   +FromObject:or([bnode,iri,literal]),
%!   -ToHive:atom,
%!   -ToGraph:atom,
%!   -ToSubject:or([bnode,iri]),
%!   -ToPredicate:iri,
%!   -ToObject:or([bnode,iri,literal])
%! ) is nondet.
% Returns directly connected triples.
%
% @param FromHive The atomic name of a hive.
% @param FromGraph The atomic name of a graph.
% @param FromSubject An RDF subject term;
%        either a blank node or an IRI.
% @param FromPredicate An RDF predicate term;
%        an IRI.
% @param FromObject An RDF object term;
%        either a blank node, a literal, or an IRI.
% @param ToHive The atomic name of a hive.
% @param ToGraph The atomic name of a graph.
% @param ToSubject An RDF subject term;
%        either a blank node or an IRI.
% @param ToPredicate An RDF predicate term;
%        an IRI.
% @param ToObject An RDF object term;
%        either a blank node, a literal, or an IRI.

rdf_next_triple(H1, G1, S1, _P1, _O1, H2, G2, S2, P2, O2):-
  rdf2(S2, P2, S1, G2),
  connected(H1, G1, S1, H2, G2),
  O2 = S1.
rdf_next_triple(H1, G1, _S1, _P1, O1, H2, G2, S2, P2, O2):-
  rdf2(O1, P2, O2, G2),
  connected(H1, G1, O1, H2, G2),
  S2 = O1.
rdf_next_triple(H1, G1, _S1, P1, _O1, H2, G2, S2, P2, O2):-
  rdf2(S2, P2, P1, G2),
  connected(H1, G1, P1, H2, G2),
  O2 = P1.
rdf_next_triple(H1, G1, _S1, P1, _O1, H2, G2, S2, P2, O2):-
  rdf2(P1, P2, O2, G2),
  connected(H1, G1, P1, H2, G2),
  S2 = P1.

%! rdf_next_triple_random(
%!   +FromHive:atom,
%!   +FromGraph:atom,
%!   +FromSubject:or([bnode,iri]),
%!   +FromPredicate:iri,
%!   +FromObject:or([bnode,iri,literal]),
%!   -ToHive:atom,
%!   -ToGraph:atom,
%!   -ToSubject:or([bnode,iri]),
%!   -ToPredicate:iri,
%!   -ToObject:or([bnode,iri,literal])
%! ) is det.
% Returns a randomly chosen directly connected triple.
%
% @param FromHive The atomic name of a hive.
% @param FromGraph The atomic name of a graph.
% @param FromSubject An RDF subject term;
%        either a blank node or an IRI.
% @param FromPredicate An RDF predicate term;
%        an IRI.
% @param FromObject An RDF object term;
%        either a blank node, a literal, or an IRI.
% @param ToHive The atomic name of a hive.
% @param ToGraph The atomic name of a graph.
% @param ToSubject An RDF subject term;
%        either a blank node or an IRI.
% @param ToPredicate An RDF predicate term;
%        an IRI.
% @param ToObject An RDF object term;
%        either a blank node, a literal, or an IRI.

rdf_next_triple_random(H1, G1, S1, P1, O1, H2, G2, S2, P2, O2):-
  findall(
    H2-G2-S2-P2-O2,
    rdf_next_triple(H1, G1, S1, P1, O1, H2, G2, S2, P2, O2),
    Tuples
  ),
  length(Tuples, L),
  random_betwixt(1, L, I),
  nth1(I, Tuples, H2-G2-S2-P2-O2).

%! start_sampler(+DestinationGraph:atom, +Interval:positive_integer) is det.

start_sampler(G, I):-
  rdf_graph(G),
  % Retieves a state of the following form:
  % ~~~
  % state(Hive:atom,DefaultGraph:atom,Triple:compound)
  % ~~~
  initial_state(IS),
  thread_create(sampling(IS, I, G), _Id, []),
  debug(dh_samp, 'A sampler was started in graph ~w.', [G]).

%! sampling(
%!   +State:compound,
%!   +Interval:positive_integer,
%!   +ToGraph:atom
%! ) is det.
% Starts sampling in the given state, sampling data into the given graph.
%
% @param State A compound term of the following form:
%        ~~~
%        state(Hive:atom,DefaultGraph:atom,Triple:compound)
%        ~~~
% @param Interval A positive integer representing
%        the sampling interval in seconds.
% @param ToGraph The atomic name of the graph in which
%        sampled triples are stored.

sampling(state(H1,G1,rdf(S1,P1,O1)), I, ToG):-
  rdf_next_triple_random(H1, G1, S1, P1, O1, H2, G2, S2, P2, O2),

  send_to_store(H2, G2, S2, P2, O2, ToG),

  % DEB
  thread_self(ThreadId),
  with_output_to(atom(TripleName), rdf_triple_name([], S2, P2, O2)),
  hive_graph_name(H2, G2, HG_Name),
  debug(
    dh_samp,
    'Moved to state [~w/~w/~w]',
    [ThreadId,HG_Name,TripleName]
  ),
  sleep(I),
  sampling(state(H2,G2,rdf(S2,P2,O2)), I, ToG).

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
% @param Hive The atomic name of a hive.
% @param Graph The atomic name of a graph.
% @param Subject An RDF subject term;
%        either a blank node or an IRI.
% @param Predicate An RDF predicate term;
%        an IRI.
% @param Object An RDF object term;
%        either a blank node, a literal, or an IRI.
% @param ToGraph The atomic name of the graph where
%        sampled triples are stored.

send_to_store(H, G, S, P, O, ToG):-
  rdf_assert(S, P, O, ToG),
  tick_position(H, G, S, P, O),
  
  % DEB
  hive_graph_name(H, G, HG_Name),
  with_output_to(atom(TripleName), rdf_triple_name([], S, P, O)),
  debug(
    dh_samp,
    'A sampler has sent to the store: [~w] ~w',
    [HG_Name,TripleName]
  ).

%! tick_position(
%!   +Hive:atom,
%!   +Graph:atom,
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal])
%! ) is det.
% Update the per-triple counter.
%
% @param Hive The atomic name of a hive.
% @param Graph The atomic name of a graph.
% @param Subject An RDF subject term;
%        either a blank node or an IRI.
% @param Predicate An RDF predicate term;
%        an IRI.
% @param Object An RDF object term;
%        either a blank node, a literal, or an IRI.

tick_position(H, G, S, P, O):-
  tick_position_location(H, G, S, P, O),
  retract(locations(A1)),
  tick_position_locations(S, P, O, A1, A2),
  assert(locations(A2)).

tick_position_location(H, G, S, P, O):-
  location(H, G, S, P, O, I1), !,
  retract(location(H,G,S,P,O,I1)),
  I2 is I1 + 1,
  assert(location(H,G,S,P,O,I2)).
tick_position_location(H, G, S, P, O):-
  assert(location(H,G,S,P,O,1)).

tick_position_locations(S, P, O, A1, [I2-rdf(S,P,O)|A2]):-
  select(I1-rdf(S,P,O), A1, A2), !,
  I2 is I1 + 1.
tick_position_locations(S, P, O, A, [1-rdf(S,P,O)|A]).

top_position_locations(A3):-
  locations(A1),
  keysort(A1, A2),
  reverse(A2, A3).

toplocs_web([HTML_Table]):-
  top_position_locations(L0),
  length(L1, 10),
  append(L1, _, L0),
  findall(
    [K,V],
    member(K-V, L1),
    Rows
  ),
  html_table(
    [
      caption('The top locations until now.'),
      header(true),
      indexed(true)
    ],
    [['Count','Triple']|Rows],
    HTML_Table
  ).

