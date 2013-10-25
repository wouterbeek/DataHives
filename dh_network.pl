:- module(
  dh_network,
  [
    connect_hives/1, % +Hives:list(compound)
    connected/5, % ?HiveName1:atom
                 % ?Graph1:atom
                 % ?RDF_Term:iri
                 % ?HiveName2:atom
                 % ?Graph2:atom
    connected_hives_web/1, % -DOM:list
    create_hive/3, % +Name:atom
                   % +RDF_Dataset:compound
                   % -Hive:compound
    hive/2, % ?Name:atom
            % ?RDF_Dataset:compound
    hive_graph_name/3, % +Hive:atom
                       % +Graph:atom
                       % -HiveGraphName:atom
    home_hive/1, % ?HomeHive:or([atom,compound])
    random_initial_state/1, % -RandomInitialState:compound
    register_home_hive/1, % +Hive:compound
    state_display/2, % +State:compound
                     % -StateName:atom
    state_identity/2 % +State1:compound
                     % +State2:compound
  ]
).

/** <module> DataHives Network

@author Wouter Beek
@version 2013/09-2013/10
*/

:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(gv(gv_file)).
:- use_module(html(html_table)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_dataset)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_random)).
:- use_module(rdf(rdf_term)).

%! connection(
%!   ?HiveName1:atom,
%!   ?Graph1:atom,
%!   ?RDF_Term:iri,
%!   ?HiveName2:atom,
%!   ?Graph2:atom
%! ) is nondet.

:- dynamic(connection/5).

%! hive(?Name:atom, ?RDF_Dataset:compound) is nondet.

:- dynamic(hive/2).

%! home_hive(?Hive:compound) is semidet.

:- dynamic(home_hive/1).

:- debug(dh_network).



%! connect_graphs(
%!   +Hive1:atom,
%!   +Graph1:atom,
%!   +Hive2:atom,
%!   +Graph2:atom
%! ) is det.

connect_graphs(H1, G1, H2, G2):-
  % Use the number of triples as a proxy for the number of RDF nodes.
  rdf_statistics(triples_by_graph(G1,N1)),
  rdf_statistics(triples_by_graph(G2,N2)),

  % Read the smaller graph first (optimizing speed).
  (
    N1 =< N2
  ->
    connect_graphs_(H1, G1, H2, G2)
  ;
    connect_graphs_(H1, G2, H2, G1)
  ).

connect_graphs_(H1, G1, H2, G2):-
  % Find a connection.
  rdf_node(G1, T),
  rdf_is_iri(T),
  rdf_node(G2, T),

  % Assert the connection.
  once((
    connection(H1,G1,T,H2,G2)
  ;
    assert(connection(H1,G1,T,H2,G2)),
    flag(conn, C, C+1)
  )),
  % Enumerate by failure.
  fail.
connect_graphs_(_H1, _G1, _H2, _G2).

%! connect_hives(+Hives:list(compound)) is det.

connect_hives(Hs):-
  forall(
    (
      member(H1, H2, Hs),
      hive_graph(H1, G1),
      hive_graph(H2, G2),
      % Assuming the connectivity relation is symmetric,
      % we only need to store connections in one direction.
      once((
        H1 @< H2
      ;
        G1 @< G2
      ))
    ),
    (
      debug(dh_network, 'Comparing ~w:~w and ~w:~w.', [H1,G1,H2,G2]),
      connect_graphs(H1, G1, H2, G2)
    )
  ),
  flag(conn, C, 0),
  debug(dh_network, 'Added ~w connections.', [C]).

%! connected(
%!   ?HiveName1:atom,
%!   ?Graph1:atom,
%!   ?RDF_Term:iri,
%!   ?HiveName2:atom,
%!   ?Graph2:atom
%! ) is nondet.

connected(H, G1, _, H, G2):-
  rdf_graph:rdf_same_graph(G1, G2).
connected(H1, G1, T, H2, G2):-
  connection(H1, G1, T, H2, G2).
connected(H1, G1, T, H2, G2):-
  connection(H2, G2, T, H1, G1).

connected_hives_web([HTML_Table|SVG]):-
  findall(
    [H1,G1,T_Name,H2,G2],
    (
      connection(H1, G1, T, H2, G2),
      rdf_term_name(T, T_Name)
    ),
    Rows
  ),
  Caption = 'The connections between the hives.',
  html_table(
    [
      caption(Caption),
      header(true),
      indexed(true)
    ],
    [['Hive1','Graph1','Connecting term','Hive2','Graph2']|Rows],
    HTML_Table
  ),
  findall(
    edge(N1,N2,[label(T_Name)]),
    (
      connection(H1, G1, T, H2, G2),
      variant_sha1(node(H1,G1), N1),
      variant_sha1(node(H2,G2), N2),
      rdf_term_name(T, T_Name)
    ),
    Es
  ),
  findall(
    vertex(N,node(H,G),[label(HG)]),
    (
      connection(H1, G1, _T, H2, G2),
      ((H = H1, G = G1) ; (H = H2, G = G2)),
      variant_sha1(node(H,G), N),
      format(atom(HG), '~w:~w', [H,G])
    ),
    Vs
  ),
  G_Attrs = [
    charset('UTF-8'),
    directedness(forward),
    fontsize(11),
    label(Caption),
    overlap(false)
  ],
  graph_to_svg_dom([], graph(Vs,Es,G_Attrs), sfdp, SVG).

%! create_hive(+HiveName:atom, +RDF_Dataset:compound, -Hive:compound) is det.

create_hive(N, rdf_dataset(DG,NGs), hive(N,rdf_dataset(DG,NGs))):-
  atom(N),
  rdf_dataset(DG, NGs), !,
  db_add_novel(hive(N,rdf_dataset(DG,NGs))).

%! hive_graph(+HiveName:atom, -Graph:atom) is nondet.

hive_graph(N, G):-
  hive(N, DS),
  rdf_dataset_graph(DS, G).

hive_graph_name(H, G, N):-
  format(atom(N), '~w/~w', [H,G]).

%! random_initial_state(-RandomInitialState:compound) is det.

random_initial_state(state(H,DG,rdf(S,P,O))):-
  home_hive(H),
  hive(H, DS),
  rdf_default_graph(DS, DG),
  rdf_random_triple(DG, S, P, O).

%! register_home_hive(+Hive:or([atom,compound])) is det.

register_home_hive(hive(N,_DS)):- !,
  register_home_hive(N).
register_home_hive(N):-
  atom(N),
  hive(N, _DS), !,
  db_replace_novel(home_hive(N), [r]).

%! state_display(+State:compound, -StateName:atom) is det.

state_display(state(H,G,T), N):-
  with_output_to(atom(TName), rdf_triple_name([], T)),
  hive_graph_name(H, G, HG_Name),
  format(atom(N), '[~w:~w]', [HG_Name,TName]).

%! state_identity(+State1:compound, +State2:compound) is semidet.

state_identity(state(H1,G1,T1), state(H2,G2,T2)):-
  H1 == H2,
  G1 == G2,
  T1 == T2.

%! valid_initial_state(
%!   +Graph:atom,
%!   +RDF_Term:or([bnode,literal,iri])
%! ) is semidet.

valid_initial_state(G, T):-
  rdf_node(G, T),
  rdf_is_iri(T).
