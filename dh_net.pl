:- module(
  dh_net,
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
    initial_state/1, % -State:compound
    register_home_hive/1, % +Hive:compound
    state_display/2, % +State:compound
                     % -StateName:atom
    state_identity/2 % +State1:compound
                     % +State2:compound
  ]
).

/** <module> DataHives Network

@author Wouter Beek
@version 2013/09
*/

:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(gv(gv_file)).
:- use_module(html(html)).
:- use_module(library(debug)).
:- use_module(rdf(rdf_dataset)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_random)).
:- use_module(rdf(rdf_term)).
:- use_module(server(web_console)).

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

:- register_module(dh_net).

:- debug(dh_net).



%! connect_hives(+Hives:list(compound)) is det.

connect_hives(Hs):-
  member(H1, H2, Hs),
  hive_graph(H1, G1),
  hive_graph(H2, G2),
  % Assuming the connectivity relation is symmetric,
  % we only need to store one direction.
  once((
    H1 @< H2
  ;
    G1 @< G2
  )),
  debug(dh_net, 'Comparing ~w:~w and ~w:~w.', [H1,G1,H2,G2]),
  rdf_node(G1, T),
  rdf_is_iri(T),
  rdf_node(G2, T),
  once((
    connection(H1,G1,T,H2,G2)
  ;
    assert(connection(H1,G1,T,H2,G2)),
    flag(conn, C, C+1)
  )),
  fail.
connect_hives(_Hs):-
  flag(conn, C, 0),
  debug(dh_net, 'Added ~w connections.', [C]).

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

connected_hives_web([Table|SVG]):-
  findall(
    [H1,G1,T_Name,H2,G2],
    (
      connection(H1, G1, T, H2, G2),
      rdf_term_name(T, T_Name)
    ),
    Rows
  ),
  list_to_table(
    [caption('The connections between the hives.'),header(true)],
    [['Hive1','Graph1','Connecting term','Hive2','Graph2']|Rows],
    Table
  ),
  findall(
    edge(N1,N2,[label(T_Name)]),
    (
      connection(H1, G1, T, H2, G2),
      variant_sha1(node(H1,G1), N1),
      variant_sha1(node(H2,G2), N2),
      rdf_term_name(T, T_Name)
    ),
    Edges
  ),
  findall(
    vertex(N, node(H,G), [label(HG)]),
    (
      connection(H1, G1, _T, H2, G2),
      ((H = H1, G = G1) ; (H = H2, G = G2)),
      variant_sha1(node(H,G), N),
      format(atom(HG), '~w:~w', [H,G])
    ),
    Vertices
  ),
  graph_to_svg_dom([], graph(Vertices,Edges,[]), sfdp, SVG).

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

%! initial_state(-InitialState:compound) is det.

initial_state(state(H,DG,rdf(S,P,O))):-
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
  rdf_term_name(T, T_Name),
  hive_graph_name(H, G, HG_Name),
  format(atom(N), '[~w/~w]', [HG_Name,T_Name]).

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

