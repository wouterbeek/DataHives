% The load file for the DataHives project.

:- use_module(library(ansi_term)).

:- multifile(user:prolog/3).
:- dynamic(user:prolog/3).

user:project('DataHives', 'Where agents travel across the Semantic Web', dh).

:- initialization(load_dh).

load_dh:-
  % Entry point.
  source_file(load_dh, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(dh, ThisDir)),
  assert(user:file_search_path(project, ThisDir)),
  
  % Data subdirectory.
  directory_file_path(ThisDir, data, DataDir),
  make_directory_path(DataDir),
  assert(user:file_search_path(data, DataDir)),
  
  % File search paths.
  ensure_loaded(dh(index)),
  
  % Load submodules.
  load_plc(dh),
  load_plHtml(dh),
  load_plServer(dh),
  load_plDev(dh),
  load_plRdf(dh),
  load_plRdfDev(dh),
  
  % Load the Web-based development environment.
  use_module(plDev(plDev)),
  use_module(dh_web(dh_web_agent)),
  use_module(dh_web(dh_web_graph)),
  
  % Load some tests.
  use_module(dh_test(dh_test)).


load_plc(_):-
  user:file_search_path(plc, _), !.
load_plc(Parent):-
  Spec =.. [Parent,'Prolog-Library-Collection'],
  absolute_file_name(Spec, _, [access(read),file_type(directory)]), !,
  assert(user:file_search_path(plc, Spec)),
  ensure_loaded(plc(index)).
load_plc(Parent):-
  print_message(warning, missing_submodule(plc,Parent)).

load_plHtml(_):-
  user:file_search_path(plHtml, _), !.
load_plHtml(Parent):-
  Spec =.. [Parent,plHtml],
  absolute_file_name(Spec, _, [access(read),file_type(directory)]), !,
  assert(user:file_search_path(plHtml, Spec)).
load_plHtml(Parent):-
  print_message(warning, missing_submodule(plHtml,Parent)).

load_plServer(_):-
  user:file_search_path(plServer, _), !.
load_plServer(Parent):-
  Spec =.. [Parent,plServer],
  absolute_file_name(Spec, _, [access(read),file_type(directory)]), !,
  assert(user:file_search_path(plServer, Spec)).
load_plServer(Parent):-
  print_message(warning, missing_submodule(plServer,Parent)).

load_plDev(_):-
  user:file_search_path(plDev, _), !.
load_plDev(Parent):-
  Spec =.. [Parent,plDev],
  absolute_file_name(Spec, _, [access(read),file_type(directory)]), !,
  assert(user:file_search_path(plDev, Spec)).
load_plDev(Parent):-
  print_message(warning, missing_submodule(plDec,Parent)).

load_plRdfDev(_):-
  user:file_search_path(plRdfDev, _), !.
load_plRdfDev(Parent):-
  Spec =.. [Parent,'plRdf-Dev'],
  absolute_file_name(Spec, _, [access(read),file_type(directory)]), !,
  assert(user:file_search_path(plRdfDev, Spec)).
load_plRdfDev(Parent):-
  print_message(warning, missing_submodule(plRdfDev,Parent)).

load_plRdf(_):-
  user:file_search_path(plRdf, _), !.
load_plRdf(Parent):-
  Spec =.. [Parent,plRdf],
  absolute_file_name(Spec, _, [access(read),file_type(directory)]), !,
  assert(user:file_search_path(plRdf, Spec)).
load_plRdf(Parent):-
  print_message(warning, missing_submodule(plRdf,Parent)).


:- multifile(prolog:message//1).

prolog:message(missing_submodule(Submodule,Parent)) -->
  [
    'The ', Submodule, ' submodule is not present.', nl,
    'Consider running the following from within the ', Parent, ' directory:', nl,
    '    git submodule init', nl,
    '    git submodule update'
  ].

