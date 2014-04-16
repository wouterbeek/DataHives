#!/home/wbeek/bin/swipl

% The run file for the DataHives project.

:- initialization(run_dh).

run_dh:-
  source_file(run_dh, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  
  % PGC
  load_plc(project),
  
  % DataHives load file.
  ensure_loaded(load).

load_plc(_Project):-
  user:file_search_path(plc, _Spec), !.
load_plc(Project):-
  Spec =.. [Project,'Prolog-Library-Collection'],
  assert(user:file_search_path(plc, Spec)),
  load_or_debug(plc).

load_or_debug(Project):-
  predicate_property(user:debug_mode, visible), !,
  Spec =.. [Project,debug],
  ensure_loaded(Spec).
load_or_debug(Project):-
  Spec =.. [Project,load],
  ensure_loaded(Spec).

