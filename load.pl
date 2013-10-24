% The load file for the DataHives project.

project_name('DataHives').

:- initialization(load_datahives).

load_datahives:-
  source_file(load_datahives, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  % By asserting the DataHives directory as mnemonic =project= we can refer
  % to this from within the PGC (which does not 'know' that DataHives is
  % using it).
  assert(user:file_search_path(project, ThisDirectory)),
  assert(user:file_search_path(dh, ThisDirectory)),

  assert(user:file_search_path(data, dh('Data'))),

  % Load the PGC.
  assert(user:file_search_path(pgc, dh('PGC'))),
  (
    predicate_property(debug_project, visible)
  ->
    ensure_loaded(pgc(debug))
  ;
    ensure_loaded(pgc(load))
  ),

  % DataHives main module.
  use_module(dh(dh_test)),
  use_module(dh(dh)).

