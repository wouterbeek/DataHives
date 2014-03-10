% The load file for the DataHives project.

:- multifile(user:project_name/1).
user:project_name('DataHives').

:- initialization(load_dh).

load_dh:-
  % Entry point.
  source_file(load_dh, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  
  % DataHives
  assert(user:file_search_path(dh, ThisDir)),
  use_module(dh(dh_lit_tag)),
  use_module(dh(dh_test)).

