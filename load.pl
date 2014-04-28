% The load file for the DataHives project.

:- multifile(user:project/2).
user:project('DataHives', 'Agent simulation platform for PraSem.').

:- initialization(load_dh).

load_dh:-
  set_file_search_path,
  ensure_loaded(dh(index)).

set_file_search_path:-
  user:file_search_path(dh, _).
set_file_search_path:-
  source_file(load_dh, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(dh, ThisDir)).

