% Index for the DataHives project.

user:file_search_path(dh_agent, dh(agent)).
user:file_search_path(dh_beh,   dh(behavior)).
  user:file_search_path(dh_act,  dh_beh(act)).
  user:file_search_path(dh_com,  dh_beh(communicate)).
  user:file_search_path(dh_eval, dh_beh(evaluate)).
  user:file_search_path(dh_nav,  dh_beh(navigate)).
user:file_search_path(dh_core,  dh(core)).
user:file_search_path(dh_debug, dh(debug)).
user:file_search_path(dh_test,  dh(test)).
user:file_search_path(dh_web,   dh(web)).

