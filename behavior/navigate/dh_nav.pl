:- module(dh_nav, []).

/** <module> DataHives navigate

Reexports the navigation stategies in DataHives.

@author Wouter Beek
@version 2104/07
*/

:- reexport(dh_nav(dh_random_jump)).
:- reexport(dh_nav(dh_random_walk)).
:- reexport(dh_nav(dh_weighted_walk)).

