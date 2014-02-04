:- module(
  dh_term_check,
  [
    dh_term_check/2 % +Triple:compound
                    % -NoStore:var
  ]
).

/** <module> Term check

Program that checks RDF terms and that runs on the DataHives architecture.

@author Wouter Beek
@version 2013/10, 2014/02
*/

:- use_module(html(html_table)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(web_modules)).

http:location(dh, root(dh), []).
:- http_handler(dh(termcheck), suspicious_terms_web, []).

:- web_module_add('DH TermCheck', suspicious_terms_web).

%! suspicious_term(?RDF_Term:or([bnode,literal,iri])) is nondet.

:- dynamic(suspicious_term/1).

:- initialization(init_termcheck).



init_termcheck:-
  (
    thread_property(_Id, alias(term_check_manager)), !
  ;
    thread_create(term_check_manager, _Id, [alias(term_check_manager)])
  ).

suspicious_terms(Terms):-
  findall(
    Term,
    suspicious_term(Term),
    Terms
  ).

suspicious_terms_web(_Request):-
  suspicious_terms(Terms),
  reply_html_page(
    app_style,
    title('DataHives - Term check'),
    html(
      \rdf_html_table(
        _,
        `A list suspicious RDF terms that were encountered.`,
        ['RDF term'],
        Terms
      )
    )
  ).

term_check(Term):-
  rdf_is_bnode(Term), !.
term_check(Term):-
  rdf_is_literal(Term), !.
term_check(Term):-
  (
    suspicious_iri(Term)
  ->
    flag(iri_bad, X, X + 1),
    thread_send_message(term_check_manager, suspicious_term(Term))
  ;
    flag(iri_fine, X, X + 1)
  ).

dh_term_check(rdf(S,P,O), _NoStore):-
  maplist(term_check, [S,P,O]).

%! suspicious_iri(+Term:iri) is semidet.

suspicious_iri(Term):-
  uri_components(Term, uri_components(Scheme1, Host1, Path1, _, _)),
  sub_atom(Path1, _, _, 0, SubPath),
  uri_components(SubPath, uri_components(Scheme2, Host2, _, _, _)),
  Scheme1 == Scheme2,
  Host1 == Host2.

term_check_manager:-
  thread_get_message(suspicious_term(Term)),
  (
    suspicious_term(Term), !
  ;
    assert(suspicious_term(Term))
  ).

