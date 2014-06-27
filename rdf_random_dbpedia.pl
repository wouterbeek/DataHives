:- module(
  rdf_random_dbpedia,
  [
    rdf_random_dbpedia_resource/1, % -RandomResource:iri
    rdf_random_dbpedia_triple/3 % -RandomSubject:or([bnode,iri])
                                % -RandomPredicate:iri
                                % -RandomObject:or([bnode,iri,literal])
  ]
).

/** <module> RDF random DBpedia

Support for random RDF triples coming from DBpedia.

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(http/http_client)).
:- use_module(library(memfile)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).
:- use_module(library(uri)).
:- use_module(library(xpath)).

:- use_module(generics(typecheck)).
:- use_module(nlp(dictionary)).
:- use_module(xml(xml_namespace)).

:- use_module(plRdf(rdf_namespaces)). % Namespace declarations.

:- use_module(lodCache(lod_cache_egograph)).

:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').



%! rdf_random_dbpedia_resource(-Resource:iri) is det.

rdf_random_dbpedia_resource(Resource):-
  repeat,
    random_word('en-US', Word, _),
    setup_call_cleanup(
      new_memory_file(Handle),
      rdf_random_dbpedia_resource(Word, Handle, Resource),
      free_memory_file(Handle)
    ), !.

%! rdf_random_dbpedia_resource(
%!   +Word:atom,
%!   +Handle:blob,
%!   -Resource:iri
%! ) is det.

rdf_random_dbpedia_resource(Word, Handle, Resource):-
  uri_components(Url, uri_components(http,'dbpedia.org','/fct/',_,_)),
  uri_query_components(Content, [q=Word]),
  setup_call_cleanup(
    open_memory_file(Handle, write, Write),
    http_post(
      Url,
      atom('application/x-www-form-urlencoded', Content),
      _,
      [request_header('Accept'='text/html'),to(stream(Write))]
    ),
    close(Write)
  ),
  setup_call_cleanup(
    open_memory_file(Handle, read, Read),
    (
      load_structure(Read, Dom, [dialect(xhtml),max_errors(-1)]),
      first_resource_in_dom(Dom, Resource)
    ),
    close(Read)
  ).

%! first_resource_in_dom(+Dom:list, -Resource:iri) is det.

first_resource_in_dom(Dom, Resource):-
  xpath_chk(Dom, //table(@id=result_t), Table),
  xpath(Table, //tr/td, Td),
  xpath_chk(Td, //a, _), !,
  xpath_chk(Td, /self(normalize_space), Resource0),
  expand_resource_name(Resource0, Resource).

expand_resource_name(Resource, Resource):-
  is_url(Resource), !.
expand_resource_name(Resource0, Resource):-
  atomic_list_concat([Namespace,LocalName], ':', Resource0),
  rdf_global_id(Namespace:LocalName, Resource).


%! rdf_random_dbpedia_triple(
%!   -RandomSubject:or([bnode,iri])
%!   -RandomPredicate:iri
%!   -RandomObject:or([bnode,iri,literal])
%! ) is det.

rdf_random_dbpedia_triple(S, P, O):-
  rdf_random_dbpedia_resource(Resource),
  lod_cache_egograph(Resource, Triples, [cache(false)]),
  random_member(rdf(S,P,O), Triples).

