:- module(
  rdf_random_dbpedia,
  [
    rdf_random_dbpedia_resource/1, % -RandomResource:iri
    rdf_random_dbpedia_triple/1 % -Triple:compound
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

:- use_module(plRdf(rdf_prefixes)). % Namespace declarations.

:- use_module(lodCache(lod_cache_egograph)).

:- rdf_register_prefix(dbpedia, 'http://dbpedia.org/resource/').



%! rdf_random_dbpedia_resource(-Resource:iri) is det.

rdf_random_dbpedia_resource(Resource):-
  % We must repeat, since some English words do not deliver
  % any search results on DBpedia.
  repeat,
  
  % Take a random word from an English dictionary.
  random_word('en-US', Word, _),
  
  % Return a triple from the description of the dereference of
  % the first resource found upon searching for the given word
  % using DBpedia's Virtuoso search service.
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


%! rdf_random_dbpedia_triple(-Triple:compound) is det.

rdf_random_dbpedia_triple(rdf(S,P,O)):-
  rdf_random_dbpedia_resource(Resource),
  lod_cache_egograph(Resource, Triples, [cache(false)]),
  random_member(rdf(S,P,O), Triples).

