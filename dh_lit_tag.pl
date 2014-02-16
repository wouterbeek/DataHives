:- module(
  dh_lit_tag,
  [
    dh_lit_tag_crawler/1 % ?URL
  ]
).

/** <module> DataHives: Language tags

Count languages and datatypes used in literals.

@author Wouter Beek
@version 2014/02
*/

:- use_module(dbpedia(dbpedia)).
:- use_module(dcg(dcg_generic)).
:- use_module(dh(dh)).
:- use_module(dh(dh_walk)).
:- use_module(generics(meta_ext)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_web(rdf_html_term)).
:- use_module(server(web_modules)).

http:location(dh, root(dh), []).
:- http_handler(dh(literal_tag), dh_lit_tag, []).

:- web_module_add('DH LitTag', dh_lit_tag).

:- dynamic(literal_tag/3).

default_url('http://dbpedia.org/resource/Banana').



dh_lit_tag(_Request):-
  setoff(
    Tag,
    literal_tag(_, Tag, _),
    Tags
  ),
  findall(
    Sum-Tag,
    (
      member(Tag, Tags),
      findall(
        N,
        literal_tag(_, Tag, N),
        Ns
      ),
      sum_list(Ns, Sum)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),
  pairs_values(Pairs3, OrderedTags),
  setoff(
    Agent,
    literal_tag(Agent, _, _),
    Agents
  ),
  findall(
    [Tag,Sum|Ns],
    (
      member(Tag, OrderedTags),
      findall(
        N,
        (
          member(Agent, Agents),
          once((literal_tag(Agent, Tag, N) ; N = 0))
        ),
        Ns
      ),
      sum_list(Ns, Sum)
    ),
    Rows
  ),
  reply_html_page(
    app_style,
    title('DataHives - Language tags'),
    \html_table(
      [header_column(true),header_row(true)],
      `Overview of language tags.`,
      rdf_html_term,
      [['LangTag','Sum'|Agents]|Rows]
    )
  ).

dh_lit_tag_crawler(URL1):-
  default_url(URL0),
  default(URL1, URL0, URL2),
  init_agent(
    dh_random_walk,
    literal_tag,
    some_communication, %STUB
    URL2
  ).


literal_tag(_, _, _, To):-
  literal_tag(To).

literal_tag(literal(lang(Lang,_))):- !,
  increment_literal_tag(Lang).
literal_tag(literal(type(Datatype,_))):- !,
  increment_literal_tag(Datatype).
literal_tag(literal(_)):- !,
  increment_literal_tag(plain).
literal_tag(_).

increment_literal_tag(Tag):-
  thread_self(Alias),
  increment_literal_language_per_thread(Alias, Tag).

increment_literal_language_per_thread(Alias, Tag):-
  % @tbd Why does this not work?
  with_mutex(
    dh_test,
    (
      retract(literal_tag(Alias, Tag, N1)),
      N2 is N1 + 1,
      assert(literal_tag(Alias, Tag, N2))
    )
  ), !.
increment_literal_language_per_thread(Alias, Tag):-
  % @tbd Why does this not work?
  with_mutex(
    dh_test,
    assert(literal_tag(Alias, Tag, 1))
  ).
