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
:- http_handler(dh(lit_tag), dh_lit_tag, []).

:- web_module_add('DH LitTag', dh_lit_tag).

:- dynamic(literal_tag/3).

default_url('http://dbpedia.org/resource/Banana').



dh_lit_tag(_Request):-
  setoff(
    Lang,
    literal_tag(_, Lang, _),
    Langs
  ),
  findall(
    Sum-Lang,
    (
      member(Lang, Langs),
      findall(
        N,
        literal_tag(_, Lang, N),
        Ns
      ),
      sum_list(Ns, Sum)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),
  pairs_values(Pairs3, OrderedLangs),
  findall(
    [Lang,Sum|Ns],
    (
      member(Lang, OrderedLangs),
      member(Sum-Lang, Pairs1),
      findall(
        N,
        literal_tag(_, Lang, N),
        Ns
      )
    ),
    Rows
  ),
  (
    once(member(ArbitraryLang, OrderedLangs))
  ->
    findall(
      Alias,
      literal_tag(Alias, ArbitraryLang, _),
      Aliases
    )
  ;
    Aliases = []
  ),
  reply_html_page(
    app_style,
    title('DataHives - Language tags'),
    \html_table(
      [header_column(true),header_row(true)],
      `Overview of language tags.`,
      rdf_html_term,
      [['LangTag','Sum'|Aliases]|Rows]
    )
  ).


dh_lit_tag_crawler(URL1):-
  default_url(URL0),
  default(URL1, URL0, URL2),
  init_agent(
    dh_random_walk,
    lit_tag,
    some_communication, %STUB
    URL2
  ).


lit_tag(_, _, _, To):-
  lit_tag(To).

lit_tag(literal(lang(Lang,_))):- !,
  increment_literal_language(Lang).
lit_tag(literal(type(Datatype,_))):- !,
  increment_literal_language(Datatype).
lit_tag(literal(_)):- !,
  increment_literal_language(plain).
lit_tag(_, _).

increment_literal_language(Lang):-
  thread_self(Alias),
  increment_literal_language_per_thread(Alias, Lang).

increment_literal_language_per_thread(Alias, Lang):-
  % @tbd Why does this not work?
  with_mutex(
    dh_test,
    (
      retract(literal_tag(Alias, Lang, N1)),
      N2 is N1 + 1,
      assert(literal_tag(Alias, Lang, N2))
    )
  ), !.
increment_literal_language_per_thread(Alias, Lang):-
  % @tbd Why does this not work?
  with_mutex(
    dh_test,
    assert(literal_tag(Alias, Lang, 1))
  ).

