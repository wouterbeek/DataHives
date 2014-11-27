:- module(
  dh_lit_tag,
  [
    dh_lit_tag_crawler/1 % ?URL
  ]
).

/** <module> DataHives: Language tags

Count languages and datatypes used in literals.

@author Wouter Beek
@version 2014/02-2014/04, 2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

:- use_module(generics(meta_ext)).

:- use_module(plDcg(dcg_generics)).

:- use_module(plHtml(html_table)).

:- use_module(plServer(web_modules)).

:- use_module(plRdf(rdf_name)).

:- use_module(plTabular(rdf_html_table)).

:- use_module(dh(beh/nav/dh_random_walk)).
:- use_module(dh(web/dh_web_generics)).

:- http_handler(dh(literal_tag), dh_lit_tag, []).

user:web_module('DH LitTag', dh_lit_tag).

:- dynamic(literal_tag/3).

:- debug(dh).

default_url('http://dbpedia.org/resource/Banana').



dh_lit_tag(_Request):-
  aggregate_all(
    set(Tag),
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
  aggregate_all(
    set(Agent),
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
  user:current_html_style(HtmlStyle),
  reply_html_page(
    HtmlStyle,
    \dh_head(['Language tags']),
    \rdf_html_table(
      [header_column(true),header_row(true),location(dh_literal_tag)],
      html('Overview of language tags.'),
      [['Language tag','Sum'|Agents]|Rows]
    )
  ).

dh_lit_tag_crawler(Url):-
  default_url(DefaultUrl),
  default(DefaultUrl, Url),
  init_agent(
    dh_random_walk,
    literal_tag,
    default_action, %STUB
    Url
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

