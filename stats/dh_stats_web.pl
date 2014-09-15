:- module(
  dh_stats_web,
  [
    dh_stats_web/2 % +Request:list(nvpair)
                   % +HtmlStyle
  ]
).

/** <module> DataHives Statistics: Web

Web-based front-end for statistics in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_path)).

:- use_module(generics(request_ext)).

:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_web(dh_web_generics)).



% GET html *
dh_stats_web(Request, HtmlStyle):-
  cors_enable,
  request_filter(Request, get, _/html, Root),
  http_absolute_uri(dh_stats(.), Root), !,
  reply_html_page(
    HtmlStyle,
    \dh_stats_head(['']),
    \dh_body(
      form(
        [action=Root,class=['pure-form'],id=statisticsForm,method=post],
        fieldset([
          legend('Perform measurement for statistics'),
          'Show',
          select(id=yProperties, []),
          'per',
          select(id=xProperties, []),
          'for',
          select(
            id=scopes,
            [
              option(value=agent, agent),
              option(value=agentDefinition, 'agent definition'),
              option(value=population, population)
            ]
          ),
          select(id=subscopes, []),
          button(
            [
              class=['pure-button','pure-button-primary'],
              id=submitBtn,
              type=submit
            ],
            ['Submit']
          )
        ])
      )
    )
  ).



% Helpers

dh_stats_head(Substrings) -->
  html(\dh_head(['Statistics'|Substrings])).

