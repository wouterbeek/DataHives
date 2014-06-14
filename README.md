# DataHives

Running agent simulations on the Web of Data / the Linked Open Data cloud.

![](https://raw.githubusercontent.com/wouterbeek/DataHives/master/dh_graph.png "Example graph of an agent's exploratory behavior.")

## Get started

### Installation

  1. Install the latest develop[ment version of SWI-Prolog.
    1. [Windows download](http://www.swi-prolog.org/download/daily/bin/)
    2. [Build process on Linux](http://www.swi-prolog.org/git.html),
       which has some
       [prerequisites](http://www.swi-prolog.org/build/LinuxDistro.html).
  2. Git clone this project:
    ~~~
    $ git clone https://github.com/wouterbeek/DataHives.git
    ~~~
  3. Install the project's Git submodules:
    ~~~
    $ cd DataHives
    $ git submodule update --init
    ~~~

### Startup

DataHives is started in the following way:

~~~
$ swipl load.pl
~~~

### Add an agent

The following creates a single agent that randomly traverses the LOD cloud
using forward links.

~~~
?- dh_test(X).
X = <URL-AT-WHICH-THE-AGENT-STARTED>
~~~

This can be repeated to add more agents.
The following adds multiple (e.g., 100) agents at once:

~~~{.pl}
?- forall(between(1, 100, _), dh_test(_)).
~~~

## Navigate-Act-Communicate

Agent behavior consists of the following three components:

  1. Navigate, ``Go somewhere''.
  2. Act, ``Do something''.
  3. Communicate, ``Tell outhers where you are and what you did''.

An agent's lifecycle consists of repeated execution of these three steps,
and is called the Navigate-Act-Communicate cycle.

### Navigate

Agents are able to navigate the entire Linked Open Data cloud.
The underlying system is able to retrieve links in
the Web of interconnected Linked Data based on queries to SPARQL enpoints,
IRI prefix resolutions, and full IRI dereferencing.

Linked Data is fetched from the LOD cloud and cached locally
for the agents to traverse. A Linked Data garbage collector removes
cached data that has not been visited by an agent in a while.

This searching, fetching, and retrieving of data all happens in
the background.

### Act

### Communicate

