# DataHives

## Installation

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
    $ git submodule init
    $ git submodule update
    ~~~

## Startup

DataHives is started in the following way:

~~~
$ ./run --debug
~~~

The debug flag opens up a monitor of agent activity.

## Add an agent

~~~
?- dh_test(_).
~~~

This creates a single agent that randomly traverses the LOD cloud
using forward links.

