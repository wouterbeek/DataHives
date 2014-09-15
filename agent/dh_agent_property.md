Agent properties
================

This document enumerates the kinds of properties an agent can have
in DataHives.


Core properties
---------------

Core properties are so basic that we do not represent them explicitly
in DataHives. These are the `graph` and `thread` properties,
that are assumed to have the agent IRI as their value.



Local/global properties
-----------------------

Local properties are stored within an agent thread.
Global properties are stoed within the generic agent environment.



Dynamic/static
--------------

Dynamic properties are a function of time.



Enumeration of all non-core properties
--------------------------------------

  - age: dynamic,local
  - cpuTime: dynamic,global
  - creation: static,global
  - cycles: dynamic,local
  - deductions: dynamic,local
  - effectiveness: dynamic,local
  - status: dynamic,global
  - steps: dynamic,local
