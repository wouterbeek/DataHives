Agent properties
================

This document enumerates the kinds of properties an agent can have
in DataHives.


Core properties
---------------

Core properties are so basic that we do not represent them explicitly
in DataHives. These are the `graph` and `thread` properties,
that are assumed to have the agent IRI as their value.



Perdurant properties
--------------------

Perdurant properties are asserted in the global RDF graph, named `dh`.
They can be directly read, just as any other RDF triple is read.

The following perdurant properties are defined:
  - `creation`
    The `xsd:dateTime` at which an agent was created.



Endurent properties
-------------------

Endurant properties cannot be directly read,
but must be calculated at a specific moment in time.
They are represented by reified RDF triples.

