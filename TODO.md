# To do list

## Caption

- already done
@ work in progress
+ to be add

## Nav :
	- Pick a random link
	@ Pick a link based on weighted random
		+ Discards to high pheromon nodes (cf Template)
		+ Discards hubs ?
		+ Find a way to discard irelevant nodes (heuristic)

## Act :
	- Display path
	- Apply http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/#rdfs-entailment to find some more triples

## Com :
	+ Leave pheromons
		+ Depending on the relevance of the path (based on found information ?)
			+ Closeness to a certain node (start node ?)
	  		+ Distance
				+ Number of paths between them (too expensive ?)
				+ Language label (and other labels)
			+ Importance of a node
				+ How many in-going / out-going links
				+ Seens a lot or not
	- Play with bactrack function to optimize efficiency
		- single alley predicate
		- rebirth of a dead agent.
		- devaluation with death of an agent
