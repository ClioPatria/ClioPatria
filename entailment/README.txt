---+ Entailment reasoning modules

The ClioPatria system can deal with pluggable `entailment reasoners'.
Each reasoner must export the predicate rdf/3. This predicate must be
a `pure' predicate: it must accept any instantiation and it must
return consistent results, regardless of the instantiation.

New entailment modules can be added to load.pl

The user can use the  entailment   modules  for application reasoning by
importing the desired entailment module:

    ==
    :- use_module(entailment(rdfs_lite)).

    	...,
	rdf(S, rdf:type, rdfs:'Class'),
    ==
