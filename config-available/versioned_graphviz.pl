:- module(version_opm_graph, []).


:- use_module(cliopatria(hooks)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_abstract)).
:- use_module(library(graph_version)). % for gv: namespace

:- rdf_meta
        context_triple(r, t),
        transitive_context(r).

/* Conveys commit history by showing key medadata and parent/child (prev/next) commits
*/

cliopatria:context_graph(URI, RDF) :-
	rdf(URI, gv:graph, _),
	!,
	findall(T, commit_context_triple(URI, T), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	bagify_graph(RDF2, RDF3, Bags, []),	% Create bags of similar resources
	append(RDF3, Bags, RDF),
	RDF \= [].

commit_direct_context_triple(S, rdf(S,P,O)) :-
	rdf(S,P,O),
	\+ O = literal(_).

commit_context_triple(S, Triple) :-
	commit_direct_context_triple(S, Triple).
commit_context_triple(S1, Triple) :-
	rdf(S1,gv:parent, S2),
	commit_direct_context_triple(S2, Triple).
commit_context_triple(S1, Triple) :-
	rdf(S2,gv:parent, S1),
	commit_direct_context_triple(S2, Triple).
