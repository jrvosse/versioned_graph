:- module(graph_version,
	  [gv_resource_commit/5,
	   gv_resource_head/2,
	   gv_resource_graph/2,
	   gv_resource_last_user_commit/3,
	   gv_delete_old_graphs/0,
	   gv_hash_uri/2,
	   compute_hash/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).


:- rdf_register_ns(gv,   'http://semanticweb.cs.vu.nl/graph/version/').
:- rdf_register_ns(hash, 'http://semanticweb.cs.vu.nl/graph/hash/').

%%      gv_resource_commit(+Resource, +User, :Action, -Commit, -Graph)
%
%	Performs Action on the most recent versioned named graph
%	associated with Resource, and stores the result in Graph.
%	The action is commited by creating a Commit object, this object
%	links with:
%	* gv:parent to the previous commit
%	* gv:graph to Graph
%	* dc:creator to User
%	* dc:date to the current time

gv_resource_commit(Resource, User, Action, Commit, Graph) :-
	get_time(TimeStamp),
	gv_resource_head(Resource, ParentCommit),
	gv_resource_graph(ParentCommit, ParentGraph),
	create_merged_graph(ParentGraph, Action, Graph),
	CommitContent = [ po(rdf:type, gv:'Commit'),
			  po(gv:parent, ParentCommit),
			  po(gv:graph, Graph),
			  po(dcterms:creator, User),
			  po(dcterms:date, literal(TimeStamp), Commit)
			],
	rdf_global_term(CommitContent, Pairs0),
	sort(Pairs0, Pairs),
	variant_sha1(Pairs, Hash),
	gv_hash_uri('hash/commit_', Hash, Commit),
	gv_move_resource_head(Resource, Commit),
	rdf_transaction(
	    forall(member(po(P,O), Pairs),
		   rdf_assert(Commit, P, O, Commit))
		       ).


%%	gv_resource_head(+Resource, -Commit) is det.
%
%	Commit is the most recent commit on the versioned graphs
%	associated with Resource, or the value 'init' if there are no
%	versioned graphs associated with resource.

gv_resource_head(Resource, Commit) :-
	rdf(Commit, gv:head, Resource),
	!.
gv_resource_head(_, init).


%%	gv_resource_last_user_commit(+Resource, +User, -Commit)
%
%	Commit is the last Commit of User.

gv_resource_last_user_commit(Resource, User, Commit) :-
	rdf(Head, gv:head, Resource),
	gv_user_head(User, Head, Commit).

gv_user_head(User, Commit, UserHead) :-
	(   rdf(Commit, dcterms:creator, User)
	->  UserHead = Commit
	;   rdf(Commit, gv:parent, ParentCommit)
	->  gv_user_head(User, ParentCommit, UserHead)
	;   UserHead = init
	).

%%	gv_resource_graph(Commit, Graph) is det.
%
%	Graph is the versioned graph checked in by Commit, or the value
%	'init' if Commit == 'init'. The latter is ensures that every
%	Commit value generated by gv_resource_head/2 can be used as
%	the first argument to gv_resource_graph/2.

gv_resource_graph(init, init) :- !.
gv_resource_graph(Commit, Graph) :-
	rdf(Commit, gv:graph, Graph),!.

%%	gv_move_resource_head(+Resource, +Commit) is det.
%
%	Make Commit the new head of the branch associated with
%	Resource.
%
gv_move_resource_head(Resource, Commit) :-
	(   rdf(Parent, gv:head, Resource)
	->  rdf_retractall(Parent, gv:head, Resource, heads)
	;   true
	),
	rdf_assert(Commit, gv:head, Resource, heads).

create_merged_graph(OldGraph, Action, NewGraph) :-
	Action =.. [ Mode, Triples0],
	findall(rdf(S,P,O),
		rdf(S,P,O,OldGraph),
		OldTriples0),
	sort(OldTriples0, OldTriples),
	sort(Triples0, Triples),
	(   Mode = add
	->  ord_union(OldTriples, Triples, AllTriples)
	;   ord_subtract(OldTriples, Triples, AllTriples)
	),
	compute_hash(AllTriples, Hash),
	gv_hash_uri(Hash, NewGraph),
	rdf_transaction(forall(member(rdf(S,P,O), AllTriples),
			       rdf_assert(S,P,O,NewGraph))).

%%	compute_hash(+Triples, ?Hash) is det.
%
%	True of Hash is a SHA1 hash of the list of Triples.
%	Hash is computed using the same recipee git uses.
%	So, if one would run "git hash-object" on the
%       file containing the canonical turtle serialisation of
%       Triples, git would generate the same hash.


compute_hash(Triples, Hash) :-
	with_output_to(
	    atom(Content),
	    rdf_save_canonical_turtle(
		stream(current_output),
		[ expand(triple_in(Triples)),
		  encoding(wchar_t)])),
	write_length(Content, Clen, []),
	format(atom(Out), 'blob ~w\0~w', [Clen, Content]),
	sha_hash(Out, Sha, []),
	hash_atom(Sha, Hash).

triple_in(RDF, S,P,O,_G) :-
	member(rdf(S,P,O), RDF).


gv_delete_ancestors(init) :- !.
gv_delete_ancestors(Commit) :-
	(   rdf(Commit, gv:graph, Graph),
	    rdf(Commit, gv:parent, Parent)
	->  gv_delete_ancestors(Parent),
	    (rdf_graph(Commit) -> rdf_unload(Commit); true),
	    (rdf_graph(Graph)   -> rdf_unload(Graph); true)
	;   true
	).


gv_delete_old_graphs :-
	findall(P,
		(   rdf(Head, gv:head, _),
		    rdf(Head, gv:parent, P)
		),
		Parents),
	forall(member(H, Parents),
	       gv_delete_ancestors(H)
	      ).


%%	gv_hash_uri(+Prefix, +Hash, -URI) is det.
%
%	URI is a uri constructed by concatenating the
%	Prefix and Hash, with some additional prefix to
%	legal URI.

gv_hash_uri(Prefix, Hash, URI) :-
	atom_concat(Prefix,Hash, Local),
	rdf_global_id(hash:Local, URI).

gv_hash_uri(Hash, URI) :-
	atom_concat(x, Hash, Local),
	rdf_global_id(hash:Local, URI).
