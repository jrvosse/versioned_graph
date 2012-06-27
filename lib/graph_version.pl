:- module(graph_version,
	  [gv_resource_commit/4,
	   gv_head/1,
	   gv_hash_uri/2,
	   gv_compute_hash/2,
	   gv_copy_graph/2,
	   gv_graph_triples/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).


:- rdf_register_ns(gv,   'http://semanticweb.cs.vu.nl/graph/version/').
:- rdf_register_ns(hash, 'http://semanticweb.cs.vu.nl/graph/hash/').

%%      gv_resource_commit(+Graph, +Committer, +Comment, -Commit)
%
%       Commit Graph to the versioned graph storage.
%	The action is commited by creating a Commit object, this object
%	links with:
%	* gv:parent to the previous commit
%	* gv:tree to the tree representation of the current
%	  version graphs
%	* dc:creator to Committer
%	* dc:date to the current time
%
%	Todo: Fix MT issues, just a mutex is not sufficient.
%	Needs true git-like branching model?

gv_resource_commit(Graph, Committer, Comment, Commit) :-
	with_mutex(gv_commit_mutex,
		   do_gv_resource_commit(
		       Graph, Committer, Comment, Commit)).

do_gv_resource_commit(Graph, Committer, Comment, Commit) :-
	get_time(TimeStamp),
	gv_store_graph(Graph, Blob),
	gv_head(HEAD),
	gv_tree(HEAD, CurrentTree),
	gv_add_blob_to_tree(CurrentTree, Graph, Blob, NewTree),

	(   Comment = ''
	->  CommentPair = []
	;   CommentPair = [ po(rdfs:comment, literal(Comment)) ]
	),
	CommitContent = [ po(rdf:type, gv:'Commit'),
			  po(gv:parent, HEAD),
			  po(gv:tree, NewTree),
			  po(dcterms:creator, Committer),
			  po(dcterms:date, literal(TimeStamp), Commit)
			  | CommentPair
			],
	rdf_global_term(CommitContent, Pairs0),
	sort(Pairs0, Pairs),
	variant_sha1(Pairs, Hash),
	gv_hash_uri(Hash, Commit),
	rdf_transaction(
	    forall(member(po(P,O), Pairs),
		   rdf_assert(Commit, P, O, Commit))),
	gv_move_head(Commit).

%%	gv_head(+Commit) is det.
%
%	Commit is the most recent commit on the current branch,
%	or the value 'init' if there are no current branch yet.

gv_head(Commit) :-
	rdf(gv:default, gv:head, Commit, 'HEAD'),
	!.
gv_head(init) :-
	rdf_assert(gv:default, gv:head, init, 'HEAD').

%%	gv_move_head(+NewHead) is det.
%
%	Advance head to commit NewHead.

gv_move_head(NewHead) :-
	with_mutex(gv_head_mutex,
		   (   rdf_unload('HEAD'),
		       rdf_assert(gv:default, gv:head, NewHead, 'HEAD'))).

%%	gv_tree(+Commit, -Tree) is semidet.
%%	gv_tree(-Commit, +Tree) is semidet.
%%	gv_tree(-Commit, -Tree) is nondet.
%
gv_tree(Commit, Tree) :-
	rdf(Commit, gv:tree, Tree), !.
gv_tree(init, init).

%%	gv_store_graph(+Graph, -Blob) is det.
%
%	Snapshot of Graph is stored in Blob.

gv_store_graph(Graph, Blob) :-
	with_output_to(
	    atom(Content),
	    rdf_save_canonical_turtle(
		stream(current_output),
		[graph(Graph),
		 encoding(wchar_t)
		])),
	write_length(Content, Clen, []),
	format(atom(Out), 'blob ~w\0~w', [Clen, Content]),
	sha_hash(Out, Sha, []),
	hash_atom(Sha, Hash),
	gv_hash_uri(Hash, Blob),
	gv_copy_graph(Graph, Blob).

%%	gv_add_blob_to_tree(+Tree, +Graph, +Blob, -NewTree) is det.
%
%	Adds/replaces the entry of Graph in Tree to form NewTree.
%
gv_add_blob_to_tree(Tree, Graph, Blob, NewTree) :-
	gv_graph_triples(Tree, Triples0),
	rdf_equal(HashProp, gv:hash),
	(   rdf(Graph, HashProp, OldBlob, Tree)
	->  selectchk(rdf(Graph, HashProp, OldBlob), Triples0, Triples1)
	;   Triples1 = Triples0
	),
	NewTriples =  [rdf(Graph, HashProp, Blob)|Triples1],
	gv_compute_hash(NewTriples, Hash),
	gv_hash_uri(Hash, NewTree),
	gv_graph_triples(NewTree, [rdf(Graph, HashProp, Blob)|Triples1]).

%%	gv_compute_hash(+Triples, ?Hash) is det.
%
%	True of Hash is a SHA1 hash of the list of Triples.
%	Hash is computed using the same recipee git uses.
%	So, if one would run "git hash-object" on the
%       file containing the canonical turtle serialisation of
%       Triples, git would generate the same hash.


gv_compute_hash(Triples, Hash) :-
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

%%	gv_hash_uri(+Hash, -URI) is det.
%
%	URI is a uri constructed by concatenating the
%	Hash with some additional prefix to make it a
%	legal URI.


gv_hash_uri(Hash, URI) :-
	atom_concat(x, Hash, Local),
	rdf_global_id(hash:Local, URI).


%%	gv_copy_graph(+Source, +Target) is det.
%
%	Copy graph Source to graph Target.

gv_copy_graph(Source, Target) :-
	gv_graph_triples(Source, Triples),
	gv_graph_triples(Target, Triples).

%%	gv_graph_triples(+Graph, -Triples) is det.
%%	gv_graph_triples(+Graph, +Triples) is det.
%
%	When Triples are given, they are asserted to Graph,
%       otherwise, Triples are unified with the triples in Graph.

gv_graph_triples(Graph, Triples) :-
	nonvar(Triples),
	nonvar(Graph),
	rdf_transaction(
	    forall(member(rdf(S,P,O), Triples),
		   rdf_assert(S,P,O, Graph))).

gv_graph_triples(Graph, Triples) :-
	nonvar(Graph),
	var(Triples),
	findall(rdf(S,P,O), rdf(S,P,O,Graph), Triples).
