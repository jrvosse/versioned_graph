:- module(graph_version,
	  [
	   gv_init/0,
	   gv_current_branch/1,
	   gv_branch_head/2,
	   gv_resource_commit/4,
	   gv_head/1,
	   gv_hash_uri/2,
	   gv_copy_graph/2,
	   gv_graph_triples/2,
	   gv_commit_property/2,
	   gv_diff/6,
	   gv_checkout/1
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).

:- use_module(url_to_filename).
:- use_module(hash_atom).
:- use_module(parse_git_objects).
:- use_module(gv_git_io).

:- rdf_register_ns(gv,       'http://semanticweb.cs.vu.nl/graph/version/').
:- rdf_register_ns(hash,     'http://semanticweb.cs.vu.nl/graph/hash/').
:- rdf_register_ns(localgit, 'http://localhost/git/').

:- setting(gv_git_dir, atom, 'gv.git',
	   'GIT repository for named graph snapshots').
:- setting(gv_blob_store,   oneof([git_only, rdf_only, both]), git_only,
	   'Where to store blob snapshots objects').
:- setting(gv_tree_store,   oneof([git_only, rdf_only, both]), git_only,
	   'Where to store tree snapshots objects').
:- setting(gv_commit_store, oneof([git_only, rdf_only, both]), git_only,
	   'Where to store commit objects').
:- setting(gv_refs_store, oneof([git_only, rdf_only, both]),   git_only,
	   'Where to store HEAD, refs etc.').

:- listen(settings(changed(graph_version:_Setting, _Old, _New)),
	  gv_init).

%%	gv_hash_uri(+Hash, -URI) is det.
%
%	URI is a uri constructed by concatenating the
%	Hash with some additional prefix to make it a
%	legal URI.
%
%	This provides a basic one to one mapping between
%       git's SHA1 hash ids and the URIs used in RDF.

gv_hash_uri(Hash, URI) :-
	nonvar(Hash), Hash \= null,
	!,
	atom_concat(x, Hash, Local),
	rdf_global_id(hash:Local, URI).

gv_hash_uri(Hash, URI) :-
	nonvar(URI),!,
	rdf_global_id(hash:Local, URI),
	atom_concat(x, Hash, Local).


%%	git_init is det.
%
%       Initialise the RDF and/or GIT version repositories.
gv_init :-
	setting(gv_git_dir, Dir),
	setting(gv_blob_store,   BS),
	setting(gv_tree_store,   TS),
	setting(gv_commit_store, CS),
	setting(gv_refs_store,   RS),
	sort([BS,TS,CS,RS], StorageOpts),
	(   (StorageOpts == [git_only] ; rdf_graph('HEAD'))
	->  true % no need to init RDF storage
	;   gv_init_rdf
	),

	(   (StorageOpts == [rdf_only] ; exists_directory(Dir) )
	->  true % no need to init git storage
	;   gv_init_git
	).


gv_init_rdf :-
	rdf_assert(gv:default, gv:branch, localgit:'refs/heads/master', 'HEAD').


:- gv_init.

%%	gv_current_branch(-Branch) is det.
%
%	Branch is unified with the branch name of the current branch.
%	Note: This should be the only triple in the HEAD named graph.

gv_current_branch(Branch) :-
	% assume current branch is stored in named graph HEAD:
	rdf(gv:default, gv:branch, Branch, 'HEAD'),!.

gv_current_branch(Branch) :-
	\+ setting(gv_refs_store, rdf_only),
	% Assume current branch is the git symbolic ref HEAD:
	gv_current_branch_git(Ref),
	rdf_global_id(localgit:Ref, Branch).

%%	gv_commit_property(+Commit, -Prop) is det.
%
%	True if Prop unifies with a property of Commit.
%	Prop is of the form property_name(property_value).

gv_commit_property(null, tree(null)) :- !.

gv_commit_property(Commit, Prop) :-
	\+ setting(gv_commit_store, git_only),
	Prop  =.. [Local, RDFValue],
	rdf_global_id(gv:Local, RdfProp),
	rdf(Commit, RdfProp, Value0, Commit),
	literal_text(Value0, RDFValue).
gv_commit_property(Commit, RDFProp) :-
	setting(gv_commit_store, git_only),
	RDFProp	=.. [RDFPred, RDFValue],
	GitProp =.. [RDFPred, GitValue],
	gv_hash_uri(Hash, Commit),
	gv_commit_property_git(Hash, GitProp),
	(   memberchk(RDFPred, [parent, tree])
	->  gv_hash_uri(GitValue, RDFValue)
	;   GitValue = RDFValue
	).


gv_diff(Commit1, null, [], OnlyIn1, [], []) :-
	gv_commit_property(Commit1, tree(Tree1)),
	gv_tree_triples(Tree1, Tree1Triples),
	gv_graphs_changed(Tree1Triples, [], [], OnlyIn1S, [], []),
	gv_triples_changed(OnlyIn1S, OnlyIn1).

gv_diff(null, Commit2, [], [], OnlyIn2, []) :-
	gv_commit_property(Commit2, tree(Tree2)),
	gv_tree_triples(Tree2, Tree2Triples),
	gv_graphs_changed([],Tree2Triples, [], [], OnlyIn2S, []),
	gv_triples_changed(OnlyIn2S, OnlyIn2).

gv_diff(Commit1, Commit2, Changed, OnlyIn1, OnlyIn2, UnChanged) :-
	gv_commit_property(Commit1, tree(Tree1)),
	gv_commit_property(Commit2, tree(Tree2)),
	gv_tree_triples(Tree1, Tree1Triples),
	gv_tree_triples(Tree2, Tree2Triples),
	gv_graphs_changed(Tree1Triples, Tree2Triples,
			   ChangedS, OnlyIn1S, OnlyIn2S, UnChanged),
	gv_triples_changed(OnlyIn1S, OnlyIn1),
	gv_triples_changed(OnlyIn2S, OnlyIn2),
	gv_triples_changed(ChangedS, Changed).

gv_triples_changed([], []).
gv_triples_changed([Head|Tail], [Graph-(Diff1,Diff2)|TailResult]) :-
	Head = Graph-(Blob1,Blob2),!,
	gv_graph_triples(Blob1, Triples1),
	gv_graph_triples(Blob2, Triples2),
	ord_intersection(Triples1, Triples2, Intersect, Diff2),
	ord_intersection(Triples2, Triples1, Intersect, Diff1),
	gv_triples_changed(Tail, TailResult).
gv_triples_changed([Head|Tail], [Graph-Triples|TailResult]) :-
	Head = Graph-Blob,!,
	gv_graph_triples(Blob, Triples),
	gv_triples_changed(Tail, TailResult).

gv_graphs_changed([], [], [], [], [], []).
gv_graphs_changed([rdf(S1,_,O1)|T], [],
		   Changed, [S1-O1|OI1], OI2, Same) :-
	gv_graphs_changed(T, [], Changed, OI1, OI2, Same).
gv_graphs_changed([], [rdf(S2,_,O2)|T],
		   Changed, OI1, [S2-O2|OI2], Same) :-
	gv_graphs_changed(T, [], Changed, OI1, OI2, Same).
gv_graphs_changed([rdf(S1,P1,O1)|T1], [rdf(S2,P2,O2)|T2],
		   Changed, OI1, OI2, Same)  :-
	compare(C,S1,S2),
	(   C == '<'
	->  gv_graphs_changed(T1, [rdf(S2,P2,O2)|T2],
			       Changed, OI1t, OI2, Same),
	    OI1 = [S1-O1|OI1t]
	;   C == '>'
	->  gv_graphs_changed([rdf(S1,P1,O1)|T1], T2,
			       Changed, OI1, OI2t, Same),
	    OI2 = [S2-O2|OI2t]
	;   (	O1 == O2
	    ->	gv_graphs_changed(T1, T2, Changed, OI1, OI2, Samet),
		Same = [S1|Samet]
	    ;	gv_graphs_changed(T1, T2, Changedt, OI1, OI2, Same),
		Changed = [S1-(O1,O2)|Changedt]
	    )
	).

%%	gv_branch_head(+Branch, -Commit) is det.
%
%	Commit is unified with the tip of branch Branch.
gv_branch_head(Branch, Commit) :-
	\+ setting(gv_refs_store, git_only),
	rdf(Branch, gv:tip, Commit, 'refs/heads'), !.

gv_branch_head(Branch, Commit) :-
	\+ setting(gv_refs_store, rdf_only),
	rdf_global_id(localgit:Ref, Branch),
	gv_branch_head_git(Ref, Hash),
	gv_hash_uri(Hash, Commit).

%%	gv_head(+Commit) is det.
%
%	Commit is the most recent commit on the current branch,
%	or the value 'null' if there are no current branches yet.

gv_head(Commit) :-
	gv_current_branch(B),
	gv_branch_head(B,Commit),
	!.

gv_head(null).

%%	gv_move_head(+NewHead) is det.
%
%	Advance head to commit NewHead.

gv_move_head(NewHead) :-
	with_mutex(gv_head_mutex, gv_move_head_(NewHead)).

gv_move_head_(NewHead) :-
	setting(gv_refs_store, StoreMode),
	gv_current_branch(Branch),
	(   (StoreMode == rdf_only ; StoreMode == both)
	->  rdf_retractall(Branch, gv:tip, _OldHead, 'refs/heads'),
	    rdf_assert(    Branch, gv:tip,  NewHead, 'refs/heads')
	;   true
	),
	(   (StoreMode == git_only ; StoreMode == both)
	->  gv_hash_uri(Hash, NewHead),
	    rdf_global_id(localgit:Local, Branch),
	    gv_move_head_git(Local, Hash)
	;   true
	).

%%      gv_resource_commit(+Graph, +Committer, +Comment, -Commit)
%
%       Commit Graph to the versioned graph storage.
%	The action is commited by creating a Commit object, this object
%	links with:
%	* gv:parent to the previous commit
%	* gv:tree to the tree representation of the current set of
%	  versioned graphs
%	* gv:committer_name to Committer
%	* gv:commiter_date to the current time
%	* gv:author_name to Committer
%	* gv:author_date to the current time
%	* gv:comment to Comment
%
%	Todo: Fix MT issues, just a mutex is not sufficient.
%	Needs true git-like branching model?
%	Fix email handling.

gv_resource_commit(Graph, Committer, Comment, Commit) :-
	with_mutex(gv_commit_mutex,
		   gv_resource_commit_(
		       Graph, Committer, Comment, Commit)).

gv_resource_commit_(Graph, Committer, Comment, Commit) :-
	setting(gv_git_dir, Dir),
	setting(gv_commit_store, StoreMode),
	Options=[directory(Dir)],
	gv_store_graph(Graph, BlobUri, Options),

	gv_head(HEAD),
	gv_commit_property(HEAD, tree(CurrentTree)),
	gv_add_blob_to_tree(CurrentTree, Graph, BlobUri, NewTree, Options),
	get_time(Now),
	format_time(atom(GitTimeStamp), '%s %z', Now), % Git format
	format_time(atom(RDFTimeStamp), '%s',    Now), % Hack format ...
	(   Comment = ''
	->  CommentPair = []
	;   CommentPair = [ po(gv:comment, literal(Comment)) ]
	),

	Email='no_email@example.com',

	RDFObject = [ po(rdf:type, gv:'Commit'),
		      po(gv:parent, HEAD),
		      po(gv:tree, NewTree),
		      po(gv:committer_name, Committer),
		      po(gv:committer_date, literal(RDFTimeStamp)),
		      po(gv:author_name, Committer),
		      po(gv:author_date, literal(RDFTimeStamp))
		    | CommentPair
		    ],
	gv_hash_uri(TreeHash, NewTree),
	(   gv_hash_uri(ParentHash, HEAD)
	->  format(atom(ParentLine), 'parent ~w~n', [ParentHash])
	;   ParentLine = ''
	),
	new_memory_file(MF),
	open_memory_file(MF, write, Out),
	format(Out,
	       'tree ~w~n~wauthor ~w <~w> ~w~ncommitter ~w <~w> ~w~n~n~w~n',
	       [TreeHash, ParentLine,
		Committer, Email, GitTimeStamp,
		Committer, Email, GitTimeStamp,
		Comment]),
	close(Out),
	size_memory_file(MF, ByteSize, octet), % Git counts the size in bytes not chars!
	memory_file_to_atom(MF, GitCommitContent),
	free_memory_file(MF),
	format(atom(GitObject), 'commit ~d\u0000~w', [ByteSize, GitCommitContent]),
	sha_hash(GitObject, Sha, []),
	hash_atom(Sha, Hash),
	gv_hash_uri(Hash, Commit),
	(   (StoreMode == rdf_only ; StoreMode == both)
	->  rdf_global_term(RDFObject, Pairs),
	    rdf_transaction(
		forall(member(po(P,O), Pairs),
		       rdf_assert(Commit, P, O, Commit)))
	;   true
	),
	(   (StoreMode == git_only ; StoreMode == both)
	->  gv_store_git_object(Hash, GitCommitContent, [type(commit)|Options])
	;   true
	),
	gv_move_head(Commit).


%%	gv_store_graph(+Graph, -Blob, +Options) is det.
%
%	Snapshot of Graph is stored in Blob.

gv_store_graph(Graph, Uri, Options) :-
	setting(gv_blob_store, StoreMode),
	new_memory_file(MF),
	open_memory_file(MF, write, Out),
	rdf_save_canonical_turtle(Out, [graph(Graph), encoding(utf8)]),
	close(Out),
	size_memory_file(MF, ByteSize, octet), % Git counts the size in bytes not chars!
	memory_file_to_atom(MF, Turtle),
	free_memory_file(MF),
	format(atom(Blob), 'blob ~d\u0000~w', [ByteSize, Turtle]),
	sha_hash(Blob, Sha, []),
	hash_atom(Sha, Hash),
	gv_hash_uri(Hash, Uri),
	(   (StoreMode == rdf_only ; StoreMode == both)
	->  gv_copy_graph(Graph, Uri)
	;   true
	),
	(   (StoreMode == git_only ; StoreMode == both)
	->  gv_store_git_object(Hash, Turtle, [type(blob)|Options])
	;   true
	).

%%	gv_add_blob_to_tree(+Tree,+Graph,+Blob,-NewTree,+Opts) is det.
%
%	Adds/replaces the entry of Graph in Tree to form NewTree.
%
gv_add_blob_to_tree(Tree, Graph, Uri, NewTree, Options) :-
	setting(gv_tree_store, StoreMode),
	gv_tree_triples(Tree, Triples0),
	rdf_equal(HashProp, gv:blob),
	(   selectchk(rdf(Graph, HashProp, _OldBlob), Triples0, Triples1)
	->  true
	;   Triples1 = Triples0
	),
	NewTriples0 =  [rdf(Graph, HashProp, Uri)|Triples1],
	sort(NewTriples0, NewTriples),
	maplist(tree_triple_to_git, NewTriples, Atoms),
	atomic_list_concat(Atoms, TreeContent),
	atom_length(TreeContent, Clen),
	format(atom(TreeObject), 'tree ~d\u0000~w', [Clen, TreeContent]),
	sha_hash(TreeObject, Sha, [encoding(octet)]),
	hash_atom(Sha, Hash),
	gv_hash_uri(Hash, NewTree),
	(   (StoreMode == rdf_only ; StoreMode == both)
	->  gv_graph_triples(NewTree, NewTriples)
	;   true
	),
	(   (StoreMode == git_only ; StoreMode == both)
	->  gv_store_git_object(Hash,TreeContent, [type(tree)|Options])
	;   true
	).

tree_triple_to_git(rdf(S,P,O), Atom) :-
	rdf_equal(P, gv:blob), % just checking ...
	gv_hash_uri(Hash, O),
	gv_hash_atom(Codes, Hash),
	url_to_filename(S, Filename),
	atom_codes(HashCode,Codes),
	format(atom(A), '100644 ~w\u0000', [Filename]),
	atom_concat(A, HashCode, Atom).

git_tree_pair_to_triple([hash(H),name(Senc)], rdf(Sdec,P,O)) :-
	rdf_equal(P, gv:blob),
	url_to_filename(Sdec, Senc),
	gv_hash_uri(H,O).





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
	nonvar(Graph),!,
	rdf_transaction(
	    forall(member(rdf(S,P,O), Triples),
		   rdf_assert(S,P,O, Graph))).

gv_graph_triples(Graph, Triples) :-
	nonvar(Graph),
	\+ setting(gv_blob_store, git_only),
	var(Triples),!,
	findall(rdf(S,P,O), rdf(S,P,O,Graph), Triples0),
	sort(Triples0, Triples).

gv_graph_triples(Blob, Triples) :-
	setting(gv_blob_store, git_only),
	gv_hash_uri(Hash, Blob),
	gv_git_cat_file(Hash,Codes),
	atom_codes(TurtleAtom,Codes),
	atom_to_memory_file(TurtleAtom, MF),
	open_memory_file(MF, read, Stream),
	rdf_read_turtle(stream(Stream), TriplesU, []),
	sort(TriplesU, Triples),
	free_memory_file(MF).

gv_tree_triples(null, []).
gv_tree_triples(Tree, Triples) :-
	nonvar(Tree),
	rdf_graph(Tree),
	\+ setting(gv_tree_store, git_only),
	findall(rdf(S,P,O), rdf(S,P,O,Tree), Triples0),
	sort(Triples0, Triples).
gv_tree_triples(Tree, Triples) :-
	\+ setting(gv_tree_store, rdf_only),
	gv_hash_uri(Hash, Tree),
	gv_parse_tree(Hash, TreeObject),
	gv_git_cat_file(Hash, Codes),
	phrase(tree(TreeObject), Codes),
	maplist(git_tree_pair_to_triple, TreeObject, Triples).

gv_checkout(Commit) :-
	% TODO: need to get repo in 'detached HEAD' state...
	gv_commit_property(Commit, tree(Tree)),
	gv_tree_triples(Tree, TreeTriples),
	load_blobs(TreeTriples, graph).

load_blobs([], _) :- !.
load_blobs([rdf(_Blob, _P, Hash)|T], hash) :-
	rdf_graph(Hash),!,
	load_blobs(T, hash).
load_blobs([rdf(Blob,_P,Hash)|T], Mode) :-
	gv_graph_triples(Hash, Triples),
	(   Mode == graph
	->  gv_graph_triples(Blob, Triples)
	;   gv_graph_triples(Hash, Triples)
	),
	load_blobs(T, Mode).


gv_restore_rdf_from_git :-
	setting(gv_commit_store, CommitStore),
	set_setting(gv_commit_store, git_only),

	gv_head(Head),
	gv_checkout(Head),
	gv_restore_rdf_from_git(Head),
	set_setting(gv_commit_store, CommitStore).

gv_restore_rdf_from_git(Commit) :-
	gv_commit_property(Commit, tree(Tree)),
	tree_to_rdf(Tree),
	commit_to_rdf(Commit),
	(   gv_commit_property(Commit, parent(Parent))
	->  gv_restore_rdf_from_git(Parent)
	;   true
	).

tree_to_rdf(Tree) :-
	(   rdf_graph(Tree)
	->  true
	;   gv_tree_triples(Tree, Triples),
	    gv_graph_triples(Tree, Triples)
	),
	load_blobs(Triples, hash).

commit_to_rdf(Commit) :-
	(   rdf_graph(Commit)
	->  true
	;   gv_hash_uri(Hash, Commit),
	    gv_git_cat_file(Hash, Codes),
	    phrase(commit(CommitObject), Codes),
	    assert_commit_props(CommitObject, Commit)
	).

assert_commit_props([], _).
assert_commit_props([parent(null)|T], Graph) :-
	!,
	assert_commit_props(T, Graph).
assert_commit_props([H|T], Graph) :-
	H =.. [P,V],
	member(P, [comment, author_name, author_date, author_email,
		  committer_name, committer_date, committer_email]),
	!,
	rdf_global_id(gv:P, Pred),
	rdf_assert(Graph, Pred, literal(V), Graph),
	assert_commit_props(T, Graph).
assert_commit_props([H|T], Graph) :-
	!,
	H =.. [P,V],
	(   member(P, [tree, parent])
	->  gv_hash_uri(V, Object),
	    rdf_global_id(gv:P, Pred),
	    rdf_assert(Graph, Pred, Object, Graph)
	;   assert_commit_props(V, Graph)
	),
	assert_commit_props(T, Graph).






