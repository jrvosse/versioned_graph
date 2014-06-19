:- module(graph_version,
	  [gv_init/0,                % Initialize RDF/GIT repo if needed
	   gv_current_branch/1,      % -Branch, Branch is URI of current branch
	   gv_branch_head/2,         % +Branch, -HEAD, HEAD is Trusty URI of tip of Branch
	   gv_head/1,                % -HEAD is Trusty URI of tip of current branch
	   gv_checkout/0,	     % Load RDF named graphs from current HEAD
	   gv_checkout/1,	     % Restore named graphs from commit

	   gv_commit/5,
	   gv_resource_commit/4, % deprecated
	   gv_commit_property/2,
	   gv_diff/6,
	   gv_restore_rdf_from_git/0, % Restore HEAD + all objects
	   gv_restore_rdf_from_git/2,
	   gv_restore_git_from_rdf/0,
	   gv_restore_git_from_rdf/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).

:- use_module(gv_namespaces).
:- use_module(url_to_filename).
:- use_module(gv_git_objects).
:- use_module(gv_git_io).
:- use_module(gv_hash_uri).



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

:- setting(gv_head_graph, uri, 'http://localhost/git/HEAD/',
	   'Named graph for storing the HEAD in RDF').
:- setting(gv_refs_graph, uri, 'http://localhost/git/refs/',
	   'Named graph for storing the refs in RDF').

:- listen(settings(changed(graph_version:_Setting, _Old, _New)),
	  gv_init).


%%	git_init is det.
%
%       Initialise the RDF and/or GIT version repositories.
gv_init :-
	setting(gv_git_dir, Dir),
	setting(gv_blob_store,   BS),
	setting(gv_tree_store,   TS),
	setting(gv_commit_store, CS),
	setting(gv_refs_store,   RS),
	setting(gv_head_graph, HEAD),
	setting(gv_refs_graph, Refs),
	sort([BS,TS,CS,RS], StorageOpts),
	atom_concat(Refs, 'heads/master', Master),
	(   (StorageOpts == [git_only] ; rdf_graph(HEAD))
	->  true % no need to init RDF storage
	;   gv_init_rdf(Master)
	),

	(   (StorageOpts == [rdf_only] ; exists_directory(Dir) )
	->  true % no need to init git storage
	;   gv_init_git
	).

gv_init_rdf(Ref) :-
	setting(gv_head_graph, HEAD),
	rdf_assert(gv:current, gv:branch, Ref, HEAD).


:- gv_init.

%%	gv_current_branch(-Branch) is det.
%
%	Branch is unified with the branch name of the current branch.
%	Note: This should be the only triple in the HEAD named graph.

gv_current_branch(Branch) :-
	% assume current branch is stored in named graph HEAD:
	setting(gv_head_graph, HEAD),
	rdf(gv:current, gv:branch, Branch, HEAD),!.

gv_current_branch(Branch) :-
	\+ setting(gv_refs_store, rdf_only),
	setting(gv_refs_graph, Refs),
	% Assume current branch is the git symbolic ref HEAD:
	gv_current_branch_git(Ref),
	atomic_concat(Refs,Ref,Branch).

%%	gv_branch_head(+Branch, -Commit) is det.
%
%	Commit is unified with the tip of branch Branch.
gv_branch_head(Branch, Commit) :-
	\+ setting(gv_refs_store, git_only),
	rdf(Branch, gv:tip, Commit), !.

gv_branch_head(Branch, Commit) :-
	\+ setting(gv_refs_store, rdf_only),
	setting(gv_refs_graph, Refs),
	atom_length(Refs, RefsLen),
	sub_atom(Branch, RefsLen,_,0, Ref),
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

%%	gv_checkout is det.
%
%	Checkout the current HEAD

gv_checkout :-
	gv_head(HEAD),
	gv_checkout(HEAD).

%%	gv_checkout(Commit) is det.
%
%	Checkout the named graphs in the tree of Commit into the triple
%	store.

gv_checkout(Commit) :-
	% TODO: need to get repo in 'detached HEAD' state if commit \= HEAD ...
	gv_commit_property(Commit, tree(Tree)),
	gv_tree_triples(Tree, TreeTriples),

	setting(gv_blob_store,  BlobsStore),
	set_setting(gv_blob_store, git_only),
	load_blobs(TreeTriples, graph),
	set_setting(gv_blob_store,  BlobsStore).


%%	gv_commit_property(+Commit, -Prop) is det.
%
%	True if Prop unifies with a property of Commit.
%	Prop is of the form property_name(property_value).

gv_commit_property(null, tree(null)) :- !.

gv_commit_property(Commit, Prop) :-
	\+ setting(gv_commit_store, git_only),
	(   compound(Prop)
	->  Prop  =.. [Local, RDFValue],
	    rdf_global_id(gv:Local, RdfProp),
	    rdf(Commit, RdfProp, Value0, Commit),
	    literal_text(Value0, RDFValue),!
	;   rdf(Commit, RdfProp, Value0, Commit),
	    rdf_global_id(gv:Local, RdfProp),
	    literal_text(Value0, RDFValue),
	    Prop  =.. [Local, RDFValue]
	).

gv_commit_property(Commit, RDFProp) :-
	setting(gv_commit_store, git_only),
	compound(RDFProp),
	RDFProp	=.. [RDFPred, RDFValue],
	GitProp =.. [RDFPred, GitValue],
	gv_hash_uri(Hash, Commit),
	gv_commit_property_git(Hash, GitProp),
	(   memberchk(RDFPred, [parent, tree])
	->  gv_hash_uri(GitValue, RDFValue)
	;   GitValue = RDFValue
	),!.


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


%%	gv_move_head(+Branch, +NewHead, +Options) is det.
%
%	Advance head to commit NewHead.

gv_move_head(Branch, NewHead, Options) :-
	with_mutex(gv_head_mutex, gv_move_head_(Branch, NewHead, Options)).

gv_move_head_(Branch, NewHead, Options) :-
	setting(gv_refs_store, DefaultStoreMode),
	option(gv_refs_store(StoreMode), Options, DefaultStoreMode),
	(   (StoreMode == rdf_only ; StoreMode == both)
	->  gv_move_head_rdf(Branch, NewHead)
	;   true
	),
	(   (StoreMode == git_only ; StoreMode == both)
	->  gv_hash_uri(Hash, NewHead),
	    rdf_global_id(localgit:Local, Branch),
	    gv_move_head_git(Local, Hash)
	;   true
	).

gv_move_head_rdf(Branch, NewHead) :-
	setting(gv_refs_graph, Refs),
	atomic_concat(Refs, 'heads', RefsHeads),
	rdf_retractall(Branch, gv:tip, _OldHead, RefsHeads),
	rdf_assert(    Branch, gv:tip,  NewHead, RefsHeads).

%%      gv_resource_commit(+Graph, +Committer, +Comment, -Commit)
%
%       Commit Graph to the versioned graph storage.
%	The action is commited by creating a Commit object, this object
%	links with:
%	* gv:parent to the previous commit
%	* gv:tree to the tree representation of the current set of
%	  versioned graphs
%	* gv:committer_url to Committer
%	* gv:commiter_date to the current time
%	* gv:author_url to Committer
%	* gv:author_date to the current time
%	* gv:comment to Comment
%
%	Todo: Fix MT issues, just a mutex is not sufficient.
%	Needs true git-like branching model?
%	Fix email handling.

gv_resource_commit(Graph, Committer, Comment, Commit) :-
	with_mutex(gv_commit_mutex,
		   gv_commit_(
		       [Graph], Committer, Comment, Commit, [])).

gv_commit(Graphs, Committer, Comment, Commit, Options) :-
	with_mutex(gv_commit_mutex,
		   gv_commit_(
		       Graphs, Committer, Comment, Commit, Options)).

gv_commit_(Graphs0, Committer, Comment, Commit, Options0) :-
	is_list(Graphs0),
	sort(Graphs0, Graphs),
	setting(gv_git_dir, Dir),
	gv_current_branch(Branch),
	gv_branch_head(Branch, HEAD),
	gv_commit_property(HEAD, tree(CurrentTree)),
	Options=[directory(Dir)|Options0],
	maplist(gv_create_blob_object(Options),Graphs, Blobs),
	gv_add_blobs_to_tree(CurrentTree, Graphs, Blobs, NewTree, Options),
	(   CurrentTree \= NewTree
	->  true
	;   format(atom(Message), 'No changes on commit by ~w (~w)', [Committer, Comment]),
	    throw(nochange(gv_commit/5, Message))
	),

	gv_create_commit_object(NewTree, HEAD, Committer, Comment, Commit, Options),
	gv_move_head(Branch, Commit, Options).


ps(P,S, rdf(S,P,_)).
pso(P,S,O, rdf(S,P,O)).

gv_add_blobs_to_tree(Tree, Graphs, Blobs, NewTree, Options) :-
	rdf_equal(HashProp, gv:blob),
	maplist(ps(HashProp), Graphs, ToDelete),
	maplist(pso(HashProp), Graphs, Blobs, ToAdd),
	gv_tree_triples(Tree, OldTriples),
	subtract(OldTriples, ToDelete, Triples1), % ord_subtract does not unify
	append(Triples1, ToAdd, Triples2),
	sort(Triples2, NewTriples),
	gv_create_tree_object(NewTriples, NewTree, Options).

git_tree_pair_to_triple([hash(H),name(Senc)], rdf(Sdec,P,O)) :-
	rdf_equal(P, gv:blob),
	url_to_filename(Sdec, Senc),
	gv_hash_uri(H,O).


gv_tree_triples(null, []) :- !.
gv_tree_triples(Tree, Triples) :-
	nonvar(Tree),
	rdf_graph(Tree),
	\+ setting(gv_tree_store, git_only),!,
	findall(rdf(S,P,O), rdf(S,P,O,Tree), Triples0),
	sort(Triples0, Triples).
gv_tree_triples(Tree, Triples) :-
	\+ setting(gv_tree_store, rdf_only),!,
	gv_hash_uri(Hash, Tree),
	gv_parse_tree(Hash, TreeObject),
	maplist(git_tree_pair_to_triple, TreeObject, Triples).


load_blobs([], _) :- !.
load_blobs([rdf(_IRI, _P, Hash)|T], hash) :-
	rdf_graph(Hash),!,
	load_blobs(T, hash).
load_blobs([rdf(IRI,_P,Hash)|T], Mode) :-
	gv_graph_triples(Hash, Triples),
	(   Mode == graph
	->  gv_graph_triples(IRI, Triples)
	;   gv_graph_triples(Hash, Triples)
	),
	load_blobs(T, Mode).

store_blobs([], _) :- !.
store_blobs([rdf(_IRI,_P, Blob)|T], Options) :-
	gv_create_blob_object(Options, Blob, Blob),
	store_blobs(T, Options).

gv_restore_rdf_from_git :-
	gv_restore_rdf_from_git(
	    [commits(restore),
	     trees(restore),
	     blobs(restore)
	    ]).

gv_restore_rdf_from_git(Options) :-
	setting(gv_commit_store, CommitStore),
	setting(gv_tree_store,   TreeStore),
	setting(gv_blob_store,   BlobsStore),
	setting(gv_refs_store,   RefStore),

	set_setting(gv_commit_store, git_only),
	set_setting(gv_tree_store, git_only),
	set_setting(gv_blob_store, git_only),
	set_setting(gv_refs_store, git_only),

	gv_current_branch(Branch),
	gv_branch_head(Branch, Head),
	gv_init_rdf(Branch),  % make sure RDF is on branch too
	gv_move_head(Branch, Head, [gv_refs_store(rdf_only)]),
	gv_checkout(Head),
	gv_restore_rdf_from_git(Head, Options),

	set_setting(gv_commit_store, CommitStore),
	set_setting(gv_tree_store,   TreeStore),
	set_setting(gv_blob_store,   BlobsStore),
	set_setting(gv_refs_store,   RefStore).

gv_restore_rdf_from_git(Commit, Options) :-
	debug(gv, 'Restoring commit ~p', [Commit]),
	gv_commit_property(Commit, tree(Tree)),
	tree_to_rdf(Tree, Options),
	commit_to_rdf(Commit, Options),
	(   gv_commit_property(Commit, parent(Parent))
	->  gv_restore_rdf_from_git(Parent, Options)
	;   true
	).

gv_restore_git_from_rdf :-
	setting(graph_version:gv_git_dir, Dir),
	gv_restore_git_from_rdf(
	    [commits(restore),
	     trees(restore),
	     blobs(restore),
	     directory(Dir)
	    ]).

gv_restore_git_from_rdf(Options) :-
	setting(gv_commit_store, CommitStore),
	setting(gv_tree_store,   TreeStore),
	setting(gv_blob_store,   BlobsStore),
	setting(gv_refs_store,   RefStore),

	set_setting(gv_commit_store, rdf_only),
	set_setting(gv_tree_store, rdf_only),
	set_setting(gv_blob_store, rdf_only),
	set_setting(gv_refs_store, rdf_only),

	gv_current_branch(Branch),
	gv_branch_head(Branch, Head),
	gv_move_head(Branch, Head, [gv_refs_store(git_only)| Options]),
	gv_restore_git_from_rdf(Head, Options),

	set_setting(gv_commit_store, CommitStore),
	set_setting(gv_tree_store,   TreeStore),
	set_setting(gv_blob_store,   BlobsStore),
	set_setting(gv_refs_store,   RefStore).

gv_restore_git_from_rdf(Commit, Options) :-
	debug(gv, 'Restoring commit ~p', [Commit]),
	gv_commit_property(Commit, tree(Tree)),
	tree_to_git(Tree, Options),
	rdf_commit_to_git(Commit, Tree, Options),
	(   gv_commit_property(Commit, parent(Parent))
	->  gv_restore_git_from_rdf(Parent, Options)
	;   true
	).

tree_to_rdf(Tree, Options) :-
	gv_tree_triples(Tree, Triples),
	(   rdf_graph(Tree)
	->  true
	;
	    (	option(trees(ignore), Options)
	    ->  true
	    ;	gv_graph_triples(Tree, Triples)
	    )
	),
	(   option(blobs(ignore), Options)
	->  true
	;   load_blobs(Triples, hash)
	).

tree_to_git(Tree, Options) :-
	gv_tree_triples(Tree, Triples),
	(   option(trees(ignore), Options)
	->  true
	;   gv_create_tree_object(Triples, Tree, [gv_tree_store(git_only)|Options])
	),
	(   option(blobs(ignore), Options)
	->  true
	;   store_blobs(Triples, [gv_blob_store(git_only)|Options])
	).

commit_to_rdf(Commit, Options) :-
	(   rdf_graph(Commit)
	->  true
	;   option(commits(ignore), Options)
	->  true
	;   gv_hash_uri(Hash, Commit),
	    gv_parse_commit(Hash, CommitObject),
	    assert_commit_props(CommitObject, Commit)
	).

rdf_commit_to_git(Commit, Tree, Options) :-
	(   option(commits(ignore), Options)
	->  true
	;   gv_commit_property(Commit, comment(Comment)),
	    gv_commit_property(Commit, committer_url(Committer)),
	    (	gv_commit_property(Commit, parent(Parent))
	    ->	true
	    ;	Parent = null
	    ),
	    findall(P, gv_commit_property(Commit, P), CommitProps),
	    append([CommitProps, [gv_commit_store(git_only)], Options], AllOptions),
	    gv_create_commit_object(Tree, Parent, Committer, Comment, Commit, AllOptions)
	).


assert_commit_props([], _).
assert_commit_props([parent(null)|T], Graph) :-
	!,
	assert_commit_props(T, Graph).
assert_commit_props([H|T], Graph) :-
	H =.. [P,V],
	member(P, [comment, author_url, author_date, author_email,
		  committer_url, committer_date, committer_email]),
	!,
	rdf_global_id(gv:P, Pred),
        rdf_assert(Graph, Pred, V, Graph),
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


cliopatria:list_resource(URI) :-
	gv_hash_uri(_Hash, URI),
	\+ rdf_graph(URI),
	gv_graph_triples(URI, Triples), % load
	gv_graph_triples(URI, Triples), % save
	fail. % now do the normal thing.
