:- module(gv_replay,
	  [ gv_restore_rdf_from_git/0, % Restore HEAD + all objects
	    gv_restore_rdf_from_git/2,
	    gv_restore_git_from_rdf/0,
	    gv_restore_git_from_rdf/2
	  ]).

:- use_module(library(semweb/rdf_db)).

:- use_module(graph_version).
:- use_module(gv_git_objects).
:- use_module(gv_git_io).
:- use_module(gv_hash_uri).

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
	gv_init_rdf(Branch, Options),  % make sure RDF is on branch too
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
	git_commit_to_rdf(Commit, Options),
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
	;   gv_load_blobs(Triples, hash)
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

git_commit_to_rdf(Commit, Options) :-
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

store_blobs([], _) :- !.
store_blobs([rdf(_IRI,_P, Blob)|T], Options) :-
	gv_create_blob_object(Options, Blob, Blob),
	store_blobs(T, Options).


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
