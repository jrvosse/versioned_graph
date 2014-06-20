:- module(graph_version,
	  [ gv_commit/5,	      % +GraphList, +Committer, +Message, -Commit, +Options
	    gv_checkout/0,	      % Load RDF named graphs from current HEAD
	    gv_checkout/1,	      % Restore named graphs from commit

	    gv_current_branch/1,      % -Branch, Branch is URI of current branch
	    gv_branch_head/2,         % +Branch, -HEAD, HEAD is Trusty URI of tip of Branch
	    gv_head/1,                % -HEAD is Trusty URI of tip of current branch

	    gv_resource_commit/4, % deprecated
	    gv_commit_property/2,
	    gv_diff/6,
	    gv_init/1		      % +Options
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).

:- use_module(gv_namespaces).
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
:- setting(gv_refs_prefix, uri, 'http://localhost/git/',
	   'Prefix for storing the local refs in RDF').

:- initialization(gv_init([])).

:- listen(settings(changed(graph_version:_Setting, _Old, _New)),
	  gv_init([])).

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


%%	git_init is det.
%
%       Initialise the RDF and/or GIT version repositories.
gv_init(Options) :-
	(   option(directory(Dir), Options)      -> true; setting(gv_git_dir, Dir)),
	(   option(gv_blob_store(BS), Options)   -> true; setting(gv_blob_store, BS)),
	(   option(gv_tree_store(TS), Options)   -> true; setting(gv_tree_store, TS)),
	(   option(gv_commit_store(CS), Options) -> true; setting(gv_commit_store, CS)),
	(   option(gv_refs_store(RS), Options)   -> true; setting(gv_refs_store, RS)),
	(   option(gv_refs_prefix(Refs), Options)-> true; setting(gv_refs_prefix, Refs)),

	sort([BS,TS,CS,RS], StorageOpts),
	gv_head_graph(HEAD),

	(   (StorageOpts == [git_only] ; rdf_graph(HEAD))
	->  true % no need to init RDF storage
	;   gv_init_rdf([gv_refs_prefix(Refs) | Options])
	),

	(   (StorageOpts == [rdf_only] ; exists_directory(Dir) )
	->  true % no need to init git storage
	;   gv_init_git([directory(Dir) | Options])
	).

gv_init_rdf(Options) :-
	option(gv_refs_prefix(Refs), Options),
	atom_concat(Refs, 'refs/heads/master', Ref),
	gv_init_rdf(Ref, Options).

gv_head_graph(Graph) :-
	setting(gv_refs_prefix, Refs),
	atom_concat(Refs, 'HEAD', Graph).

%%	gv_current_branch(-Branch) is det.
%
%	Branch is unified with the branch name of the current branch.
%	Note: This should be the only triple in the HEAD named graph.

gv_current_branch(Branch) :-
	% assume current branch is stored in named graph HEAD:
	gv_head_graph(HEAD),
	rdf(gv:current, gv:branch, Branch, HEAD),!.

gv_current_branch(Branch) :-
	\+ setting(gv_refs_store, rdf_only),
	setting(gv_refs_prefix, Refs),
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
	setting(gv_refs_prefix, Refs),
	atom_length(Refs, RefsLen),
	sub_atom(Branch, RefsLen,_,0, Ref),
	gv_branch_head_git(Ref, Hash),
	gv_hash_uri(Hash, Commit),!.

gv_branch_head(_, null) :- !.

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
	gv_load_blobs(TreeTriples, graph),
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


cliopatria:list_resource(URI) :-
	gv_hash_uri(_Hash, URI),
	\+ rdf_graph(URI),
	gv_graph_triples(URI, Triples), % load
	gv_graph_triples(URI, Triples), % save
	fail. % now do the normal thing.
