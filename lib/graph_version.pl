:- module(graph_version,
	  [
	   gv_init/0,
	   gv_current_branch/1,
	   gv_branch_head/2,
	   gv_resource_commit/4,
	   gv_head/1,
	   gv_hash_uri/2,
	   gv_compute_hash/2,
	   gv_copy_graph/2,
	   gv_graph_triples/2,
	   gv_commit_property/2,
	   gv_diff/6
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).
:- use_module(library(git)).

:- rdf_register_ns(gv,       'http://semanticweb.cs.vu.nl/graph/version/').
:- rdf_register_ns(hash,     'http://semanticweb.cs.vu.nl/graph/hash/').
:- rdf_register_ns(localgit, 'http://localhost/git/').

:- setting(gv_git_dir, atom, 'gv.git',
	   'GIT repository for named graph snapshots').
:- setting(gv_blob_store,   oneof([git_only, rdf_only, both]), rdf_only,
	   'Where to store blob snapshots objects').
:- setting(gv_tree_store,   oneof([git_only, rdf_only, both]), rdf_only,
	   'Where to store tree snapshots objects').
:- setting(gv_commit_store, oneof([git_only, rdf_only, both]), rdf_only,
	   'Where to store commit objects').
:- setting(gv_refs_store, oneof([git_only, rdf_only, both]), rdf_only,
	   'Where to store HEAD, refs etc.').

:- listen(settings(changed(graph_version:_Setting, _Old, _New)),
	  gv_init).

%%	git_init is det.
%
%       Initialise the RDF and/or GIT version repositories.
gv_init :-
	setting(gv_git_dir, Dir),
	setting(gv_blob_store, BS),
	setting(gv_tree_store, TS),
	setting(gv_commit_store, CS),
	sort([BS,TS,CS], StorageOpts),
	(   (StorageOpts == [git_only] ; rdf_graph('HEAD'))
	->  true % no need to init RDF storage
	;   gv_init(rdf)
	),

	(   (StorageOpts == [rdf_only] ; exists_directory(Dir) )
	->  true % no need to init git storage
	;   gv_init(git)
	).


gv_init(rdf) :-
	rdf_assert(gv:default, gv:branch, localgit:'refs/heads/master', 'HEAD').


gv_init(git) :-
	setting(gv_git_dir, Dir),
	directory_file_path(Dir, '.git', DotDir),
	directory_file_path(DotDir, objects, ObjectsDir),
	directory_file_path(DotDir, 'HEAD', HEAD),
	directory_file_path(DotDir, refs, RefsDir),
	directory_file_path(RefsDir, heads, RefsHeadsDir),
	make_directory_path(DotDir),
	make_directory_path(ObjectsDir),
	make_directory_path(RefsHeadsDir),
	open(HEAD, write, Out),
	write(Out, 'ref: refs/heads/master\n'),
	close(Out).

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
	setting(gv_git_dir, Dir),
	git(['symbolic-ref', 'HEAD'],[directory(Dir), output(OutCodes)]),!,
	atom_codes(RefNL, OutCodes),
	sub_atom(RefNL, 0, _, 1, Ref),
	rdf_global_id(localgit:Ref, Branch).

gv_current_branch(Branch) :-
	\+ setting(gv_refs_store, rdf_only),
	% as above, but without using git itself.
	% Assume current branch is in git file HEAD:
	setting(gv_git_dir, Dir),
	directory_file_path(Dir, '.git', DotDir),
	directory_file_path(DotDir, 'HEAD', HEAD),
	read_file_to_codes(HEAD, Codes, []),
	atom_codes(Atom, Codes),  %'ref: refs/heads/master\n'
	sub_atom(Atom, 5,_,1, Ref),
	rdf_global_id(localgit:Ref, Branch).


gv_commit_property(Commit, Prop) :-
	Prop =.. [Local, Value],
	rdf_global_id(gv:Local, RdfProp),
	(   rdf(Commit, RdfProp, Value0, Commit)
	->  literal_text(Value0, Value)
	;   Value=could_be_stored_in_git_not_implemented
	).

gv_diff(init, Commit2, [], [], OnlyIn2, []) :-
	gv_commit_property(Commit2, tree(Tree2)),
	gv_graph_triples(Tree2, Tree2Triples),
	gv_graphs_changed([],Tree2Triples, C, OnlyInOne, OnlyIn2, Unchanged),
	C= [],
	OnlyInOne = [],
	Unchanged = [],
	true.

gv_diff(Commit1, Commit2, Changed, OnlyIn1, OnlyIn2, UnChanged) :-
	gv_commit_property(Commit1, tree(Tree1)),
	gv_commit_property(Commit2, tree(Tree2)),
	gv_graph_triples(Tree1, Tree1Triples),
	gv_graph_triples(Tree2, Tree2Triples),
	gv_graphs_changed(Tree1Triples, Tree2Triples,
			   ChangedS, OnlyIn1, OnlyIn2, UnChanged),
	gv_triples_changed(ChangedS, Changed).

gv_triples_changed([], []).
gv_triples_changed([Head|Tail], [S-(Diff1,Diff2)|TailResult]) :-
	Head = S-(B1,B2),
	gv_graph_triples(B1, Triples1),
	gv_graph_triples(B2, Triples2),
	ord_intersection(Triples1, Triples2, Intersect, Diff2),
	ord_intersection(Triples2, Triples1, Intersect, Diff1),
	gv_triples_changed(Tail, TailResult).

gv_graphs_changed([], [], [], [], [], []).
gv_graphs_changed([rdf(S1,_,_)|T], [],
		   Changed, [S1|OI1], OI2, Same) :-
	gv_graphs_changed(T, [], Changed, OI1, OI2, Same).
gv_graphs_changed([], [rdf(S2,_,_)|T],
		   Changed, OI1, [S2|OI2], Same) :-
	gv_graphs_changed(T, [], Changed, OI1, OI2, Same).
gv_graphs_changed([rdf(S1,P1,O1)|T1], [rdf(S2,P2,O2)|T2],
		   Changed, OI1, OI2, Same)  :-
	compare(C,S1,S2),
	(   C == '<'
	->  gv_graphs_changed(T1, [rdf(S2,P2,O2)|T2],
			       Changed, OI1t, OI2, Same),
	    OI1 = [S1|OI1t]
	;   C == '>'
	->  gv_graphs_changed([rdf(S1,P1,O1)|T1], T2,
			       Changed, OI1, OI2t, Same),
	    OI2 = [S2|OI2t]
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
	rdf(Branch, gv:tip, Commit, 'refs/heads'), !.

gv_branch_head(Branch, Commit) :-
	\+ setting(gv_refs_store, rdf_only),
	% Assume current branch is in git refs file:
	setting(gv_git_dir, Dir),
	rdf_global_id(localgit:Ref, Branch),
	directory_file_path(Dir, '.git', DotDir),
	directory_file_path(DotDir, Ref, RefHead),
	exists_file(RefHead),
	read_file_to_codes(RefHead, Codes, []),
	atom_codes(Atom, Codes),  % hash with newline
	sub_atom(Atom, 0, 40 ,1, Hash),
	gv_hash_uri(Hash, Commit).

%%	gv_head(+Commit) is det.
%
%	Commit is the most recent commit on the current branch,
%	or the value 'init' if there are no current branches yet.

gv_head(Commit) :-
	gv_current_branch(B),
	gv_branch_head(B,Commit),
	!.

gv_head(init).

%%	gv_move_head(+NewHead) is det.
%
%	Advance head to commit NewHead.

gv_move_head(NewHead) :-
	with_mutex(gv_head_mutex, gv_move_head_(NewHead)).

gv_move_head_(NewHead) :-
	setting(gv_commit_store, StoreMode),
	setting(gv_git_dir, Dir),

	gv_current_branch(Branch),
	(   (StoreMode == rdf_only ; StoreMode == both)
	->  rdf_retractall(Branch, gv:tip, _OldHead, 'refs/heads'),
	    rdf_assert(    Branch, gv:tip,  NewHead, 'refs/heads')
	;   true
	),
	(   (StoreMode == git_only ; StoreMode == both)
	->  rdf_global_id(localgit:Local, Branch),
	    directory_file_path(Dir, '.git', DotGit),
	    directory_file_path(DotGit, Local, Filename),
	    gv_hash_uri(Hash, NewHead),
	    open(Filename, write, Out),
	    write(Out, Hash), nl(Out),
	    close(Out)
	;   true
	).

%%      gv_resource_commit(+Graph, +Committer, +Comment, -Commit)
%
%       Commit Graph to the versioned graph storage.
%	The action is commited by creating a Commit object, this object
%	links with:
%	* gv:parent to the previous commit
%	* gv:tree to the tree representation of the current
%	  version graphs
%	* gv:creator to Committer
%	* gv:comment to Comment
%	* gv:date to the current time
%
%	Todo: Fix MT issues, just a mutex is not sufficient.
%	Needs true git-like branching model?

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
	gv_tree(HEAD, CurrentTree),
	gv_add_blob_to_tree(CurrentTree, Graph, BlobUri, NewTree, Options),
	get_time(Now),
	format_time(atom(GitTimeStamp), '%s %z', Now),
	format_time(atom(RDFTimeStamp), '%Y-%m-%dT%H:%M:%S%Oz', Now),

	(   Comment = ''
	->  CommentPair = []
	;   CommentPair = [ po(gv:comment, literal(Comment)) ]
	),


	RDFObject = [ po(rdf:type, gv:'Commit'),
			  po(gv:parent, HEAD),
			  po(gv:tree, NewTree),
			  po(gv:creator, Committer),
			  po(gv:date, literal(RDFTimeStamp))
			  | CommentPair
			],
	gv_hash_uri(TreeHash, NewTree),
	(   gv_hash_uri(ParentHash, HEAD)
	->  format(atom(ParentLine), 'parent ~w~n', [ParentHash])
	;   ParentLine = ''
	),
	Email='no_email@example.com',
	format(atom(GitCommitContent),
	       'tree ~w~n~wauthor ~w <~w> ~w~ncommitter ~w <~w> ~w~n~n~w',
	       [TreeHash, ParentLine,
		Committer, Email, GitTimeStamp,
		Committer, Email, GitTimeStamp,
		Comment]),
	atom_length(GitCommitContent, Clen),
	format(atom(GitObject), 'commit ~d\u0000~w', [Clen, GitCommitContent]),
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
	->  gv_store_git_object(Hash, GitObject, Options)
	;   true
	),
	gv_move_head(Commit).


gv_store_git_object(Hash, Object, Options) :-
	sub_atom(Hash, 0, 2, 38, Subdir),
	sub_atom(Hash, 2, 38, 0, Local),
	option(directory(GitDir), Options),
	directory_file_path(GitDir, '.git/objects', GitObjects),
	directory_file_path(GitObjects, Subdir, Dir),
	directory_file_path(Dir,Local, File),
	(   exists_directory(Dir) -> true; make_directory(Dir)),
	open(File, write, Output, [type(binary)]),
	zopen(Output, Zout, []),
	write(Zout, Object),
	close(Zout).

%%	gv_tree(+Commit, -Tree) is semidet.
%%	gv_tree(-Commit, +Tree) is semidet.
%%	gv_tree(-Commit, -Tree) is nondet.
%
gv_tree(Commit, Tree) :-
	rdf(Commit, gv:tree, Tree), !.
gv_tree(init, init).

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
	->  gv_store_git_object(Hash, Blob, Options)
	;   true
	).

%%	gv_add_blob_to_tree(+Tree,+Graph,+Blob,-NewTree,+Opts) is det.
%
%	Adds/replaces the entry of Graph in Tree to form NewTree.
%
gv_add_blob_to_tree(Tree, Graph, Uri, NewTree, Options) :-
	setting(gv_tree_store, StoreMode),
	gv_graph_triples(Tree, Triples0),
	rdf_equal(HashProp, gv:blob),
	(   rdf(Graph, HashProp, OldBlob, Tree)
	->  selectchk(rdf(Graph, HashProp, OldBlob), Triples0, Triples1)
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
	->  gv_store_git_object(Hash,TreeObject, Options)
	;   true
	).

tree_triple_to_git(rdf(S,P,O), Atom) :-
	rdf_equal(P, gv:blob), % just checking ...
	gv_hash_uri(Hash, O),
	my_hash_atom(Codes, Hash),
	legal_filename(S, Filename),
	atom_codes(HashCode,Codes),
	format(atom(A), '100644 ~w\u0000', [Filename]),
	atom_concat(A, HashCode, Atom).


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
	format(atom(Out), 'blob ~d\u0000~w', [Clen, Content]),
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
	ground(Hash),!,
	atom_concat(x, Hash, Local),
	rdf_global_id(hash:Local, URI).
gv_hash_uri(Hash, URI) :-
	var(Hash),
	nonvar(URI),
	rdf_global_id(hash:Local, URI),
	atom_concat(x, Hash, Local).


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
	var(Triples),!,
	findall(rdf(S,P,O), rdf(S,P,O,Graph), Triples0),
	sort(Triples0, Triples).


%%	my_hash_atom(+Codes, -Hash) is det.
%       my_hash_atom(-Codes, +Hash) is det.
%
%       Bi-directional version of hash_atom/2 ...
%
my_hash_atom(Codes, Hash) :-
	nonvar(Codes),
	!,
	hash_atom(Codes, Hash).

my_hash_atom(Codes, Hash) :-
	nonvar(Hash),
	atom_chars(Hash, Chars),
	phrase(hex_bytes(Chars), Codes).

hex_bytes([]) --> [].
hex_bytes([High,Low|T]) -->
	{ char_type(High, xdigit(H)),
	  char_type(Low,  xdigit(L)),
	  Code is 16*H + L
	},
	[Code],
	hex_bytes(T).

legal_filename(Url, Filename) :-
	atom_chars(Url, UrlCodes),
	phrase(url_file(UrlCodes), FileCodes),
	atom_chars(Filename, FileCodes).


url_file([]) --> [].
url_file(['/'|T]) -->
	['%', '2', 'F'],
	url_file(T).
url_file([':'|T]) -->
	['%', '2', 'A'],
	url_file(T).
url_file([H|T]) -->
	[H],
	url_file(T).
