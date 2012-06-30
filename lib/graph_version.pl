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
:- use_module(library(settings)).
:- use_module(library(git)).

:- rdf_register_ns(gv,   'http://semanticweb.cs.vu.nl/graph/version/').
:- rdf_register_ns(hash, 'http://semanticweb.cs.vu.nl/graph/hash/').

:- setting(gv_git_dir, atom, 'gv.git',
	   'GIT repository for named graph snapshots').
:- setting(gv_blob_store,   oneof([git_only, rdf_only, both]), rdf_only,
	   'Where to store blob snapshots objects').
:- setting(gv_tree_store,   oneof([git_only, rdf_only, both]), rdf_only,
	   'Where to store tree snapshots objects').
:- setting(gv_commit_store, oneof([git_only, rdf_only, both]), rdf_only,
	   'Where to store commit objects').


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
	setting(gv_git_dir, Dir),
	Options = [directory(Dir)],
	gv_store_graph(Graph, BlobUri, Options),
	gv_head(HEAD),
	gv_tree(HEAD, CurrentTree),
	gv_add_blob_to_tree(CurrentTree, Graph, BlobUri, NewTree, Options),
	(   Comment = ''
	->  CommentPair = []
	;   CommentPair = [ po(rdfs:comment, literal(Comment)) ]
	),
	get_time(Now), format_time(atom(TimeStamp), '%s %z', Now),

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

%%	gv_store_graph(+Graph, -Blob, +Options) is det.
%
%	Snapshot of Graph is stored in Blob.

gv_store_graph(Graph, BlobUri, Options) :-
	setting(gv_blob_store, StoreMode),
	(   StoreMode = both
	->  gv_store_graph_(rdf_only, Graph, BlobUri,  Options),
	    gv_store_graph_(git_only, Graph, BlobUri2, Options),
	    assertion(BlobUri == BlobUri2)
	;   gv_store_graph_(StoreMode, Graph, BlobUri, Options)
	).

gv_store_graph_(rdf_only, Graph, Uri, _Options) :-
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
	gv_copy_graph(Graph, Uri).

gv_store_graph_(git_only, Graph, Uri, Options) :-
	tmp_file_stream(text, Tmp, Stream), close(Stream),
	rdf_save_canonical_turtle(Tmp, [graph(Graph), encoding(utf8)]),
	git(['hash-object', '-w', Tmp], [output(HashCodes)|Options]),
	atom_codes(HashN, HashCodes),
	sub_atom(HashN, 0, _, 1, Hash), % remove trailing new line ...
	gv_hash_uri(Hash, Uri).


%%	gv_add_blob_to_tree(+Tree,+Graph,+Blob,-NewTree,+Opts) is det.
%
%	Adds/replaces the entry of Graph in Tree to form NewTree.
%
gv_add_blob_to_tree(Tree, Graph, Uri, NewTree, Options) :-
	setting(gv_tree_store, StoreMode),
	(   StoreMode == both
	->  gv_add_blob_to_tree_(rdf_only, Tree, Graph, Uri, NewTree, Options),
	    gv_add_blob_to_tree_(git_only, Tree, Graph, Uri, NewTree1,Options),
	    assertion(NewTree == NewTree1)
	;   gv_add_blob_to_tree_(StoreMode,Tree, Graph, Uri, NewTree, Options)
	).


gv_add_blob_to_tree_(rdf_only, Tree, Graph, Uri, NewTree, _Options) :-
	gv_graph_triples(Tree, Triples0),
	rdf_equal(HashProp, gv:blob),
	(   rdf(Graph, HashProp, OldBlob, Tree)
	->  selectchk(rdf(Graph, HashProp, OldBlob), Triples0, Triples1)
	;   Triples1 = Triples0
	),
	NewTriples =  [rdf(Graph, HashProp, Uri)|Triples1],
	maplist(tree_triple_to_git, NewTriples, Atoms),
	atomic_list_concat(Atoms, TreeContent),
	atom_length(TreeContent, Clen),
	format(atom(TreeObject), 'tree ~w\0~w', [Clen, TreeContent]),
	sha_hash(TreeObject, Sha, [encoding(octet)]),
	hash_atom(Sha, Hash),
	gv_hash_uri(Hash, NewTree),
	gv_graph_triples(NewTree, NewTriples).

gv_add_blob_to_tree_(git_only, _Tree, Graph, Uri, NewTree, Options) :-
	gv_hash_uri(BlobHash, Uri),
	git(['update-index', '--add', '--cacheinfo', '100644', BlobHash, Graph],
	    Options),
	git(['write-tree'], [output(HashCodes)|Options]),
	atom_codes(HashN, HashCodes),
	sub_atom(HashN, 0, _, 1, Hash), % remove trailing new line ...
	gv_hash_uri(Hash, NewTree).

tree_triple_to_git(rdf(S,P,O), Atom) :-
	rdf_equal(P, gv:blob), % just checking ...
	gv_hash_uri(Hash, O),
	my_hash_atom(Codes, Hash),
	atom_codes(HashCode,Codes),
	format(atom(A), '100644 ~w\0', [S]),
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
	findall(rdf(S,P,O), rdf(S,P,O,Graph), Triples).


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
