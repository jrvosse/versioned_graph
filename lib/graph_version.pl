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
	   gv_graph_triples/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(settings)).

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

%%	git_init is det.
%
%       Intialise the HEAD to refs/heads/master if no HEAD exist.
gv_init :-
	(   rdf_graph('HEAD')
	->  true
	;   rdf_assert(gv:default, gv:branch, localgit:'refs/heads/master', 'HEAD')
	).

%%	gv_current_branch(-Branch) is det.
%
%	Branch is unified with the branch name of the current branch.
%	Note: This should be the only triple in the HEAD named graph.
gv_current_branch(Branch) :-
	(   rdf(gv:default, gv:branch, Branch, 'HEAD')
	->  true
	;   gv_init,
	    rdf(gv:default, gv:branch, Branch, 'HEAD')
	).

%%	gv_branch_head(+Branch, -Commit) is det.
%
%	Commit is unified with the tip of branch Branch.
gv_branch_head(Branch, Commit) :-
	rdf(Branch, gv:tip, Commit, 'refs/heads'), !.

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
	    write(Out, Hash),
	    close(Out)
	).

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

	(   Comment = ''
	->  CommentPair = []
	;   CommentPair = [ po(rdfs:comment, literal(Comment)) ]
	),
	format_time(atom(TimeStamp), '%s %z', Now),

	RDFObject = [ po(rdf:type, gv:'Commit'),
			  po(gv:parent, HEAD),
			  po(gv:tree, NewTree),
			  po(dcterms:creator, Committer),
			  po(dcterms:date, literal(TimeStamp))
			  | CommentPair
			],
	gv_hash_uri(TreeHash, NewTree),
	(   gv_hash_uri(ParentHash, HEAD)
	->  format(atom(ParentLine), 'parent ~w~n', [ParentHash])
	;   ParentLine = ''
	),
	Email='fixme@example.com',
	format(atom(GitCommitContent),
	       'tree ~w~n~wauthor ~w <~w> ~w~n~w <~w> ~w~n~n~w',
	       [TreeHash, ParentLine,
		Committer, Email, TimeStamp,
		Committer, Email, TimeStamp,
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
