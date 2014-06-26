:- module(gv_git_objects,
	  [ gv_create_blob_object/3,
	    gv_create_tree_object/3,
	    gv_create_commit_object/6,

	    % do we need to export these?
	    gv_graph_triples/2,
	    gv_tree_triples/2,
	    gv_restore_blobs/2,
	    gv_init_rdf/2,
	    gv_move_head/3
	  ]).

:- use_module(library(memfile)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(sha)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).

:- use_module(gv_namespaces).
:- use_module(gv_hash_uri).
:- use_module(hash_atom).
:- use_module(gv_git_io).
:- use_module(url_to_filename).

:- rdf_meta
	tree_triple_to_git(t,o).

%%	gv_create_blob_object(+Options, +Graph, -Blob) is det.
%
%	Snapshot of Graph is stored in Blob.

gv_create_blob_object(Options, Graph, Uri) :-
	setting(graph_version:gv_blob_store, DefaultStoreMode),
	option(gv_blob_store(StoreMode), Options, DefaultStoreMode),
	new_memory_file(MF),
	open_memory_file(MF, write, Out),
	(   rdf_statistics(triples_by_graph(Graph, _)) % FIXME: no longer needed in swipl 6.3.8
	->  rdf_save_canonical_turtle(Out, [graph(Graph), encoding(utf8)])
	;   true % empty graph, store empty file
	),
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

%%	gv_create_tree_object(+Triples, -TreeURI, +Options) is det.
%
%	Create new tree object described by Triples,
%	TreeURI is the Trusty URI of the created tree object

gv_create_tree_object(Triples, TreeURI, Options) :-
	setting(graph_version:gv_tree_store, DefaultStoreMode),
	option(gv_tree_store(StoreMode), Options,  DefaultStoreMode),
	maplist(tree_triple_to_git, Triples, Atoms),
	atomic_list_concat(Atoms, TreeContent),
	atom_length(TreeContent, Clen),
	format(atom(TreeObject), 'tree ~d\u0000~w', [Clen, TreeContent]),
	sha_hash(TreeObject, Sha, [encoding(octet)]),
	hash_atom(Sha, Hash),
	gv_hash_uri(Hash, TreeURI),
	(   (StoreMode == rdf_only ; StoreMode == both)
	->  gv_graph_triples(TreeURI, Triples)
	;   true
	),
	(   (StoreMode == git_only ; StoreMode == both)
	->  gv_store_git_object(Hash, TreeContent, [type(tree)|Options])
	;   true
	).
%%	gv_create_commit_object(+T, +P, +C, +M, -Commit, +O) is semidet.
%
%	Create a Commit object with the mandatory following properties:
%	* tree(T)
%	* parent(P)
%	* committer_url(C)
%	* comment(M).
%
%
%	Optional object in Options, along with the default values
%	* committer_email(mailto:no_email@example.com)
%	* committer_date(now)
%	* author_url(committer_url)
%	* author_email(committer_email)
%	* author_date(now)

gv_create_commit_object(Tree, Parent, CommitterURL, Comment, Commit, Options) :-
	setting(graph_version:gv_commit_store, DefaultStoreMode),
	option(gv_commit_store(StoreMode), Options, DefaultStoreMode),

	Comment = literal(CommitMessage),
	(   CommitMessage = ''
	->  CommentPair = []
	;   CommentPair = [ po(gv:comment, Comment) ]
	),

	(   option(committer_date(RDFTimeStamp), Options)
	->  literal_text(RDFTimeStamp, TS),
	    parse_time(TS, iso_8601, Now)
	;   get_time(Now),
	    format_time(atom(RDFTimeStamp), '%FT%T%:z', Now) % xsd dateTimeStamp ...
	),
	format_time(atom(GitTimeStamp), '%s %z',    Now), % Git time format

	DefaultMailTo='mailto:no_email@example.com',
	option(committer_email(CommitterMailto), Options, DefaultMailTo),
	option(author_email(AuthorMailto),       Options, CommitterMailto),
	option(author_url(AuthorURL),		Options, CommitterURL),
	sub_atom(CommitterMailto, 7, _, 0, CommitterEmail), % strip off mailto: part
	sub_atom(AuthorMailto,    7, _, 0, AuthorEmail),

	gv_hash_uri(TreeHash, Tree),
	(   Parent \= null
	->  gv_hash_uri(ParentHash, Parent),
	    format(atom(ParentLine), 'parent ~w~n', [ParentHash])
	;   ParentLine = ''
	),
	new_memory_file(MF),
	open_memory_file(MF, write, Out),
	format(Out,
	       'tree ~w~n~wauthor ~w <~w> ~w~ncommitter ~w <~w> ~w~n~n~w~n',
	       [TreeHash, ParentLine,
		AuthorURL, AuthorEmail, GitTimeStamp,
		CommitterURL, CommitterEmail, GitTimeStamp,
		CommitMessage]),
	close(Out),
	size_memory_file(MF, ByteSize, octet), % Git counts the size in bytes not chars!
	memory_file_to_atom(MF, GitCommitContent),
	free_memory_file(MF),
	format(atom(GitObject), 'commit ~d\u0000~w', [ByteSize, GitCommitContent]),
	sha_hash(GitObject, Sha, []),
	hash_atom(Sha, Hash),
	gv_hash_uri(Hash, Commit),
	(   (StoreMode == rdf_only ; StoreMode == both)
	->  RDFObject = [ po(gv:parent, Parent),
			  po(gv:tree, Tree),
			  po(gv:committer_url, CommitterURL),
			  po(gv:committer_email, CommitterMailto),
			  po(gv:committer_date, literal(type(xsd:dateTimeStamp, RDFTimeStamp))),
			  po(gv:author_url, AuthorURL),
			  po(gv_author_email, AuthorMailto),
			  po(gv:author_date, literal(type(xsd:dateTimeStamp, RDFTimeStamp)))
			| CommentPair
			],
	    rdf_global_term(RDFObject, Pairs),
	    rdf_transaction(
		forall(member(po(P,O), Pairs),
		       rdf_assert(Commit, P, O, Commit)))
	;   true
	),
	(   (StoreMode == git_only ; StoreMode == both)
	->  gv_store_git_object(Hash, GitCommitContent, [type(commit)|Options])
	;   true
	).

%%	gv_copy_graph(+Source, +Target) is det.
%
%	Copy graph Source to non-existing graph Target.
%	Don nothing if Target graph exists already.

gv_copy_graph(Source, Target) :-
	rdf_graph(Source),
	(   rdf_graph(Target)
	->  true
	;   gv_graph_triples(Source, Triples),
	    gv_graph_triples(Target, Triples)
	).

%%	gv_restore_blobs(+TreeTriples, +Mode) is semidet.
%
%	* Mode == graph: Restore Blobs into their associated named
%	graphs identified by IRI as defined the Tree triples in
%	TreeTriples.
%	* Mode == hash: Restore Blobs into their associated named graphs
%	identified by the Trusty (hash) URI by loading them from the git
%	repo.  (used when restoring the history from GIT).
gv_restore_blobs([], _) :- !.
gv_restore_blobs([rdf(_IRI, _P, Hash)|T], hash) :-
	rdf_graph(Hash),!,
	debug(gv, 'Blob graph ~p already exists, restore attempt ignored',
	      [Hash]),
	gv_restore_blobs(T, hash).
gv_restore_blobs([rdf(IRI,_P,Hash)|T], Mode) :-
	gv_graph_triples(Hash, Triples),
	(   Mode == graph
	->  gv_graph_triples(IRI, Triples)
	;   gv_graph_triples(Hash, Triples)
	),
	gv_restore_blobs(T, Mode).

%%	gv_graph_triples(+Graph, -Triples) is det.
%%	gv_graph_triples(+Graph, +Triples) is det.
%
%	When Triples are given, they are asserted to Graph.
%
%	When Graph is given, Triples are unified with the triples in
%	Graph.  If Graph is an existing Graph in the triple store and
%	graph_version:gv_blob_store is not set to git_only, the Triples
%	are copied from Graph. Otherwise, Graph is read back from the
%	corresponding Blob object in the git repository.

gv_graph_triples(Graph, Triples) :-
	nonvar(Triples),
	nonvar(Graph),!,
	(   rdf_graph(Graph) -> rdf_unload_graph(Graph); true ),
	rdf_transaction(
	    forall(member(rdf(S,P,O), Triples),
		   rdf_assert(S,P,O, Graph))).

gv_graph_triples(Graph, Triples) :-
	nonvar(Graph),
	\+ setting(graph_version:gv_blob_store, git_only),
	var(Triples),!,
	findall(rdf(S,P,O), rdf(S,P,O,Graph), Triples0),
	sort(Triples0, Triples).

gv_graph_triples(Blob, Triples) :-
	setting(graph_version:gv_blob_store, git_only),
	gv_hash_uri(Hash, Blob),
	gv_git_cat_file(Hash,Codes),
	atom_codes(TurtleAtom,Codes),
	format(atom(AnonPrefix), '__bnode_git_~w', [Hash]),
	rdf_read_turtle(atom(TurtleAtom), TriplesU, [anon_prefix(AnonPrefix)]),
	sort(TriplesU, Triples).

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


%%	gv_tree_triples(+Tree, -Triples) is semidet.
%
%	Unify Triples with the content of tree object Tree.
%	If Tree is a named graph in the triple store, just
%       unify with these triples.
%	If not, read Tree back from the corresponding tree object in the
%	git repository.

gv_tree_triples(null, []) :- !.
gv_tree_triples(Tree, Triples) :-
	nonvar(Tree),
	rdf_graph(Tree),
	\+ setting(graph_version:gv_tree_store, git_only),!,
	findall(rdf(S,P,O), rdf(S,P,O,Tree), Triples0),
	sort(Triples0, Triples).
gv_tree_triples(Tree, Triples) :-
	\+ setting(graph_version:gv_tree_store, rdf_only),!,
	gv_hash_uri(Hash, Tree),
	gv_parse_tree(Hash, TreeObject),
	maplist(git_tree_pair_to_triple, TreeObject, Triples).

%%	gv_init_rdf(+Ref, +Options) is det.
%
%	Make sure the current branch in the head graph points to Ref.

gv_init_rdf(Ref, Options) :-
	option(gv_refs_prefix(Refs), Options),
	atom_concat(Refs, 'HEAD', HEAD),
	rdf_assert(gv:current, gv:branch, Ref, HEAD).

%%	gv_move_head(+Branch, +NewHead, +Options) is det.
%
%	Advance head to commit NewHead.

gv_move_head(Branch, NewHead, Options) :-
	with_mutex(gv_head_mutex, gv_move_head_(Branch, NewHead, Options)).

gv_move_head_(Branch, NewHead, Options) :-
	setting(graph_version:gv_refs_store, DefaultStoreMode),
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
	setting(graph_version:gv_refs_prefix, Refs),
	atomic_concat(Refs, 'refs/heads', RefsHeads),
	rdf_retractall(Branch, gv:tip, _OldHead, RefsHeads),
	rdf_assert(    Branch, gv:tip,  NewHead, RefsHeads).
