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
:- use_module(library(semweb/rdf_turtle)).
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
:- setting(gv_refs_store, oneof([git_only, rdf_only, both]),   rdf_only,
	   'Where to store HEAD, refs etc.').

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
	sort([BS,TS,CS,RS], StorageOpts),
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


gv_commit_property(null, tree(null)).

gv_commit_property(Commit, Prop) :-
	Prop  =.. [Local, RDFValue],
	rdf_global_id(gv:Local, RdfProp),
	rdf(Commit, RdfProp, Value0, Commit),
	literal_text(Value0, RDFValue).

gv_commit_property(Commit, RDFProp) :-
	RDFProp	=.. [RDFPred, RDFValue],
	setting(gv_git_dir, Dir),
	gv_hash_uri(Hash, Commit),
	catch(git(['cat-file', '-p', Hash],[directory(Dir), output(Codes)]),_,fail),
	phrase(commit(CommitObject), Codes),
	(   memberchk(RDFPred, [parent, tree])
	->  GitProp =.. [RDFPred, GitValue],
	    option(GitProp, CommitObject),
	    gv_hash_uri(GitValue, RDFValue)
	;   RDFPred = comment
	->  option(RDFProp, CommitObject)
	;   memberchk(RDFPred, [committer_name, committer_date, committer_email])
	->  option(committer(C), CommitObject),
	    option(RDFProp, C)
	).

gv_diff(Commit1, null, Changed, OnlyIn1, OnlyIn2, Same) :-
	gv_diff(null, Commit1, Changed, OnlyIn2, OnlyIn1, Same).

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
%	* gv:committer_name to Committer
%	* gv:author_name to Committer
%	* gv:comment to Comment
%	* gv:commiter_date to the current time
%	* gv:author_date to the current time
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
	format(atom(GitCommitContent),
	       'tree ~w~n~wauthor ~w <~w> ~w~ncommitter ~w <~w> ~w~n~n~w~n',
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
	->  gv_store_git_object(Hash,TreeObject, Options)
	;   true
	).

tree_triple_to_git(rdf(S,P,O), Atom) :-
	rdf_equal(P, gv:blob), % just checking ...
	gv_hash_uri(Hash, O),
	my_hash_atom(Codes, Hash),
	url_to_filename(S, Filename),
	atom_codes(HashCode,Codes),
	format(atom(A), '100644 ~w\u0000', [Filename]),
	atom_concat(A, HashCode, Atom).

git_tree_pair_to_triple([hash(H),name(Senc)], rdf(Sdec,P,O)) :-
	rdf_equal(P, gv:blob),
	url_to_filename(Sdec, Senc),
	gv_hash_uri(H,O).


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
	ground(Hash), Hash \= null,
	!,
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
	\+ setting(gv_blob_store, git_only),
	var(Triples),!,
	findall(rdf(S,P,O), rdf(S,P,O,Graph), Triples0),
	sort(Triples0, Triples).

gv_graph_triples(Blob, Triples) :-
	setting(gv_blob_store, git_only),
	setting(gv_git_dir, Dir),
	gv_hash_uri(Hash, Blob),
	catch(git(['cat-file', '-p', Hash],
		  [directory(Dir), output(Codes)]),
	      _,
	      fail),
	atom_codes(TurtleAtom, Codes),
	atom_to_memory_file(TurtleAtom, MF),
	open_memory_file(MF, read, Stream),
	rdf_read_turtle(stream(Stream), Triples, []),
	free_memory_file(MF).

gv_tree_triples(null, []).
gv_tree_triples(Tree, Triples) :-
	nonvar(Tree),
	rdf_graph(Tree),
	\+ setting(gv_tree_store, git_only),
	findall(rdf(S,P,O), rdf(S,P,O,Tree), Triples0),
	sort(Triples0, Triples).
gv_tree_triples(Tree, Triples) :-
	\+ setting(gv_tree_store, git_only),
	setting(gv_git_dir, Dir),
	gv_hash_uri(Hash, Tree),
	catch(git(['cat-file', '-p', Hash],
		  [directory(Dir), output(Codes)]),
	      _,
	      fail),
	phrase(tree(TreeObject), Codes),
	maplist(git_tree_pair_to_triple, TreeObject, Triples).


%%	url_to_filename(+URL, -FileName) is det.
%%	url_to_filename(-URL, +FileName) is det.
%
%	Turn  a  valid  URL  into  a  filename.  Earlier  versions  used
%	www_form_encode/2, but this can produce  characters that are not
%	valid  in  filenames.  We  will  use    the   same  encoding  as
%	www_form_encode/2,  but  using  our  own    rules   for  allowed
%	characters. The only requirement is that   we avoid any filename
%	special character in use.  The   current  encoding  use US-ASCII
%	alnum characters, _ and %
%
%	Code copied from rdf_persistency:url_to_filename/2
%	on July 16 2012.

url_to_filename(URL, FileName) :-
	atomic(URL), !,
	atom_codes(URL, Codes),
	phrase(url_encode(EncCodes), Codes),
	atom_codes(FileName, EncCodes).
url_to_filename(URL, FileName) :-
	www_form_encode(URL, FileName).

url_encode([0'+|T]) -->
	" ", !,
        url_encode(T).
url_encode([C|T]) -->
	alphanum(C), !,
	url_encode(T).
url_encode([C|T]) -->
	no_enc_extra(C), !,
	url_encode(T).
url_encode(Enc) -->
	(   "\r\n"
	;   "\n"
	), !,
	{ append("%0D%0A", T, Enc)
	},
	url_encode(T).
url_encode([]) -->
	eos, !.
url_encode([0'%,D1,D2|T]) -->
	[C],
	{ Dv1 is (C>>4 /\ 0xf),
	  Dv2 is (C /\ 0xf),
	  code_type(D1, xdigit(Dv1)),
	  code_type(D2, xdigit(Dv2))
	},
	url_encode(T).

eos([], []).

alphanum(C) -->
	[C],
	{ C < 128,			% US-ASCII
	  code_type(C, alnum)
	}.

no_enc_extra(0'_) --> "_".



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

hex_bytes([High,Low|T]) -->
	{ char_type(High, xdigit(H)),
	  char_type(Low,  xdigit(L)),
	  Code is 16*H + L
	},
	[Code],
	hex_bytes(T).
hex_bytes([]) --> [].

commit(Commit) -->
	tree_line(T),
	parent(P),
	author(AName, AEmail, ADate),
	committer(CName, CEmail, CDate),
	comment(CM),!,
	{
	 Commit = [
		   tree(T),
		   parent(P),
		   author([ author_name(AName),
			    author_email(AEmail),
			    author_date(ADate)
			  ]),
		   committer([committer_name(CName),
			      committer_email(CEmail),
			      committer_date(CDate)]),
		   comment(CM)
		  ]
	}.

tree_line(T) -->
	[116, 114, 101, 101, 32],
	hash(T),
	[10].

parent(P) -->
	[112, 97, 114, 101, 110, 116, 32],
	hash(P),
	[10].
parent(null) --> [].

author(Name,Email,Date) -->
	[97, 117, 116, 104, 111, 114, 32],
	name(NameC),
	[32, 60], author_email(EmailC), [62, 32],
	author_date(DateC,_ZoneC),
	[10],
	{
	 atom_codes(Name, NameC),
	 atom_codes(Email, EmailC),
	 atom_codes(Date, DateC)
	}.


committer(Name,Email,Date) -->
	[99, 111, 109, 109, 105, 116, 116, 101, 114, 32],
	name(NameC),
	[32, 60], author_email(EmailC), [62, 32],
	author_date(DateC,_ZoneC),
	[10],
	{
	 atom_codes(Name, NameC),
	 atom_codes(Email, EmailC),
	 atom_codes(Date, DateC)
	}.


name([N|T]) -->
	name_char(N),
	name(T).
name([]) --> [].

author_email([N|T]) -->
	email_char(N),
	author_email(T).
author_email([]) --> [].

author_date(S,Z) -->
	xdigits(S),
	[32,43],
	xdigits(Z).

name_char(N) -->
	[N],
	{
	 N \= 60,
	 N \= 10
	}.
email_char(N) -->
	[N],
	{
	 N \= 62
	}.

comment(C) -->
	[10],
	comment_chars(Codes),
	{
	 atom_codes(Atom, Codes),
	 sub_atom(Atom, 0, _, 1, C) % strip of last \n
	}.
comment_chars([C|T]) -->
	comment_char(C), !,
	comment_chars(T).
comment_chars([]) --> [].

comment_char(C) -->
	[C],
	{
	 C \= eos
	}.

end_of_lines -->
	[10], end_of_lines.
end_of_lines -->
	[].

hash(H) -->
	xdigits(D),
	{ atom_codes(H,D) }.

xdigits([D|T]) -->
        xdigit(D), !,
        xdigits(T).
xdigits([]) -->
        [].

xdigit(E) -->
        [E],
        { code_type(E, xdigit(_))
        }.





tree([H|T]) -->
	blobline(H),
	tree(T).
tree([]) --> [].

blobline(Blob) -->
	mode,
	myblob,
	hash(Hash),
	[09],
	name(NameCodes),
	[10],
	{ atom_codes(Name, NameCodes),
	  Blob = [hash(Hash),
		  name(Name)] }.

mode --> % 100644 space
	[49, 48, 48,54,52,52,32].

myblob -->
	[98, 108, 111, 98, 32].

