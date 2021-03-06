:- module(gv_git_io,
	  [ gv_init_git/1,
	    gv_current_branch_git/1,
	    gv_commit_property_git/2,
	    gv_branch_head_git/2,
	    gv_store_git_object/3,
	    gv_git_cat_file/2,
	    gv_move_head_git/2,
	    gv_parse_tree/2,
	    gv_parse_commit/2
	  ]).

/* Implement low level (plumbing) git commands using the SWI Prolog git library.
*/

:- use_module(library(git)).
:- use_module(parse_git_objects).

%%	gv_init_git(+Options) is semidet.
%
%	Init git directory specified in Options as a bare git
%	repository. Succeeds silently as no-op when directory already
%	exists.
gv_init_git(Options) :-
	option(directory(Dir), Options),
	(   exists_directory(Dir)
	->  true
	;   make_directory(Dir),
	    catch(git(['init', '--bare'],[directory(Dir)]), _, fail)
	).

%%	gv_parse_tree(+Hash, TreeObject) is semidet.
%
%	Read tree object from git repository into a TreeObject term.
gv_parse_tree(Hash, TreeObject):-
	gv_git_cat_file(Hash, Codes),
	phrase(tree(TreeObject), Codes).

gv_parse_commit(Hash, CommitObject):-
	gv_git_cat_file(Hash, Codes),
	phrase(commit(CommitObject), Codes).

gv_current_branch_git(Ref) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['symbolic-ref', 'HEAD'],[directory(Dir), output(OutCodes)]), _, fail),!,
	atom_codes(RefNL, OutCodes),
	sub_atom(RefNL, 0, _, 1, Ref).


gv_commit_property_git(CommitHash, Prop) :-
	compound(Prop),!,
	Prop =.. [RDFPred, _RDFValue],
	gv_git_cat_file(CommitHash, Codes),
	phrase(commit(CommitObject), Codes),
	(   memberchk(RDFPred, [parent, tree, comment])
	->  option(Prop, CommitObject)
	;   memberchk(RDFPred, [committer_url, committer_date, committer_email])
	->  option(committer(C), CommitObject),
	    option(Prop, C)
	;   memberchk(RDFPred, [author_url, author_date, author_email])
	->  option(author(C), CommitObject),
	    option(Prop, C)
	).
gv_commit_property_git(CommitHash, Prop) :-
	var(Prop),!,
	gv_git_cat_file(CommitHash, Codes),
	phrase(commit(CommitObject), Codes),
	member(O, CommitObject),
	O =.. [Key, Value],
	(   memberchk(Key, [parent, tree, comment])
	->  Prop = O
	;   memberchk(Key, [committer, author])
	->  member(Prop, Value)
	).

gv_branch_head_git(Ref, Hash) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['show-ref', '--hash', Ref],
		  [output(Codes), directory(Dir)]), _, fail),
	atom_codes(Atom, Codes),  % hash with newline
	sub_atom(Atom, 0, 40 ,1, Hash).

gv_move_head_git(Ref, Hash) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['update-ref', Ref, Hash],[directory(Dir)]), _, fail).

gv_store_git_object(Hash, _Object, Options) :-
	gv_git_file_exists(Hash, Options), !,
	debug(gv, 'Git object ~w already exists, save attempt ignored', [Hash]).

gv_store_git_object(Hash, Object, Options) :-
	option(type(Type), Options, blob),
	(   Type == tree
	->  Encoding = binary
	;   Encoding = utf8
	),
	tmp_file_stream(Encoding, Tmp, Stream),
	write(Stream, Object), close(Stream),
	catch(git(['hash-object', '-w', '-t', Type, Tmp],[output(Codes)|Options]), _, fail),
	atom_codes(HashN, Codes),
	sub_atom(HashN, 0, _, 1, GitHash), % remove trailing new line ...
	assertion(Hash == GitHash), !.

gv_git_file_exists(Hash, _Options) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['cat-file', '-e', Hash],
		  [directory(Dir), status(Status)]),
	      _,
	      fail),
	Status == exit(0).


gv_git_cat_file(Hash, Codes) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['cat-file', '-p', Hash],
		  [directory(Dir), output(Codes)]),
	      _,
	      fail).

