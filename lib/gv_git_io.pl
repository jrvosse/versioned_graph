:- module(gv_git_io,
	 [
	  gv_init_git/0,
	  gv_current_branch_git/1,
	  gv_commit_property_git/2,
	  gv_branch_head_git/2,
	  gv_store_git_object/3,
	  gv_git_cat_file/2,
	  gv_move_head_git/2,
	  gv_parse_tree/2
	 ]).

:- use_module(library(git)).
:- use_module(parse_git_objects).


gv_init_git :-
	setting(graph_version:gv_git_dir, Dir),
	(   exists_directory(Dir)
	->  true
	;   make_directory(Dir),
	    catch(git(['init', '--bare'],[directory(Dir)]), _, fail)
	).

gv_current_branch_git(Ref) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['symbolic-ref', 'HEAD'],[directory(Dir), output(OutCodes)]), _, fail),!,
	atom_codes(RefNL, OutCodes),
	sub_atom(RefNL, 0, _, 1, Ref).


gv_commit_property_git(CommitHash, Prop) :-
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

gv_branch_head_git(Ref, Hash) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['show-ref', '--hash', Ref],
		  [output(Codes), directory(Dir)]), _, fail),
	atom_codes(Atom, Codes),  % hash with newline
	sub_atom(Atom, 0, 40 ,1, Hash).

gv_move_head_git(Ref, Hash) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['update-ref', Ref, Hash],[directory(Dir)]), _, fail).


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


gv_git_cat_file(Hash, Codes) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['cat-file', '-p', Hash],
		  [directory(Dir), output(Codes)]),
	      _,
	      fail).

gv_parse_tree(Hash, TreeObject):-
	gv_git_cat_file(Hash, Codes),
	phrase(tree(TreeObject), Codes).
