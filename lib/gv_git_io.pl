:- module(gv_git_io,
	 [
	  gv_init_git/0,
	  gv_current_branch_git/1,
	  gv_commit_property_git/2,
	  gv_branch_head_git/2,
	  gv_move_head_git/2
	 ]).

:- use_module(library(git)).
:- use_module(parse_git_objects).


gv_init_git :-
	setting(graph_version:gv_git_dir, Dir),
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

gv_current_branch_git(Ref) :-
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['symbolic-ref', 'HEAD'],[directory(Dir), output(OutCodes)]), _, fail),!,
	atom_codes(RefNL, OutCodes),
	sub_atom(RefNL, 0, _, 1, Ref).


gv_commit_property_git(CommitHash, Prop) :-
	Prop	=.. [RDFPred, _RDFValue],
	setting(graph_version:gv_git_dir, Dir),
	catch(git(['cat-file', '-p', CommitHash],[directory(Dir), output(Codes)]),_,fail),
	phrase(commit(CommitObject), Codes),
	(   memberchk(RDFPred, [parent, tree, comment])
	->  option(Prop, CommitObject)
	;   memberchk(RDFPred, [committer_name, committer_date, committer_email])
	->  option(committer(C), CommitObject),
	    option(Prop, C)
	;   memberchk(RDFPred, [author_name, author_date, author_email])
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

