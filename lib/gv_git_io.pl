:- module(gv_git_io,
	 [
	  gv_hash_uri/2,
	  gv_init_git/0,
	  gv_current_branch_git/1,
	  gv_commit_property_git/2,
	  gv_branch_head_git/2,
	  gv_move_head_git/2
	 ]).

:- use_module(library(git)).
:- use_module(parse_git_objects).

%%	gv_hash_uri(+Hash, -URI) is det.
%
%	URI is a uri constructed by concatenating the
%	Hash with some additional prefix to make it a
%	legal URI.
%
%	This provides a basic one to one mapping between git's SHA1 hash
%	ids and the URIs used in RDF.

gv_hash_uri(Hash, URI) :-
	nonvar(Hash), Hash \= null,
	!,
	atom_concat(x, Hash, Local),
	rdf_global_id(hash:Local, URI).

gv_hash_uri(Hash, URI) :-
	nonvar(URI),!,
	rdf_global_id(hash:Local, URI),
	atom_concat(x, Hash, Local).

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

gv_current_branch_git(Branch) :-
	setting(graph_version:gv_git_dir, Dir),
	git(['symbolic-ref', 'HEAD'],[directory(Dir), output(OutCodes)]),!,
	atom_codes(RefNL, OutCodes),
	sub_atom(RefNL, 0, _, 1, Ref),
	rdf_global_id(localgit:Ref, Branch).

gv_commit_property_git(Commit, RDFProp) :-
	RDFProp	=.. [RDFPred, RDFValue],
	setting(graph_version:gv_git_dir, Dir),
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
	;   memberchk(RDFPred, [author_name, author_date, author_email])
	->  option(author(C), CommitObject),
	    option(RDFProp, C)
	).

gv_branch_head_git(Branch, Commit) :-
	setting(graph_version:gv_git_dir, Dir),
	rdf_global_id(localgit:Ref, Branch),
	git(['show-ref', '--hash', Ref],[output(Codes), directory(Dir)]),
	atom_codes(Atom, Codes),  % hash with newline
	sub_atom(Atom, 0, 40 ,1, Hash),
	gv_hash_uri(Hash, Commit).

gv_move_head_git(Branch, NewHead) :-
	setting(graph_version:gv_git_dir, Dir),
	gv_hash_uri(Hash, NewHead),
	rdf_global_id(localgit:Local, Branch),
	git(['update-ref', Local, Hash],[directory(Dir)]).

