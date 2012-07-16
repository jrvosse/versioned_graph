:- module(gv_parse_git_objects,
	  [
	  commit//1,
	  tree//1
	  ]).


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

