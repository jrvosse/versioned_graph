:- module(gv_parse_git_objects,
	  [
	  commit//1,
	  tree//1
	  ]).

:- use_module(library(semweb/rdf_db)).

commit(Commit) -->
	tree_line(T),
	parent(P),
	author(AName, AEmail, ADate, _AZone),   % fixme, deal with timezone!
	committer(CName, CEmail, CDate, _CZone),
	comment(CM),!,
	{
	    format(atom(AMailTo), 'mailto:~w', [AEmail]),
	    format(atom(CMailTo), 'mailto:~w', [CEmail]),
	    format_time(atom(AStamp), '%FT%T%:z', ADate),
	    format_time(atom(CStamp), '%FT%T%:z', CDate),
	    Commit0 = [
		tree(T),
		parent(P),
		author([ author_url(AName),
			 author_email(AMailTo),
			 author_date(literal(type(xsd:dateTimeStamp, AStamp)))
		       ]),
		committer([committer_url(CName),
			   committer_email(CMailTo),
			   committer_date(literal(type(xsd:dateTimeStamp, CStamp)))]),
		comment(literal(CM))
	    ],
	    rdf_global_term(Commit0, Commit)
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

author(Name,Email,Date, Zone) -->
	[97, 117, 116, 104, 111, 114, 32],
	name(NameC),
	[32, 60], author_email(EmailC), [62, 32],
	author_date(DateC,ZoneC),
	[10],!,
	{
	    atom_codes(Name, NameC),
	    atom_codes(Email, EmailC),
	    atom_codes(DateA, DateC),
	    atom_number(DateA, Date),
	    atom_codes(Zone, ZoneC)
	}.


committer(Name,Email,Date,Zone) -->
	[99, 111, 109, 109, 105, 116, 116, 101, 114, 32],
	name(NameC),
	[32, 60], author_email(EmailC), [62, 32],
	author_date(DateC,ZoneC),
	[10],
	{
	    atom_codes(Name, NameC),
	    atom_codes(Email, EmailC),
	    atom_codes(DateA, DateC),
	    atom_number(DateA, Date),
	    atom_codes(Zone, ZoneC)
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
	blobline(H),!,
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

