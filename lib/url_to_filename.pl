:- module(gv_url_to_filename,
	  [ url_to_filename/2
	  ]).

:- use_module(library(url)).
:- use_module(library(lists)).

%!	url_to_filename(-URL, +FileName) is det.
%
%	Encode a valid URL into a valid filename and vice versa. Earlier
%	versions used www_form_encode/2, but this can produce characters
%	that are not valid in filenames. We will use the same encoding
%	as www_form_encode/2, but using our own rules for allowed
%	characters. The only requirement is that we avoid any filename
%	special character in use. The current encoding use US-ASCII
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
	% for _decoding_ the standard www_form_encode/2 will do.
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
	{ atom_codes('%0D%0A', Codes),
	  append(Codes, T, Enc)
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
