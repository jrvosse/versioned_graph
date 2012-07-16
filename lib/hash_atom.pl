:- module(gv_hash_atom, [
			 gv_hash_atom/2
			]).

%%      gv_hash_atom(+Codes, -Hash) is det.
%       gv_hash_atom(-Codes, +Hash) is det.
%
%       Bi-directional version of hash_atom/2 ...
%
gv_hash_atom(Codes, Hash) :-
        nonvar(Codes),
        !,
        hash_atom(Codes, Hash).

gv_hash_atom(Codes, Hash) :-
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
