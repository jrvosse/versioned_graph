:-module(gv_hash_uri,
	 [ gv_hash_uri/2
	 ]).


:- use_module(library(semweb/rdf_db)).

%%	gv_hash_uri(?Hash, ?URI) is det.
%
%	URI is a uri constructed by concatenating the
%	Hash with some additional prefix to make it a
%	legal URI.
%
%	This provides a basic one to one mapping between
%       git's SHA1 hash ids and the URIs used in RDF.

gv_hash_uri(Hash, URI) :-
	nonvar(Hash), Hash \= null,
	!,
	atom_concat(x, Hash, Local),
	rdf_global_id(hash:Local, URI).

gv_hash_uri(Hash, URI) :-
	nonvar(URI),!,
	rdf_global_id(hash:Local, URI),
	atom_concat(x, Hash, Local).
