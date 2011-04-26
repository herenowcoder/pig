%%-*-prolog-*-
:- use_module(library(http/dcg_basics)).

%% lexer

keys([K|Ks]) --> key(K), whites, keys(Ks).
keys([]) --> [].

key(key(K,V)) --> nonblanks(X), whites, values(V), {X\=[], atom_codes(K,X)}.

values(many(X)) --> bracketed(X), !. % X is list to be flattened
values(one(S)) --> nonblanks(X), {string_to_list(S,X)}. % S is string

bracketed(X) --> "{", in_brackets(X), "}".

in_brackets([]) --> [].
in_brackets([X|Xs]) --> "{", !, in_brackets(X), "}", in_brackets(Xs).
in_brackets([X|Xs]) -->
    string_without("{}",X), {X\=[]},
    in_brackets(Xs).

%% parser

headline(headline(PkgName, Bytes)) --> 
    nonblanks(P), whites, integer(Bytes),
    {atom_codes(PkgName,P)}.
 
%%   ["version","revision","categories","variants","variant_desc",
%%     "platforms","description","long_description","portdir","homepage",
%%     "depends_build","depends_lib","depends_run","epoch",
%%     "universal","macosx","darwin"].

parse(P, key(description, many(D))) :-
    append(D,Flat),
    string_to_list(S,Flat),
    assert(description(P,S)).

%% feeder

eat_file :- eat_file('data/pi1').
eat_file(Fname) :- open(Fname, read, F), eat_lines(F).

eat_lines(F) :-
    read_line_to_codes(F,S1),
    (S1 = end_of_file -> true;
        phrase(headline(headline(P,Bytes)),S1),
        writef('* got pkg: %w\n', [P]),
        read_line_to_codes(F,S2),
        phrase(keys(Ks),S2),
        eat_lines(F)
    ).
