%%-*-prolog-*-
:- use_module(library(http/dcg_basics)).

%% util

+dl(Xs-Ys, Ys-Zs, Xs-Zs).

list_to_dl(S,DL-E) :- append(S,E,DL).

%% lexer

split([]) --> [].
split([X|Xs]) --> nonblanks(X), {X\=[]}, whites, split(Xs).

keys([K|Ks]) --> key(K), whites, keys(Ks).
keys([]) --> [].

key(key(K,V)) --> nonblanks(X), whites, values(V), {X\=[], atom_codes(K,X)}.

values(many(X)) --> bracketed(X), !. % X as list
values(one(S)) --> nonblanks(X), {string_to_list(S,X)}. % S as string

bracketed(S) --> "{", in_brackets(X-[]), "}", {string_to_list(S,X)}.

in_brackets(E-E) --> [].
in_brackets(Zs) --> "{", !, in_brackets(Xs), "}", in_brackets(Ys),
    {list_to_dl("{",B1), list_to_dl("}",B2),
    +dl(B1,Xs, Z1),
    +dl(Z1,B2, Z2),
    +dl(Z2,Ys, Zs)}.
in_brackets(Ys) -->
    string_without("{}",S), {S\=[]},
    in_brackets(Xs),
    {list_to_dl(S,X), +dl(X, Xs, Ys)}.

%% parser

headline(headline(PkgName, Bytes)) --> 
    nonblanks(P), whites, integer(Bytes),
    {atom_codes(PkgName,P)}.
 
%%   ["version","revision","categories","variants","variant_desc",
%%     "platforms","description","long_description","portdir","homepage",
%%     "depends_build","depends_lib","depends_run","epoch",
%%     "universal","macosx","darwin"].

parse(_P, []).
parse(P, [K|Ks]) :- parse(P,K), parse(P,Ks).

parse(P, key(name, one(X))) :- string_to_atom(X,P).

parse(P, key(portdir, one(X))) :- assert( portdir(P, X) ).

parse(P, key(depends_fetch, many(_Deps))). % todo
parse(P, key(depends_fetch, one(_Deps))). % todo

parse(P, key(depends_extract, many(_Deps))). % todo
parse(P, key(depends_extract, one(_Deps))). % todo

parse(P, key(depends_build, many(_Deps))). % todo
parse(P, key(depends_build, one(_Deps))). % todo

parse(P, key(depends_lib, many(_Deps))). % todo
parse(P, key(depends_lib, one(_Deps))). % todo

parse(P, key(depends_run, many(_Deps))). % todo
parse(P, key(depends_run, one(_Deps))). % todo

parse(P, key(variants, _OneOrMany)). % todo

parse(_P, key(variant_desc, _OneOrMany)).

parse(P, key(description, many(D))) :- assert( description(P, D) ).
parse(P, key(description, one(D)))  :- assert( description(P, D) ).

parse(_P, key(homepage, _OneOrMany)).

parse(_P, key(platforms, _OneOrMany)).

parse(_P, key(license, _OneOrMany)).

parse(_P, key(replaced_by, _OneOrMany)).

parse(_P, key(epoch, one(_X))).

parse(_P, key(maintainers, _OneOrMany)).

parse(_P, key(long_description, _OneOrMany)).

parse(P, key(version, one(V))) :- assert( version(P, V) ).

parse(_P, key(revision, one(_X))).

parse(P, key(categories, one(C))) :-
    string_to_atom(C,A),
    assert( categories(P, [A]) ).
parse(P, key(categories, many(X))) :-
    string_to_list(X,S),
    phrase(split(Cs), S),
    findall(A, (member(C,Cs),atom_codes(A,C)), As),
    assert( categories(P, As) ).

%% feeder

eat_file :- eat_file('data/pi1').
eat_file(Fname) :- open(Fname, read, F), eat_lines(F).

eat_lines(F) :-
    read_line_to_codes(F,S1),
    (S1 = end_of_file -> !;
        phrase(headline(headline(P,_Bytes)),S1),
        writef('* got pkg: %w\n', [P]),
        read_line_to_codes(F,S2),
        phrase(keys(Ks),S2),
        parse(P,Ks),
        eat_lines(F)
    ).
