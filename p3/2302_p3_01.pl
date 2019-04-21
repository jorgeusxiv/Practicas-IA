%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%    Lab assignment 2: Search
%%
%% Javier Martinez Rubio javier.martinezrubio@estudiante.uam.es e357532
%% Jorge Santisteban Rivas jorge.santisteban@estudiante.uam.es e360104
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%EJERCICIO 1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

duplica([],[]).

duplica([A|L], [A,A|L1]) :- duplica(L,L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%EJERCICIO 2%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%Concatena%%%

concatena([],L,L).
concatena([X|L1], L2,[X|L3]):-
    concatena(L1,L2,L3).

%%%Invierte%%%

invierte([],[]).
invierte([H|T],ListaInv):-
    invierte(T,InvT), concatena(InvT, [H], ListaInv).

%%%%%%%%%%%%%%%%%%%%%%%%%%EJERCICIO 3%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%Concatena%%%

concatena([],L,L).
concatena([X|L1], L2,[X|L3]):-
    concatena(L1,L2,L3).

%%%Invierte%%%

invierte([],[]).
invierte([H|T],ListaInv):-
    invierte(T,InvT), concatena(InvT, [H], ListaInv).

%%%Palindromo%%%

palindromo(L):-invierte(L,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%EJERCICIO 4%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

divide(L,N, L1,L2):-
    length(L1, N),
    append(L1, L2, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%EJERCICIO 5%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aplasta([], []):- !.
aplasta([L|Ls], L1) :-
    !,
    aplasta(L, First),
   	aplasta(Ls, Rest),
    append(First, Rest, L1).
aplasta(L, [L]).

%%%%%%%%%%%%%%%%%%%%%%%%%%EJERCICIO 6%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%next_factor%%%

next_factor(_,2,3):- !.
next_factor(N,F,NF):-
    F*F < N,!,NF is F + 2.
next_factor(N,_,N).

%%%Primos/2%%%

primos(N,L):-
    N > 0,
    primos(N,L,2).

%%%Primos/3%%%

primos(1,[],_):- !.
primos(N, [Factor|Resto], Factor):-
    Div is N // Factor,
    N =:= Div*Factor,
    !,
    primos(Div, Resto, Factor).

primos(N,Resto, Factor):-
    N > Factor,
    next_factor(N,Factor, NFactor),
    primos(N,Resto, NFactor).


%%%%%%%%%%%%%%%%%%%%%%%%%%EJERCICIO 7%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%7.1%%%

cod_primero(X,[],[],[X]).
cod_primero(X,[X|Ys], Lrem, [X|Xs]):-
    cod_primero(X,Ys,Lrem,Xs).
cod_primero(X, [Y|Ys], [Y|Ys], [X]):-
    dif(X,Y).

%%%7.2%%%

cod_all([],[]).
cod_all([X|Resto], [Y|Lfront]):-
        cod_primero(X,Resto,Lrem,Y),
        cod_all(Lrem, Lfront).

%%%7.3%%%

run_length(L,L1):-
    cod_all(L,LAux),
    transformar(LAux,L1).

transformar([],[]).
transformar([[X|Xs]|Ys], [[Len,X] | Resto]):-
    length([X|Xs],Len),
    transformar(Ys,Resto).


%%%%%%%%%%%%%%%%%%%%%%%%%%EJERCICIO 8%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%Dictionary%%%

dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

%%%Funciones auxiliares Ejercicio 7%%%

%%%7.1%%%

cod_primero(X,[],[],[X]).
cod_primero(X,[X|Ys], Lrem, [X|Xs]):-
    cod_primero(X,Ys,Lrem,Xs).
cod_primero(X, [Y|Ys], [Y|Ys], [X]):-
    dif(X,Y).

%%%7.2%%%

cod_all([],[]).
cod_all([X|Resto], [Y|Lfront]):-
        cod_primero(X,Resto,Lrem,Y),
        cod_all(Lrem, Lfront).

%%%7.3%%%

run_length(L,L1):-
    cod_all(L,LAux),
    transformar(LAux,L1).
transformar([],[]).
transformar([[X|Xs]|Ys], [[Len,X] | Resto]):-
    length([X|Xs],Len),
    transformar(Ys,Resto).

%%%Validar_txt%%%

validarTxt([]):- !.
validarTxt([X|Resto]):-
    dictionary(D),
    member(X,D),
    validarTxt(Resto).

%%%Formatear%%%

formatear([],[]):- !.
formatear([[A,B] | Resto], [[B-A]|FormatL]):-
    formatear(Resto, FormatL).

%%%InsertarElem%%%

insertarElem([A-B],[],[A-B]).
insertarElem([A-B],[C-D | Resto],[A-B, C-D | Resto]):-
    B >= D.
insertarElem([A-B],[C-D | Resto],[C-D | E]):-
    insertarElem([A-B],Resto,E),
    B < D.

%%%OrdenarLista%%%

ordenarLista([],[]).
ordenarLista([[A-B] | Resto], OrdLista):-
    ordenarLista(Resto, RecLista),
    insertarElem([A-B], RecLista, OrdLista).


%%%Concatena%%%

concatena([],L,L).
concatena([X|L1], L2,[X|L3]):-
    concatena(L1,L2,L3).

%%%Build_tree%%%

build_tree([],nil).
build_tree([X-_], tree(X,nil,nil)):- !.
build_tree([X-_|Resto], T):-
    build_tree(Resto,S),
    T = tree(1, tree(X,nil,nil), S).

%%%Encode_elem%%%

encode_elem(X, [0], tree(1, tree(X, nil, nil), _)).
encode_elem(X,[0], tree(X, nil, nil)).
encode_elem(X, Cod, tree(1,_,Resto)):-
    encode_elem(X, S, Resto),
    concatena([1], S, Cod).

%%%Encode list%%%

encode_list([],[],_).
encode_list([X], CodList, Tree):-
    encode_elem(X, Cod1, Tree),
    concatena([Cod1],[],CodList),!.
encode_list([X|Resto], CodList, Tree):-
    encode_elem(X, Cod1, Tree),
    encode_list(Resto, CodResto, Tree),
    concatena([Cod1], CodResto, CodList).

%%%Encode%%%

encode(L1,L2):-
    validarTxt(L1),
    sort(0,@=<,L1,SortL1),
    run_length(SortL1, RunL1),
    formatear(RunL1,FormatL1),
    ordenarLista(FormatL1, OrdenadaL1),
    build_tree(OrdenadaL1, TreeL1),
    encode_list(L1,L2,TreeL1).
