/*---------------solucionador de puzzles binarios-------------------------------
-----------------Lucia Filipa Lopes da Silva-----------------------------------*/

:- consult(codigo_comum).

/*-----------------------------------REGRAS--------------------------------------
1. nao existem 3 zeros ou 3 uns seguidos em nenhuma fila, ou seja, em nenhuma coluna ou linha;
2.todas as colunas e linhas tem o mesmo numero de zeros ou uns, que e metade da dimensao do puzzle;
3.nao existem nem colunas nem linhas repetidas.
-------------------------------------------------------------------------------
funcao que recebe uma lista de 3 elementos, 0, 1 ou uma variavel, e retorna a lista N_Triplo, resultante de aplicar a regra 1*/

aplica_R1_triplo(Triplo, N_Triplo) :-
    conta_var(Triplo, 1), 
        (conta_0(Triplo, 2), altera(Triplo, _, 1,N_Triplo))
        ; 
        (conta_1(Triplo, 2), altera(Triplo, _, 0,N_Triplo)).

aplica_R1_triplo(Triplo, Triplo) :-
    (conta_var(Triplo, 1),
        conta_0(Triplo,1)
        ;
        conta_1(Triplo, 1))
    ;
    (conta_var(Triplo, 2), 
        (conta_0(Triplo, 1) 
        ;
        conta_1(Triplo, 1)))
    ;
    conta_var(Triplo, 2)
    ; 
    conta_var(Triplo, 3)
    ; 
    (conta_var(Triplo, 0), 
        (conta_0(Triplo, 1)
        ; 
        conta_0(Triplo, 2)
        ; 
        conta_1(Triplo, 1) 
        ; conta_1(Triplo, 2))).


/*------------------------------------------------------------------------------------
funcao que aplica a regra um a uma coluna ou linha, aplicando, repetidamente, a funcao 
aplica_R1_fila_aux*/

aplica_R1_fila(Fila, N_Fila) :-
    aplica_R1_fila_aux(Fila, Aux),

    ((not(listas_iguais(Aux, Fila)), aplica_R1_fila(Aux, N_Fila))
    ;
    N_Fila = Fila).

%funcao que aplica a regra um, atraves da funcao aplica_R1_triplo, a uma fila inteira

aplica_R1_fila_aux([X, Y | []], [X, Y | []]).

aplica_R1_fila_aux([E1, E2, E3 | Resto], [X | N_Fila]) :-
        aplica_R1_triplo([E1, E2, E3], [X, Y, Z]),
        aplica_R1_fila_aux([Y, Z | Resto], N_Fila).


/*-----------------------------------------------------------------------------
funcao que aplica a regra 2 numa fila*/

aplica_R2_fila(Fila, N_Fila) :-
    length(Fila, N), N_Elem is N/2,
    ((conta_0(Fila, N_Elem), (altera(Fila,_ ,1, N_Fila))
    ;
    (conta_1(Fila, N_Elem)),altera(Fila,_ ,0 , N_Fila))
    ;
    (conta_0(Fila, N1), N1 < N_Elem, conta_1(Fila, N2), N2 < N_Elem, N_Fila = Fila))
    ;
    false.


/*-----------------------------------------------------------------------------
funcao que aplica a regra um e a regra dois, utilizando as funcoes ja anteriormente definidas*/

aplica_R1_R2_fila(Fila, N_Fila) :-
    aplica_R1_fila(Fila, Aux),
    aplica_R2_fila(Aux, N_Fila).


/*-------------------------------------------------------------------------------
funcao em que se aplicam as regras um e dois nas colunas e linhas do puzzle*/

aplica_R1_R2_puzzle(Puz, N_Puz) :-
    
    aplica_linhas(Puz, Aux),

    mat_transposta(Aux, Transposta),
    aplica_linhas(Transposta, N_Puz_Aux),
    mat_transposta(N_Puz_Aux, N_Puz).

%funcao que aplica a uma linha ou uma coluna as regras um e dois

aplica_linhas([],[]).
aplica_linhas([L | Resto], [X | N_Puz]) :- 
    aplica_R1_R2_fila(L, X),
    aplica_linhas(Resto, N_Puz).


/*--------------------------------------------------------------------------------
funcao que inicializa um puzzle, ou seja, aplicar a regra um e dois ate nao serem 
preenchidas novas posicoes*/

inicializa(Puz, N_Puz) :-
    conta_var_puzzle(Puz, V1), 
    aplica_R1_R2_puzzle(Puz, N_Puz_Aux),
    conta_var_puzzle(N_Puz_Aux, V2), 
    N_Puz = N_Puz_Aux,
    (((V1 =\= V2),inicializa(N_Puz_Aux, N_Puz))
    ;
    !).


/*---------------------------------------------------------------------------------
funcao que verifica se as linhas e colunas sao todas diferentes*/

verifica_R3(Puz) :-
    delete_var(Puz, Aux), 
    verifica_R3_linhas(Aux),
    verifica_R3_colunas(Aux).

verifica_R3_linhas([]).
verifica_R3_linhas([P | P1]) :- 
    (not(member(P, P1)), verifica_R3_linhas(P1))
    ;
    (not(length(P, 4)), verifica_R3_linhas(P1)).

verifica_R3_colunas([]).
verifica_R3_colunas([P | P1]) :-
    (not(member(P, P1)), verifica_R3_colunas(P1))
    ;
    (not(length(P, 4)), verifica_R3_colunas(P1)).


/*----------------------------------------------------------------------------------
funcao que, apos uma mudanca numa certa coordenada dada, vai propagar essa mudanca
no resto da matriz ate nao haver mais mudancas possiveis*/

propaga_posicoes([], N_Puz, N_Puz).
propaga_posicoes([(L,C) | Resto], Matriz, N_Puz) :- 
    copiar_puzzle(Matriz, Puz),

    linha_N(L, Puz, Linha), 
    aplica_R1_R2_fila(Linha, N_linha), !,
    mat_muda_linha(Puz, L, N_linha, Aux_Puz),

    mat_transposta(Aux_Puz, T_Puz),
    
    linha_N(C, T_Puz, Coluna),
    aplica_R1_R2_fila(Coluna, N_coluna), !,
    mat_muda_linha(T_Puz, C, N_coluna, Puzzle_Aux),
    mat_transposta(Puzzle_Aux, N_Aux),

    verifica_R3(N_Aux),
    
    coor_diferente(Matriz, N_Aux, 0, [], Lst),!,append(Resto, Lst, Nova),propaga_posicoes(Nova, N_Aux, N_Puz).

%funcao que devolve as coordenadas diferentes de duas matrizes

coor_diferente([], [], _, Lst, Lst).
coor_diferente([P1 | Puz], [P2 | Puzzle], Linha, L, Lst) :- 
    incr(Linha, N_Linha),
    ((not(listas_iguais(P1, P2)), indice_elem(P1, P2, 0, _, M), coor(M, N_Linha, _, L_aux), append(L, L_aux, LF),coor_diferente(Puz, Puzzle, N_Linha, LF, Lst))
    ;
    (coor_diferente(Puz, Puzzle, N_Linha, L, Lst))).
    
%funcao que devolve o indice do elemento que e diferente entre duas listas

indice_elem([],[],_ ,M ,M). 
indice_elem([L1 | Lst1], [L2 | Lst2], N, Lista, M) :- 
    (var(L1), var(L2), incr(N, N1), indice_elem(Lst1, Lst2, N1, Lista ,M))
    ;
    (L1 == L2, incr(N, N1),indice_elem(Lst1, Lst2, N1, Lista, M))
    ;
    (incr(N, N1), append(Lista, [N1], Lista_aux), indice_elem(Lst1, Lst2, N1, Lista_aux, M)).

%funcao que devolve a lista de coordenadas de uma linha N com os indices das colunas na Lista

coor([], _, Lista_Final, Lista_Final).
coor([L1 | Lst], N, Lista, Lista_Final) :-
    append(Lista, [(N, L1)], Lista_aux), coor(Lst, N, Lista_aux, Lista_Final).


/*-----------------------------------------------------------------------------------------
funcao que devolve uma solucao do puzzle*/

resolve(Puz, Sol) :-
    inicializa(Puz, Puzzle),
    resolve_aux(Puzzle,Sol).


resolve_aux(Puz, Puz) :-
    conta_var_puzzle(Puz, 0).

resolve_aux(Puzzle, Sol) :-

    copiar_puzzle(Puzzle, Puz),

    mat_ref(Puz, (L, C), var),
    (mat_muda_posicoes(Puz, [(L,C)], [0], N_Puz), propaga_posicoes([(L,C)], N_Puz, P)
    ;
    mat_muda_posicoes(Puz, [(L,C)], [1], N_Puz), propaga_posicoes([(L,C)], N_Puz, P)),
    resolve_aux(P, Sol).


/*--------------------------------------------*/

conta_var(Lst, Cont) :-
    include(var, Lst, Lst_var),
    length(Lst_var, Cont).

conta_1(Lst, Cont) :-
    include(valor_1, Lst,Lst_1),
    length(Lst_1, Cont).

conta_0(Lst, Cont) :-
    include(valor_0, Lst,Lst_0),
    length(Lst_0, Cont).

valor_0(X) :- X == 0.

valor_1(X) :- X == 1.

altera([], _,_, []).
altera([L1 | L2], N1, N2, [M1 | M2]) :-
    (var(L1), var(N1), M1 = N2, altera(L2, N1, N2, M2))
    ;
    (L1 == N1, M1 = N2, altera(L2, N1,N2, M2))
    ;
    (M1 = L1, altera(L2, N1, N2, M2)).

listas_iguais([], []).
listas_iguais([E1 | R1], [E2 | R2]) :-
    ((var(E1), var(E2)) 
    ;
    E1 == E2),
    listas_iguais(R1, R2).

conta_var_puzzle(Puz, V) :-
    append(Puz, P),
    conta_var(P, V).

delete_var([], []).
delete_var([L1 | Resto], [X | Lst]) :-
    exclude(var, L1, X),
    delete_var(Resto, Lst).

linha_N(N, Puz, Linha) :-
    mat_transposta(Puz, T_Puz), 
    mat_elementos_coluna(T_Puz, N, Linha).

decr(X,NX) :-
    NX is X-1.

incr(X,NX) :-
    (var(X), NX= 1)
    ;
    (number(X),NX is X+1).

copiar_puzzle([], []).
copiar_puzzle([P1 | Puzzle], [N | Novo]) :-
    copiar_linha(P1, N),
    copiar_puzzle(Puzzle, Novo).

copiar_linha([], []).
copiar_linha([P | P1], [_| N1]) :-
    var(P), copiar_linha(P1, N1).
copiar_linha([P | P1], [P | N1]) :- 
    number(P), copiar_linha(P1, N1).