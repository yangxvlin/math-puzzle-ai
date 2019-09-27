%% File     : Proj2.pl
%% Author   : XuLin Yang 904904 <xuliny@student.unimelb.edu.au>
%% Origin   : Thu Sep 26 14:54:20 2019 
%% Purpose  : An implementation of 2-player logical guessing game solution.
%%
%%|This code is for providing the implementation for the two-player logical 
%% guessing game. It is defined in three functions: 
%%     `feedback` for the respondent side 
%%     `initialGuess`, `nextGuess` for the guesser side.
%%
%% The program is for solving the game that is the respondent have `N` 
%% selected secret cards from a deck of 52 cards without jokers for the 
%% guesser to guess. The guesser first make the guess by calling 
%% `initialGuess` and then the respondent response `feedback` for the guessed
%% selection of cards and the secret selection of cards. After that the 
%% guesser repeat the process by using `nextGuess` with the respondent's 
%% `feedback` until the guesser get the correct selection.
%% 
%% The program assume the respondent has a selection of 2-4 cards. (i.e.: N 
%% range from 2 to 4 inclusively).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			SWI Prolog’s ConstraintLogic Programming Library
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded(library(clpfd)).


%% puzzlesolution(+Puzzle)
puzzlesolution/1.
puzzlesolution(Puzzle) :-
    Puzzle = [ColumnBehind0|RowRows],
    ColumnBehind0 = [0|Column],
    getNthListElements(RowRows, 1, Row, Rows),
    allDigits(Rows),
    allDifferentList(Rows),
    transpose(Rows, RowsTrans),

allDifferentList(Lists) :-
    maplist(all_different(), Lists).

allDigits(Rows) :-
    maplist(maplist(between(1, 9)), Rows).

puzzlesolution(Column, Row, Rows) :-
    transpose(Rows, Columns),
    (maplist(allGround(), Rows) ->
    diagonal(Rows, Diagonal),
    checkPuzzle(Row, Column, Rows, Columns, Diagonal)
    ;
    
    % generate(, Next),
    puzzlesolution(Column, Row, Rows)
    ).

% fill([], _).
% fill(Column, Row, Rows, Columns, Out) :-

% replaceUnground([], [], Y) :- write(Y).
% replaceUnground([], [], _).
% replaceUnground([R|Row], [X|Xs], Out) :-
%     ( allGround(X)
%     -> replaceUnground(Row, Xs, [X|Out])
%     ; generate(R, X, XGround), replaceUnground(Row, Xs, [XGround|Out])
%     ).

replaceUnground([R|Row], [X|Xs], Out) :-
    replaceUnground([R|Row], [X|Xs], [], Out).
replaceUnground([], [], a, _).
replaceUnground([], [], ACC, Out) :- replaceUnground([], [], a, [ACC|Out]).
replaceUnground([R|Row], [X|Xs], ACC, Out) :-
    ( allGround(X)
    -> replaceUnground(Row, Xs, [X|ACC], Out)
    ; generate(R, X, XGround),
        replaceUnground(Row, [XGround|Xs], ACC, Out)
    ).

% replaceUnground([], []).
% replaceUnground([R|Row], [X|Xs]) :-
%     ( allGround(X)
%     -> replaceUnground(Row, Xs)
%     ; generate(R, X, XGround),
%         X = XGround, 
%         replaceUnground(Row, Xs)
%     ).


allGround(List) :- maplist(ground(), List).

product(X, Y, Z) :- Z is X * Y.
productList(List, Product) :- foldl(product, List, 1, Product).

checkDiagonal(Diagonal) :- \+ all_different(Diagonal).

checkList(ListSOrP, List) :-
    all_different(List), 
    (sum_list(List, ListSOrP); productList(List, ListSOrP)).

% generateList(_, [], _).
% generateList(ListSOrP, [X|Xs], Ys) :-
%     ( \+ ground(X), between(1, 9, X), generateList(ListSOrP, [X|Xs], [X|Ys]) )
%     ; generateList(ListSOrP, Xs, [X|Ys]).

% possible 1
% generateList(ListSOrP, [], Ys) :- sum_list(Ys, ListSOrP); productList(Ys, ListSOrP).
% generateList(ListSOrP, [X|Xs], Ys) :-
%     ( \+ ground(X)
%     -> between(1, 9, X), generateList(ListSOrP, [X|Xs], Ys)
%     ; generateList(ListSOrP, Xs, [X|Ys])
%     ).

generate(ListSOrP, List, Out) :-   
    findall(X, (generateList(ListSOrP, List), X = List, checkList(ListSOrP, X)), Out).
    

generateList(_, []).
generateList(ListSOrP, [X|Xs]) :-
    ( \+ ground(X)
    -> between(1, 9, X), generateList(ListSOrP, [X|Xs])
    ; generateList(ListSOrP, Xs)
    ).

checkPuzzle(Row, Column, Rows, Columns, Diagonal) :-
    maplist(allGround(), Rows),
    checkDiagonal(Diagonal),
    maplist(checkList(), Row, Rows),
    maplist(checkList(), Column, Columns).


diagonal(Matrix, Diagonal) :-
    length(Matrix, Length),
    recurGetNthElement(Matrix, Length, 1, Diagonal).

getNthListElements([], _, [], []).
getNthListElements([L|Lists], N, [Element|Elements], [Rest|Rests]) :-
    nth1(N, L, Element, Rest),
    getNthListElements(Lists, N, Elements, Rests).

recurGetNthElement([], _, _, []).
recurGetNthElement([R|Rs], Length, Acc, [Nth|Result]) :-
    NewAcc is Acc + 1,
    nth1(Acc, R, Nth),
    recurGetNthElement(Rs, Length, NewAcc, Result).
