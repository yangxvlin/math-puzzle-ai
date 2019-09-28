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


%% puzzle_solution(+Puzzle)
puzzle_solution/1.
% puzzle_solution(Puzzle) :-
%     Puzzle = [ColumnBehind0|RowRows],
%     ColumnBehind0 = [0|Column],
%     getNthListElements(RowRows, 1, Row, Rows),
%     % a square grid of squares, each to be filled in with a single digit 1–9
%     allDigits(Rows),
%     % all squares on the diagonal line from upper left to lower right contain the same value
%     checkMatrixDiagonal(Rows),
%     % each row and each column contains no repeated digits
%     allDifferentList(Rows),
%     checkMatrixSP(Rows, Row),
%     transpose(Rows, RowsTrans),
%     allDifferentList(RowsTrans),
%     % the heading of reach row and column (leftmost square in a row and topmost 
%     % square in a column) holds either the sum or the product of all the digits 
%     % in that row or column
%     checkMatrixSP(RowsTrans, Column),
%     checkMatrixGround(Rows),
%     checkMatrixGround(RowsTrans).

puzzle_solution(Puzzle) :-
    Puzzle = [ColumnBehind0|RowRows],
    ColumnBehind0 = [0|Column],
    getNthListElements(RowRows, 1, Row, Rows),
    % all squares on the diagonal line from upper left to lower right contain the same value
    generateDiagonal(Rows),
    % a square grid of squares, each to be filled in with a single digit 1–9
    maplist(generateRow, Row, Rows),
    transpose(Rows, RowsTrans),
    % the heading of reach row and column (leftmost square in a row and topmost 
    % square in a column) holds either the sum or the product of all the digits 
    % in that row or column
    allDifferentList(RowsTrans),
    checkMatrixSP(RowsTrans, Column),
    checkMatrixGround(Rows),
    checkMatrixGround(RowsTrans).

same([]).   % You only need this one if you want the empty list to succeed
same([_]).
same([X,X|T]) :- same([X|T]).

generateDiagonal(Matrix) :-
    diagonal(Matrix, Diagonal),
    maplist(between(1, 9), Diagonal),
    same(Diagonal).

generateRow(RowSorP, Row) :-
    maplist(between(1, 9), Row),
    all_different(Row),
    checkList(RowSorP, Row).

allDifferentList(Lists) :-
    maplist(all_different, Lists).

allDigits(Rows) :-
    maplist(maplist(between(1, 9)), Rows).

checkMatrixGround(Matrix) :-
    maplist(allGround, Matrix).

allGround(List) :- maplist(ground, List).

product(X, Y, Z) :- Z is X * Y.
productList(List, Product) :- foldl(product, List, 1, Product).

checkMatrixDiagonal(Matrix) :-
    diagonal(Matrix, Diagonal),
    checkDiagonal(Diagonal).

checkDiagonal(Diagonal) :- \+ all_different(Diagonal).

checkMatrixSP(Matrix, SP) :-
    maplist(checkList, SP, Matrix).
    

checkList(ListSOrP, List) :-
    (sum_list(List, ListSOrP); productList(List, ListSOrP)).

generate(ListSOrP, List, Out) :-   
    findall(X, (generateList(ListSOrP, List), X = List, checkList(ListSOrP, X)), Out).
    

generateList(_, []).
generateList(ListSOrP, [X|Xs]) :-
    ( \+ ground(X)
    -> between(1, 9, X), generateList(ListSOrP, [X|Xs])
    ; generateList(ListSOrP, Xs)
    ).

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
