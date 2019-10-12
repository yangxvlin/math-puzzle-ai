:- ensure_loaded(proj2).

diagonal([[1,2,3], [4,5,6], [7,8,9]], X).
getNthListElements([[1,2,3], [4,5,6], [7,8,9]], 1, Row, Rows).

productList([1,2,3,4], Product).

allGround([1,2,3,4]).
allGround([1,_,3,4]).

generateList(6, [1,_,_]).
generateList(6, [_,_,_]).

Z = [_,_,_], generate(23, Z, Out).
Z = [_,1,_], generate(6, Z, Out).

Z = [[_,_,_],[_,_,_],[_,1,_]], replaceUnground([14,15,28], Z, [Out]).

Z = [[_,_,_],[1,_,_],[_,_,_]], allDigits(Z).

Z = [[_,_,_,2],[_,_,_,_],[_,_,_,_],[_,_,_,_]], generateDiagonal(Z).

Z = [[0,23,23,40,22],[840,_,_,_,_],[315,_,_,1,_],[120,_,_,_,_],[560,_,_,_,_]], puzzle_solution(Z), write(Z).
Z = [[_,_,_,_],[_,_,1,_],[_,_,_,_],[_,_,_,_]], generateDiagonal(Z).

Z = [[0,16,16,16,16],[16,_,_,_,_],[16,_,_,_,_],[16,_,_,_,_],[16,_,_,_,_]], puzzle_solution(Z).

1 ?- Z = [[0,16,16,16,16],[16,_,_,_,_],[16,_,_,_,_],[16,_,_,_,_],[16,_,_,_,_]], puzzle_solution(Z).
