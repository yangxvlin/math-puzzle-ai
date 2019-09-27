:- ensure_loaded(proj2).

diagonal([[1,2,3], [4,5,6], [7,8,9]], X).
getNthListElements([[1,2,3], [4,5,6], [7,8,9]], 1, Columns, Rows).

productList([1,2,3,4], Product).

allGround([1,2,3,4]).
allGround([1,_,3,4]).

generateList(6, [1,_,_]).
generateList(6, [_,_,_]).

Z = [_,_,_], generate(23, Z, Out).
Z = [_,1,_], generate(6, Z, Out).

Z = [[_,_,_],[_,_,_],[_,1,_]], replaceUnground([14,15,28], Z, [Out]).

Z = [[_,_,_],[1,_,_],[_,_,_]], allDigits(Z).