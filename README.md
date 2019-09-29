# Math Puzzle solving AI

## How to run
- Windows:
    - double click ```proj2_test.pl```
    - `?- [proj2].`
    - `?- do_tests.`
    - Test result on my `i7-7700K`:
        ```
        ?- [proj2].
        true.

        ?- do_tests.
        Num Test                              Secs Status    Score    Remark
        --- ----                              ---- ------    -----    ------
        1 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        2 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        3 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        4 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        5 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        6 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        7 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        8 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        9 puzzle_solution(inout) ...        0.03   PASS   1.0/1.0   
        10 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        11 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        12 puzzle_solution(inout) ...        0.02   PASS   1.0/1.0   
        13 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        14 puzzle_solution(inout) ...        0.02   PASS   1.0/1.0   
        15 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        16 puzzle_solution(inout) ...        0.02   PASS   1.0/1.0   
        17 puzzle_solution(inout) ...        0.02   PASS   1.0/1.0   
        18 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        19 puzzle_solution(inout) ...        0.02   PASS   1.0/1.0   
        20 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   
        21 puzzle_solution(inout) ...        0.00   PASS   1.0/1.0   

        Total tests executed: 21
        Total correctness : 21.00 / 21.00 = 100.00%
        Marks earned : 100.00 / 100.00
        true.
        ```

## File directory
```
-| proj2.pdf     - project description
 | proj2.pl      - major predicates
 | proj2_test.pl - test cases
 | test.pl       - my test cases for predicates
```

## Program Description
This code is for providing the implementation for the math puzzle solver. \
It is defined in one main predicate: 
    puzzle_solution(+Puzzle) \
    puzzle_solution/1. 

The program is for solving the math puzzle with a n*n squares of grids and a
number for each row and column (a graphical representation is given below).
With constraints: 
1. the left-top to bottom right diagonal requires same digit
2. each column sums or products to the given row number
3. each row sums or products to the given row number
4. each row's digits are different
5. each column's digits are different
6. each cell of the puzzle matrix is 1-9 digit

The program approachs the solution by:
1. unifies diagonal to the same digit
2. generate each row that satisfies constraint 3) 4)
3. then do the same thing to each column by transpose the puzzle so that can reuse the predicates for rows to unifies columns 
4. ensure each cell in the n*n puzzle is ground.

The program assumes:
1. The input puzzle matrix is 2\*2, 3\*3 or 4\*4 size (i.e.: n range from 2-4 inclusively).
2. The input Puzzle data structure is made up of an ignored top left corner, bounded header numbers, puzzle matrix with partially bounded or unbounded cells. Detailed example is given below.
3. The puzzle has at least one solution or returns false if not solvable when a proper Puzzle is given.

## Major Predicate
```
%% puzzle_solution(+Puzzle).
%% It takes a Puzzle and unifies it so that the Puzzle satisfies the 
%% constraints listed above. The predicate holds when the Puzzle is the
%% representation of a solved maths puzzle. Otherwise the result is false.
%%
%% e.g.: 
%% ?- Puzzle=[[0,14,10,35],[14,_,_,_],[15,_,_,_],[28,_,1,_]],
%% |    puzzle_solution(Puzzle).
%% Puzzle = [[0, 14, 10, 35], [14, 7, 2, 1], [15, 3, 7, 5], [28, 4, 1, 7]] ;
%% false.
%%
%% Note: 1) Some cells of the puzzle can be given a digit in the input. 
%%       Otherwise input has "_" for the predicate to find a digit for unbound
%%       cell.
%%       2) The first list of Puzzle is the column number for each math puzzle 
%%       column ("0" has no meaning just for distinguishing a cell place).
%%       The rest lists' first elemens are the row number for each row. The 
%%       rest is the matrix to be found digits for.
%%       3) In the above example, [14,10,35] in first list is the column
%%       numbers. [14,15,18] is the first element of the rest lists is the row
%%       numbers. A digit 1 is given in row 3 column 2. 
%%       Represent it graphically:
%%       +---+---+---+---+                   +---+---+---+---+
%%       |  0| 14| 10| 35|                   |  0| 14| 10| 35|
%%       +---+---+---+---+                   +---+---+---+---+
%%       | 14|  _|  _|  _|                   | 14|  7|  2|  1|
%%       +---+-----------+  == solved to =>  +---+-----------+
%%       | 15|  _|  _|  _|                   | 15|  3|  7|  5|
%%       +---+-----------+                   +---+-----------+
%%       | 18|  _|  1|  _|                   | 18|  4|  1|  7|
%%       +---+-----------+                   +---+-----------+
puzzle_solution/1.
puzzle_solution(Puzzle) :-
    % unpack the input data structure to 
    %   1. column numbers: list of number for each column digits to sum or
    %                      product to.
    %   2. row numbers: list of number for each row digits to sum or product to
    %   3. matrix of the puzzle: the n*n List of bounded or unbound cells.
    Puzzle = [FirstList|RestLists],
    FirstList = [_|ColumnNumbers],
    % 1 is because index starts from 1
    % split RowNumbers and puzzle Matrix from RestLists
    lists_nth1(1, RestLists, RowNumbers, Matrix),
    
    % step 1: unifies diagonal to the same digit for constraint 1)
    % Note: It only digitizes variables that are on the diagonal for better
    %       performance purpose.
    same_diagonal_digits(Matrix),

    % step 2: unifies each row with digits that satisfies constraint 3) 4)
    % Note: This generate a candidate row and checks its validity to decide 
    % whether or not to continue instead of generating whole Matrix and check 
    % the whole matrix which gives a better performance. i.e.: it unifies
    % variables in Matrix with digit in range [1,9].
    maplist(generate_and_validate_row, RowNumbers, Matrix),

    % step 3: unifies each column with digits that satisfies constraint 2) 5)
    % Note: Matrix's cells are ground now, no need to digitize them. Reuse the
    % validate_row/2 to check columns with the help of transpose/2.
    validate_columns(Matrix, ColumnNumbers),
    
    % step 4: check each cell in Matrix is ground to satisfy constraint 6)
    maplist(label, Matrix).
```

## Feedback
