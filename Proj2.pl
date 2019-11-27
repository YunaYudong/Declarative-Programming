%  Author   : Yu Dong (928922)
%  Origin   : Mon May 20 11:56:54 2019
%  Purpose  : Solution for Math Puzzles of proj2
%  Copyright: (c) 2019 Yu Dong. All rights reserved.
%
%  The program is to solve the maths puzzle with the following constrains.
%  
%  Constrains on Maths Puzzles
% 1. digits in the diagnal(left to right) square are the same
% 2. no repeat digits in a row or a column
% 3. single digit from 1 to 9 in a row or a column
% 4. headings of each row or column holds either sum or product 
%    of all the digit in that row or column

% load the library.
:- ensure_loaded(library(clpfd)).

% first check diagonal of subsquare.
% solve puzzle to meets three other constrains.
% transpose the puzzle to check the column in the same way.
puzzle_solution(Rows) :-
    remove_heading(Rows, Subsquare),
    check_diagonal(Subsquare),
    length(Rows, Size),
    solve_puzzle(Rows, Size),
    transpose(Rows, Columns),
    solve_puzzle(Columns, Size).

% remove_heading predicate is to remove all the headings.
% to get subsquare for diagonal checking.
% remove the first row, transpose puzzle, remove the first row again,
% transpose back and get the subsquare of puzzle without headings.
remove_row([],[]).
remove_row([_|Tail],Tail).
remove_heading([[H|T]|L],Subsquare):-
    remove_row([[H|T]|L],Square1),
    transpose(Square1,Square2),
    remove_row(Square2,Square3),
    transpose(Square3,Subsquare).

% if the puzzle has headings ignore first row.
% if puzzle do not have headings, check whether they are all digits 
% from 1 ro 9 and they are valid row.
solve_puzzle([], _).
solve_puzzle([[H|T]|L], Size) :-
    (length([[H|T]|L], Size) ->
     solve_puzzle(L, Size)
    ; 
     all_digits(T),
     valid_row([H|T]),
     solve_puzzle(L, Size)
    ).

% check whether first element in the first row equals to second 
% element in the next row. 
check_diagonal(L) :-
    vaild_diagonal(L, 0).
vaild_diagonal([_|[]],_).
vaild_diagonal([Line,Line_next|L],N):-
    N_next is N+1,
    nth0(N,Line,E),
    nth0(N_next, Line_next, E),
    vaild_diagonal([Line_next|L],N_next).

% if the element in the puzzle is unsolved, fill in a digit from 1 to 9
all_digits([]).
all_digits([E|Elt]) :-
    (\+ ground(E) -> 
     between(1, 9, E),
     all_digits(Elt)
    ; 
     all_digits(Elt)
    ).

% if the length of a list are the same before or after sort,
% there is no repeat digits in a row.
no_repeat(L):-
    sort(L,Sorted),
    length(L,Length),
    length(Sorted,Length).

% check whether heading holds the sum or product of each row
% check no repeat digits in a row (ignore heading)
valid_row([H|T]):-
   (sum(T,H)
   ;
    product(T,H)
   ), no_repeat(T).

% sum predicate takes a list and Head value, 
% to check whether the sum of all the values in the list equals to the Head value.
sum([],0).
sum([H|T], Head):-
    sum(T,Sum),
    Head is Sum+H.

% product predicate takes a list and Head value, 
% to check whether the product of all the values in the list equals to the Head value.
product([],1).
product([H|T], Head):-
    product(T,Product),
    Head is Product*H. 
