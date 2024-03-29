%1.correspond
correspond(E1, [E1|_], E2, [E2|_]).
correspond(E1, [_|T1], E2, [_|T2]) :-
   correspond(E1, T1, E2, T2).

%2.interleave
%interleave_list predicate handels the head list of Ls
interleave_list([],L,[],L).
    interleave_list([[Ls_h|Ls_t]|Lss],[Ls_h|Lr],[Ls_t|Lsss],L):-
    interleave_list(Lss,Lr,Lsss,L).

interleave([],[]).
interleave([[]|Ls_t],[]):-
    interleave(Ls_t,[]).
interleave([H|Ls_t],[Hs|Lr]):-
    interleave_list([H|Ls_t],[Hs|Lr],Lssss,L),
    interleave(Lssss,L).
 
 %3.partial_eval 
 %a number
partial_eval(Expr0,_, _, Expr):-
   number(Expr0),
   Expr = Expr0.
   
%an atom
partial_eval(Expr0,Var,Val,Expr):-
   atom(Expr0),
   (Expr0 = Var ->
       Expr = Val
   ; Expr = Expr0
   ).

%x+y, x and y are arithmetic expressions
partial_eval(A+B,Var,Val,E):-
   partial_eval(A,Var,Val,X),
   partial_eval(B,Var,Val,Y),
   (number(X),number(Y) ->
      E is X+Y
   ; E = X+Y
   ).

%x-y, x and y are arithmetic expression
partial_eval(A-B,Var,Val,E):-
    partial_eval(A,Var,Val,X),
    partial_eval(B,Var,Val,Y),
    (number(X),number(Y) ->
       E is X-Y
    ; E = X-Y
    ). 

%x*y, x and y are arithmetic expression
partial_eval(A*B,Var,Val,E):-
    partial_eval(A,Var,Val,X),
    partial_eval(B,Var,Val,Y),
    (number(X),number(Y) ->
       E is X*Y
    ; E = X*Y
    ).

%x/y, x and y are arithmetic expression
partial_eval(A/B,Var,Val,E):-
   partial_eval(A,Var,Val,X),
   partial_eval(B,Var,Val,Y),
   (number(X),number(Y) ->
      E is X/Y
   ; E = X/Y
   ).

%x//y, x and y are arithmetic expression
partial_eval(A//B,Var,Val,E):-
   partial_eval(A,Var,Val,X),
   partial_eval(B,Var,Val,Y),
   (number(X),number(Y)->
      E is X//Y
   ; E = X//Y
   ).