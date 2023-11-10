:- module(game_logic, []).
:- use_module([library(lists), io, fill, board/*, winner*/]).

play :- 
    welcome, 
    initial_board(Board),
    display_board(Board),
    is_black(Black).
    % TODO implement the predicate below:
    % play(Black, Board).

match_list(0, 1, 0, Piece, [Piece]).
match_list(0, 1, 0, Piece, [Piece | Tail]).
match_list(0, Last, N, Piece, [Head | Tail]) :-
    NewLast is Last - 1,
    match_list(0, NewLast, Counter, Piece, Tail), 
    other_player(Piece, Head), N is Counter + 1;
    NewLast is Last - 1, 
    match_list(0, NewLast, Counter, Piece, Tail), 
    N is Counter.
match_list(1, Last, N, Piece, [Piece | Tail]) :-
    NewLast is Last - 1,
    match_list(0, NewLast, N, Piece, Tail).
match_list(First, Last, N, Piece, [Head | Tail]) :- 
    NewFirst is First - 1,
    NewLast is Last - 1,
    match_list(NewFirst, NewLast, N, Piece, Tail).

enclosing_piece(X, Y, Player, Board, U, Y, N) :-
    row(Y, Board, row(Y, A, B, C, D, E, F, G, H)),
    U < X, match_list(U, X, Player, N, [A, B, C, D, E, F, G, H]);
    X < U, match_list(X, U, Player, N, [A, B, C, D, E, F, G, H]).
enclosing_piece(X, Y, Player, Board, X, V, N) :-
    column(X, Board, col(X, A, B, C, D, E, F, G, H)),
    Y < V, match_list(Y, V, Player, N, [A, B, C, D, E, F, G, H]);
    V < Y, match_list(V, Y, Player, N, [A, B, C, D, E, F, G, H]).


% match_list(0, 1, N, '*', ['*', '*', '*', '*']).
potato(t, b).



