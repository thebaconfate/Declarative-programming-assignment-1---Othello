:- use_module([library(lists), io, fill, board, winner]).
:- module(game_logic, []).


play :- 
    welcome, 
    initial_board(Board),
    display_board(Board),
    is_black(Black).
    % TODO implement the predicate below:
    % play(Black, Board).


/* match_list(First, Last, N, Piece, [Head | Tail]) :- 
    First = 1, match_list(1, Last, N, Piece, [Head | Tail]);
    FirstSucc is First - 1, 
    match_list(FirstSucc, Last, N, Piece, Tail).


enclosing_piece(X, Y, Player, Board, U, Y, N) :-
    row(Y, Board, row(Y, A, B, C, D, E, F, G, H)),
    U < X, match_list(U, X, Player, N, [A, B, C, D, E, F, G, H]);
    X < U, match_list(X, U, Player, N, [A, B, C, D, E, F, G, H]).



 */



