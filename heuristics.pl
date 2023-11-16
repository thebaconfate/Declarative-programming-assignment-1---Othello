:- module(heuristics, []).
:- use_module([library(lists), board, game_logic]).

take_corner_heuristic(Player, Board, X, Y) :-
    empty_square(X, Y, Board),
    corner(X), corner(Y),
    enclosing_piece(X, Y, PLayer, Board, U, V, N).

edge(X, Y) :-
    corner(X), valid_coord(Y);
    corner(Y), valid_coord(X).

take_edge_heuristic(Player, Board, X, Y) :-
    empty_square(X, Y, Board),
    edge(X, Y),
    enclosing_piece(X, Y, Player, Board, U, V, N).

block_corner_heuristic(Player, Board, X, Y) :-
    empty_square(X, Y, Board),
    corner(X), corner(Y),
    other_player(Player, OtherPlayer),
    enclosing_piece(X, Y, OtherPlayer, Board, _, _, _),
    enclosing_piece(X, Y, Player, Board, _, _, _).

block_edge_heuristic(Player, Board, X, Y) :-
    empty_square(X, Y, Board),
    edge(X, Y),
    other_player(Player, OtherPlayer),
    enclosing_piece(X, Y, OtherPlayer, Board, _, _, _),
    enclosing_piece(X, Y, Player, Board, _, _, _).