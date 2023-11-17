:- module(heuristics, [choose_move/4]).
:- use_module([library(lists), board, game_logic, fill]).

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

block_corner_heuristic(Player, Board, A, B) :-
    empty_square(X, Y, Board),
    corner(X), corner(Y),
    other_player(Player, OtherPlayer),
    enclosing_piece(X, Y, OtherPlayer, Board, _, _, _),
    empty_square(A, B, Board),
    enclosing_piece(A, B, Player, Board, _, _, _),
    fill_and_flip_squares( A, B, Player, Board, NewBoard),
    not(enclosing_piece(X, Y, OtherPlayer, NewBoard, _, _, _)).

block_edge_heuristic(Player, Board, A, B) :-
    empty_square(X, Y, Board),
    edge(X, Y),
    other_player(Player, OtherPlayer),
    enclosing_piece(X, Y, OtherPlayer, Board, _, _, _),
    empty_square(A, B, Board),
    enclosing_piece(A, B, OtherPlayer, Board, _, _, _),
    fill_and_flip_squares(A, B, Player, Board, NewBoard),
    not(enclosing_piece(X, Y, OtherPlayer, NewBoard, _, _,_)).



legal_move(Player, Board, X, Y, N):-
    empty_square(X, Y, Board),
    enclosing_piece(X, Y, Player, Board, U, V, N).

pick_biggest_n_heuristic(Player, Board, CoordX, CoordY):-
    setof([N, CoordX1, CoordY1], legal_move(Player, Board, CoordX1, CoordY1, N), Set),
    format('~w',[Set]),
    last(Set, [_, CoordX, CoordY]).

/*
pick_biggest_n_heuristic('o', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ','o','*',' ',' ',' ',' '],
    [' ',' ','o','*',' ',' ',' ',' '],
    [' ',' ','o','*','o',' ',' ',' '],
    [' ',' ',' ','*',' ',' ',' ',' '],
    [' ',' ',' ','o',' ',' ',' ',' ']
    ], X, Y).
*/

choose_move(Player, X, Y, Board) :-
    take_corner_heuristic(Player, Board, X, Y);
    block_corner_heuristic(Player, Board, X, Y);
    take_edge_heuristic(Player, Board, X, Y);
    block_edge_heuristic(Player, Board, X, Y);
    pick_biggest_n_heuristic(Player, Board, X, Y).