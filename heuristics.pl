:- module(heuristics, [choose_move/4, empty_board/1, square/4]).
:- use_module([library(lists), board, game_logic, fill]).


% legal_move(+Player, +Board, ?X, ?Y, -N)
% Succeeds if move at X, Y is empty and encloses a piece
legal_move(Player, Board, X, Y, N):-
    empty_square(X, Y, Board),
    enclosing_piece(X, Y, Player, Board, _, _, N).

% take_corner_heuristic(+Player, +Board, -X, -Y)
% succeeds if unification can be done on the corners and if the coordinates unify with a legal move
take_corner_heuristic(Player, Board, X, Y) :-
    corner(X), corner(Y),
    legal_move(Player, Board, X, Y, _).

% edge(?X, ?Y)
% Succeeds if X, Y can be unified with coordinates at the edges
edge(X, Y) :-
    corner(X), valid_coord(Y);
    corner(Y), valid_coord(X).

% take_edge_heuristic(+Player, +Board, ?X, ?Y)
% Succeeds if unification at X, Y is legal and is an edge position
take_edge_heuristic(Player, Board, X, Y) :-
    empty_square(X, Y, Board),
    edge(X, Y),
    enclosing_piece(X, Y, Player, Board, _, _, _).

% block_corner_heuristic(+Player, +Board, -A, -B)
% Succeeds if the player can block the other player from placing a corner piece in the next move
% return the coordinates required to block the other player
block_corner_heuristic(Player, Board, A, B) :-
    empty_square(X, Y, Board),
    corner(X), corner(Y),
    other_player(Player, OtherPlayer),
    enclosing_piece(X, Y, OtherPlayer, Board, _, _, _),
    empty_square(A, B, Board),
    enclosing_piece(A, B, Player, Board, _, _, _),
    fill_and_flip_squares( A, B, Player, Board, NewBoard),
    not(enclosing_piece(X, Y, OtherPlayer, NewBoard, _, _, _)).

% block_edge_heuristic(+Player, +Board, -A, -B)
% Succeeds if the player can block the other player from placing an edge piece in the next turn
% returns the coordinates required to block players' move
block_edge_heuristic(Player, Board, A, B) :-
    empty_square(X, Y, Board),
    edge(X, Y),
    other_player(Player, OtherPlayer),
    enclosing_piece(X, Y, OtherPlayer, Board, _, _, _),
    empty_square(A, B, Board),
    enclosing_piece(A, B, Player, Board, _, _, _),
    fill_and_flip_squares(A, B, Player, Board, NewBoard),
    not(enclosing_piece(X, Y, OtherPlayer, NewBoard, _, _,_)).

% pick_biggest_n_heuristic(+Player, +Board, +CoordX, +CoordY)
% returns the coordinates of the square that the player has to take to take the most pieces of the opponent
% setof returns a sorted list of possibilities sorted N from small to large , last returns highest N coordinates
pick_biggest_n_heuristic(Player, Board, CoordX, CoordY):-
    setof([N, CoordX1, CoordY1], legal_move(Player, Board, CoordX1, CoordY1, N), Set),
    format('~w',[Set]),
    last(Set, [_, CoordX, CoordY]).

% choose_move(+Player, -X, -Y, +Board)
% returns the coordinates of a chosen move, depending on the heuristics
choose_move(Player, X, Y, Board) :-
    take_corner_heuristic(Player, Board, X, Y);
    block_corner_heuristic(Player, Board, X, Y);
    take_edge_heuristic(Player, Board, X, Y);
    block_edge_heuristic(Player, Board, X, Y);
    pick_biggest_n_heuristic(Player, Board, X, Y).

test_heuristics :-
    take_corner_heuristic('o', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*','o','o',' ','o'],
        [' ',' ',' ','*',' ',' ','*','*'],
        [' ',' ',' ',' ',' ','o','*',' ']
        ], _, _),
    take_corner_heuristic('o', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ','*'],
        [' ',' ',' ','o',' ',' ',' ','o'],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*','o','o',' ','o'],
        [' ',' ',' ','*',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ','o',' ',' ']
        ], _, _),
    take_corner_heuristic('o', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*','o','o',' ','*'],
        [' ',' ',' ','*',' ',' ','o','*'],
        [' ','*','o',' ',' ','o','o',' ']
        ], _, _),
    take_edge_heuristic('o', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*','o','o',' ','*'],
        [' ',' ',' ','*',' ',' ','o','*'],
        [' ','*','*',' ',' ','o','o',' ']
        ], _, _),
    block_corner_heuristic('o', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o',' ',' ',' ',' '],
        [' ',' ','o','o',' ',' ',' ',' '],
        [' ',' ','o','*','o',' ',' ',' '],
        [' ',' ','o','*','o','*',' ','o'],
        [' ',' ',' ','*',' ','o','o','o'],
        [' ','*','o',' ',' ','o','o',' ']
        ], _,_),
    block_edge_heuristic('o', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ','o',' ','o',' ','*',' ',' '],
        [' ','o','o',' ',' ',' ',' ',' '],
        [' ','o','o','*','o',' ',' ',' '],
        [' ','*','o',' ','o','o',' ','o'],
        [' ','o',' ','o',' ','o','o','o'],
        [' ',' ','o',' ',' ','o','o',' ']
        ], _,_).