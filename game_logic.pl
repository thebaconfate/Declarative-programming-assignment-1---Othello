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
match_list(1, Last, N, Piece, [Head | Tail]) :-
    is_empty(Head),
    NewLast is Last - 1,
    match_list(0, NewLast, N, Piece, Tail),
    N > 0.
match_list(First, Last, N, Piece, [Head | Tail]) :-
    First > 1, 
    NewFirst is First - 1,
    NewLast is Last - 1,
    match_list(NewFirst, NewLast, N, Piece, Tail).

construct_diagonal(start(U, V), end(U, V), Board, [Piece]) :-
    square(U, V, Board, squ(U, V, Piece)).
construct_diagonal(start(X, Y), end(U, V), Board, [Head | Tail]) :-
    square(X, Y, Board, squ(X, Y, Head)),
    NewX is X + 1,
    NewY is Y + 1,
    construct_diagonal(start(NewX, NewY), end(U, V), Board, Tail).

/*
construct_diagonal(start(1, 1), end(8, 8), [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ','*',' ',' ',' ',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], List).

*/
construct_diagonal_inverted(start(U, V), end(U, V), Board, [Piece]) :-
    square(U, V, Board, squ(U, V, Piece)).
construct_diagonal_inverted(start(X, Y), end(U, V), Board, [Head | Tail]) :-
    square(X, Y, Board, squ(X, Y, Head)),
    NewX is X + 1,
    NewY is Y - 1,
    construct_diagonal_inverted(start(NewX, NewY), end(U, V), Board, Tail).

/*
construct_diagonal_inverted(start(1, 8), end(8, 1), [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ','*',' '],
    [' ',' ','*',' ',' ','*',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], List).
*/
enclosing_piece_try(X, Y, Player, Board, U, Y, N) :-
    row(Y, Board, row(Y, A, B, C, D, E, F, G, H)),
    U < X, match_list(U, X, N, Player, [H, G, F, E, D, C, B, A]);
    row(Y, Board, row(Y, A, B, C, D, E, F, G, H)),
    X < U, match_list(X, U, N, Player, [A, B, C, D, E, F, G, H]).
enclosing_piece_try(X, Y, Player, Board, X, V, N) :-
    column(X, Board, col(X, A, B, C, D, E, F, G, H)),
    Y < V, match_list(Y, V, N, Player, [A, B, C, D, E, F, G, H]);
    column(X, Board, col(X, A, B, C, D, E, F, G, H)),
    V < Y, match_list(V, Y, N, Player, [H, G, F, E, D, C, B, A]).
enclosing_piece_try(X, Y, Player, Board, U, V, N) :-
    X < U, Y < V, construct_diagonal(start(X, Y), end(U, V), Board, List),
    NewEnd is U - X + 1, match_list(1, NewEnd, N, Player, List);
    U < X, V < Y, construct_diagonal(start(U, V), end(X, Y), Board, List),
    reverse(List, ReverserdList),
    NewEnd is X - U + 1, match_list(1, NewEnd, N, Player, ReverserdList);
    X < U, Y > V, construct_diagonal_inverted(start(X, Y), end(U, V), Board, List),
    NewEnd is U - X + 1, match_list(1, NewEnd, N, Player, List);
    U < X, V > Y, construct_diagonal_inverted(start(U, V), end(X, Y), Board, List),
    reverse(List, ReverserdList),
    NewEnd is X - U + 1, match_list(1, NewEnd, N, Player, ReverserdList).


% match_list(0, 1, N, '*', ['*', '*', '*', '*']).

/* 
Horizontal testing: Scenario 1:
enclosing_piece_try(6, 4, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], 3, 4, N).


Scenario 2: 
enclosing_piece_try(3, 4, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','o','*',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], 6, 4, N).

Vertical testing: Scenario 1: 
enclosing_piece_try(4, 3, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], 4, 5, N).

Scenario 2:
enclosing_piece_try(4, 6, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','*',' ',' ',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ',' ','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], 4, 3, N).

Diagonal testing: Scenario 1:
enclosing_piece_try(3,3, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ','*',' '],
    [' ',' ',' ',' ',' ','*',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ','o',' ',' '],
    [' ',' ',' ',' ',' ',' ','*',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], 7, 7, N).

scenario 3:
enclosing_piece_try(7,7, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ','*',' '],
    [' ',' ','*',' ',' ','*',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ','o',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], 3, 3, N).

scenario 3:
enclosing_piece_try(3,6, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ','*',' '],
    [' ',' ','*',' ',' ','*',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ',' '],
    [' ',' ',' ',' ',' ','o',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], 7, 2, N).

scenario 4
enclosing_piece_try(7,2, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ','*',' ',' ',' ',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ',' '],
    [' ',' ','*',' ',' ','o',' ',' '],
    [' ','*',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], 3, 6, N).

*/
construct_locationlist_player_pieces(Player, Board, Locationlist) :-
    findall(squ(X, Y, Player), square(X, Y, Board, squ(X, Y, Player)), Locationlist).

enclosing_piece(X, Y, Player, Board, U, V, N) :-
    construct_locationlist_player_pieces(Player, Board, Locationlist).