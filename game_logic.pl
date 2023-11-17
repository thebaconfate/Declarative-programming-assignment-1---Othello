:- module(game_logic, [enclosing_piece/7, no_more_legal_squares/1, no_more_legal_squares/2]).
:- use_module([board]).

% mirror(+Integer, -Integer)
% returns the mirrored coordinate
mirror(A, B) :- 
    valid_coord(A),
    B is 8-A+1.

% sortsmall(+Integer, +Integer, -Integer, -Integer)
% returns the sorted integers as third and fourth argument
sortsmall(A, B, A, B) :- A < B.
sortsmall(A, B, B, A) :- B < A.

% match_list(+Integer, +Integer, +Piece, +List)
% Verifies if theres pieces the list encloses
match_list(0, 1, 0, Piece, [Piece]).
match_list(0, 1, 0, Piece, [Piece | _ ]).
match_list(0, Last, N, Piece, [Head | Tail]) :-
    Last > 1,
    NewLast is Last - 1,
    match_list(0, NewLast, Counter, Piece, Tail),
    other_player(Piece, Head), N is Counter + 1.
match_list(1, Last, N, Piece, [ _| Tail]) :-
    NewLast is Last - 1,
    match_list(0, NewLast, N, Piece, Tail),
    N > 0.
match_list(First, Last, N, Piece, [_ | Tail]) :-
    First > 1, 
    NewFirst is First - 1,
    NewLast is Last - 1,
    match_list(NewFirst, NewLast, N, Piece, Tail).

% construct_diagonal(+start(Integer, Integer), +end(Integer, Integer), +Board, -List)
% returns the diagonal as a list (works for coords from topleft to bottomright or vice versa)
construct_diagonal(start(U, V), end(U, V), Board, [Piece]) :-
    square(U, V, Board, squ(U, V, Piece)).
construct_diagonal(start(X, Y), end(U, V), Board, [Head | Tail]) :-
    square(X, Y, Board, squ(X, Y, Head)),
    NewX is X + 1,
    NewY is Y + 1,
    construct_diagonal(start(NewX, NewY), end(U, V), Board, Tail).
% construct_diagonal(+start(Integer, Integer), +end(Integer, Integer), +Board, -List)
% returns the diagonal as a list (works from topright to leftbottom or vice versa)
construct_diagonal_inverted(start(U, V), end(U, V), Board, [Piece]) :-
    square(U, V, Board, squ(U, V, Piece)).
construct_diagonal_inverted(start(X, Y), end(U, V), Board, [Head | Tail]) :-
    square(X, Y, Board, squ(X, Y, Head)),
    NewX is X + 1,
    NewY is Y - 1,
    construct_diagonal_inverted(start(NewX, NewY), end(U, V), Board, Tail).
% enclosing_piece_try(+X, +Y, +Player, +U, +V, -N)
% depending on the direction of the coordinates, it builds the list of elements
% between X, Y and U, V and calls the predicate to confirm it encloses pieces
enclosing_piece_try(X, Y, Player, Board, U, Y, N) :-
    row(Y, Board, row(Y, A, B, C, D, E, F, G, H)),
    U < X,
    mirror(X, X1),
    mirror(U, U1),
    sortsmall(U1, X1, S1, S2),
    match_list(S1, S2, N, Player, [H, G, F, E, D, C, B, A]);
    row(Y, Board, row(Y, A, B, C, D, E, F, G, H)),
    X < U, 
    match_list(X, U, N, Player, [A, B, C, D, E, F, G, H]).
enclosing_piece_try(X, Y, Player, Board, X, V, N) :-
    column(X, Board, col(X, A, B, C, D, E, F, G, H)),
    Y < V, match_list(Y, V, N, Player, [A, B, C, D, E, F, G, H]);
    column(X, Board, col(X, A, B, C, D, E, F, G, H)),
    V < Y, 
    mirror(Y, Y1),
    mirror(V, V1),
    sortsmall(V1, Y1, S1, S2),
    match_list(S1, S2, N, Player, [H, G, F, E, D, C, B, A]).
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

% enclosing_piece(+X, +Y, +Piece, +Board_state, -U, -V, -N)
% Succeeds if there is a piece U, V that X, Y encloses and how many it encloses (N)
enclosing_piece(X, Y, Piece, Board_state, U, V, N) :-
    square(U, V, Board_state, squ(U, V, Piece)), 
    enclosing_piece_try(X, Y, Piece, Board_state, U, V, N).

% legal_squares(+Board, +Player)
% Succeeds if there is a move a player can play
legal_squares(Board, Player) :-
    empty_square(X, Y, Board),
    enclosing_piece(X, Y, Player, Board, _, _, _).

% no_more_legal_squares(+Board, +Player)
% negation of legal_squares, succeeds if there are no more plays a player can make
no_more_legal_squares(Board, Player) :-
    not(legal_squares(Board, Player)).

% no_more_legal_squares(+Board)
% checks if both players don't have any moves that can be played
no_more_legal_squares(Board) :-
    is_black(Black),
    is_white(White),
    no_more_legal_squares(Board, Black),
    no_more_legal_squares(Board, White).

test_enclosing_piece :-
    not(enclosing_piece(7,2, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','*','o','o',' ',' ',' '],
        [' ',' ','*',' ',' ',' ',' ',' '],
        [' ',' ','*',' ',' ','o',' ',' '],
        [' ','*',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _)),
    not(enclosing_piece(6,4, 'o', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ',' ','*','o',' ',' ',' '],
        [' ',' ',' ','*',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _)),
    not(enclosing_piece(1,5, 'o', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*',' ',' ',' ',' '],
        [' ',' ','o','*','o',' ',' ',' '],
        [' ',' ',' ','*',' ',' ',' ',' '],
        [' ',' ',' ','o',' ',' ',' ',' ']
        ], _, _, _)),
    not(enclosing_piece(7,2, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','o','o','o',' ',' ',' '],
        [' ',' ','o',' ',' ',' ',' ',' '],
        [' ',' ','o',' ',' ','o',' ',' '],
        [' ','o',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _)),
    not(enclosing_piece(7,2, 'o', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','o','o','o',' ',' ',' '],
        [' ',' ','o',' ',' ',' ',' ',' '],
        [' ',' ','o',' ',' ','o',' ',' '],
        [' ','o',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _)),
    enclosing_piece(4, 3, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','*',' ',' ',' '],
        [' ',' ',' ','*','o',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _),
    enclosing_piece(6,3, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','*','o','o',' ',' ',' '],
        [' ',' ','*','*',' ',' ',' ',' '],
        [' ',' ','*',' ',' ','o',' ',' '],
        [' ','*',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _),
    enclosing_piece(4,2, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','*','o','o',' ',' ',' '],
        [' ',' ','*','*',' ',' ',' ',' '],
        [' ',' ','*',' ',' ','o',' ',' '],
        [' ','*',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _),
    enclosing_piece(6,3, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','*','o','o',' ',' ',' '],
        [' ',' ','*','*',' ',' ',' ',' '],
        [' ',' ','*',' ',' ','o',' ',' '],
        [' ','*',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _),
    enclosing_piece(6,4, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','*','o','o',' ',' ',' '],
        [' ',' ','*','*',' ',' ',' ',' '],
        [' ',' ','*',' ',' ','o',' ',' '],
        [' ','*',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _),
    enclosing_piece(3,5, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ','*',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','*','o','o',' ',' ',' '],
        [' ',' ',' ','*',' ',' ',' ',' '],
        [' ',' ','*',' ',' ','o',' ',' '],
        [' ','*',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _),
    enclosing_piece(3,2, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','*','o','o',' ',' ',' '],
        [' ',' ','*','*',' ','*',' ',' '],
        [' ',' ','*',' ',' ','o',' ',' '],
        [' ','*',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _),
    enclosing_piece(6,5, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ','*',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ','*','o','o',' ',' ',' '],
        [' ',' ','*','*',' ',' ',' ',' '],
        [' ',' ','*',' ',' ','o',' ',' '],
        [' ','*',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _),
    enclosing_piece(4, 3, '*', [
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','*',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ',' ',' ','*',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], _, _, _).

test_legal_squares :-
    not(no_more_legal_squares([
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ',' '],
        [' ','*','o','o','o',' ',' ',' '],
        [' ',' ','o',' ',' ',' ',' ',' '],
        [' ',' ','o',' ',' ','o',' ',' '],
        [' ','o',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], '*')),
    no_more_legal_squares([
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ','*'],
        [' ',' ','o','o','o',' ',' ',' '],
        [' ',' ','o',' ',' ',' ',' ',' '],
        [' ',' ','o',' ',' ','o',' ',' '],
        [' ','o',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], 'o'),
    not(no_more_legal_squares([
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','*',' ',' ',' '],
        [' ',' ',' ','*','o',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ], '*')),
    no_more_legal_squares([
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ','o','o',' ',' ','*'],
        [' ',' ','o','o','o',' ',' ',' '],
        [' ',' ','o',' ',' ',' ',' ',' '],
        [' ',' ','o',' ',' ','o',' ',' '],
        [' ','o',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
                ]),
    not(no_more_legal_squares([
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ','o',' ',' ','*',' ',' '],
        [' ',' ',' ','o','*',' ',' ',' '],
        [' ',' ',' ','*','o',' ',' ',' '],
        [' ',' ','*',' ',' ','o',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ']
        ])).

test_game_logic :-
    test_enclosing_piece,
    test_legal_squares.