:- module(game_logic, [row/3, empty_square/3, enclosing_piece/7, empty_board/1, square/4]).
:- use_module([library(lists), io, fill, board, winner]).

mirror(A, B) :- 
    valid_coord(A),
    B is 8-A+1.

sortsmall(A, B, A, B) :- A < B.
sortsmall(A, B, B, A) :- B < A.

play :- 
    welcome,
    initial_board(Board),
    display_board(Board),
    is_black(Black),
    % TODO implement the predicate below:
    play(Black, Board).

match_list(0, 1, 0, Piece, [Piece]).
match_list(0, 1, 0, Piece, [Piece | _ ]).
match_list(0, Last, N, Piece, [Head | Tail]) :-
    Last > 1,
    NewLast is Last - 1,
    match_list(0, NewLast, Counter, Piece, Tail),
    other_player(Piece, Head), N is Counter + 1;
    NewLast is Last - 1,
    match_list(0, NewLast, Counter, Piece, Tail),
    N is Counter.
match_list(1, Last, N, Piece, [ _| Tail]) :-
    NewLast is Last - 1,
    match_list(0, NewLast, N, Piece, Tail),
    N > 0.
match_list(First, Last, N, Piece, [_ | Tail]) :-
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
construct_diagonal_inverted(start(U, V), end(U, V), Board, [Piece]) :-
    square(U, V, Board, squ(U, V, Piece)).
construct_diagonal_inverted(start(X, Y), end(U, V), Board, [Head | Tail]) :-
    square(X, Y, Board, squ(X, Y, Head)),
    NewX is X + 1,
    NewY is Y - 1,
    construct_diagonal_inverted(start(NewX, NewY), end(U, V), Board, Tail).
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
enclosing_piece(X, Y, Piece, Board_state, U, V, N) :-
    square(U, V, Board_state, squ(U, V, Piece)), 
    enclosing_piece_try(X, Y, Piece, Board_state, U, V, N).


%square(CoordX, CoordY, Board_state, squ(CoordX, CoordY, Piece))
/*
find_all_piece_locations(_, _, [], _, 9).
find_all_piece_locations(Board, Player, List, 9, Y) :-
    Ys is Y + 1,
    find_all_piece_locations(Board, Player, List, 1, Ys).
find_all_piece_locations(Board, Player, [[X, Y] | Tail], X, Y) :-
    square(X, Y, Board, squ(X, Y, Player)),
    Xs is X + 1,
    find_all_piece_locations(Board, Player, Tail, Xs, Y).
find_all_piece_locations(Board, Player, List , X, Y) :-
    X < 9, Y < 9,
    Xs is X + 1, 
    find_all_piece_locations(Board, Player, List, Xs, Y).
*/
/*
find_all_piece_locations([
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ','*',' ',' ',' ',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ',' '],
    [' ',' ','*',' ',' ','o',' ',' '],
    [' ','*',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], '*', List, 1, 1).
*/
/*
    enclosing_piece(7,2, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ',' '],
    [' ',' ','*','o','o',' ',' ',' '],
    [' ',' ','*',' ',' ',' ',' ',' '],
    [' ',' ','*',' ',' ','o',' ',' '],
    [' ','*',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], U, V, N).

    enclosing_piece(6,4, 'o', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ','o','*',' ',' ',' ',' '],
    [' ',' ','o','*',' ',' ',' ',' '],
    [' ',' ','o','*',' ',' ',' ',' '],
    [' ','*',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], U, V, N).

    enclosing_piece(7,2, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ',' '],
    [' ',' ','o','o','o',' ',' ',' '],
    [' ',' ','o',' ',' ',' ',' ',' '],
    [' ',' ','o',' ',' ','o',' ',' '],
    [' ','o',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], U, V, N).

    enclosing_piece(7,2, 'o', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ',' '],
    [' ',' ','o','o','o',' ',' ',' '],
    [' ',' ','o',' ',' ',' ',' ',' '],
    [' ',' ','o',' ',' ','o',' ',' '],
    [' ','o',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], U, V, N).

    enclosing_piece(4, 3, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','*',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], U, V, N).

    enclosing_piece(4, 3, '*', [
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','*',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ',' '],
    [' ',' ',' ','*',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], U, V, N).

*/

legal_squares(Board, Player) :-
    empty_square(X, Y, Board),
    enclosing_piece(X, Y, Player, Board, _, _, _).

no_more_legal_squares(Board, Player) :-
    not(legal_squares(Board, Player)).

no_more_legal_squares(Board) :-
    is_black(Black),
    is_white(White),
    no_more_legal_squares(Board, Black),
    no_more_legal_squares(Board, White).


/*
no_more_legal_squares([
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ',' '],
    [' ','*','o','o','o',' ',' ',' '],
    [' ',' ','o',' ',' ',' ',' ',' '],
    [' ',' ','o',' ',' ','o',' ',' '],
    [' ','o',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], '*').

no_more_legal_squares([
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ','*'],
    [' ',' ','o','o','o',' ',' ',' '],
    [' ',' ','o',' ',' ',' ',' ',' '],
    [' ',' ','o',' ',' ','o',' ',' '],
    [' ','o',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], 'o').

*/

/*
no_more_legal_squares([
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','*',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ], '*').

*/
/*
no_more_legal_squares(Board) :-
    is_black(Black),
    is_white(White),
    no_more_legal_squares(Board, Black),
    no_more_legal_squares(Board, White).
*/
/*
no_more_legal_squares([
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','o',' ',' ','*'],
    [' ',' ','o','o','o',' ',' ',' '],
    [' ',' ','o',' ',' ',' ',' ',' '],
    [' ',' ','o',' ',' ','o',' ',' '],
    [' ','o',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ]).

no_more_legal_squares([
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','*',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ]).


    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','*',' ',' ',' ',' '],
    [' ',' ',' ','*','*',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ]
*/
get_legal_ai_move(Player, X, Y, Board_state) :-
    format('AI is thinking...~n'),
    empty_square(X, Y, Board_state),
    enclosing_piece(X, Y, Player, Board_state, _, _, _).

play(Player, Board_state) :- 
    no_more_legal_squares(Board_state),
    and_the_winner_is(Board_state, _);
    no_more_legal_squares(Board_state, Player), 
    report_no_move(Player),
    other_player(Player, OtherPlayer),
    play(OtherPlayer, Board_state);
    /*
    code for section 3.6
    get_legal_move(Player, X, Y, Board_state),
    fill_and_flip_squares( X, Y, Player, Board_state, NewBoard), 
    display_board(NewBoard),
    other_player(Player, OtherPlayer),
    play(OtherPlayer, NewBoard).
    */
    is_black(Player),
    get_legal_move(Player, X, Y, Board_state),
    fill_and_flip_squares( X, Y, Player, Board_state, NewBoard),
    display_board(NewBoard),
    is_white(OtherPlayer),
    play(OtherPlayer, NewBoard);
    is_white(Player),
    get_legal_ai_move(Player, X, Y, Board_state),
    fill_and_flip_squares( X, Y, Player, Board_state, NewBoard),
    report_move(Player, X, Y),
    display_board(NewBoard),
    is_black(OtherPlayer),
    play(OtherPlayer, NewBoard).




