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
    NewLast is Last - 1,
    match_list(0, NewLast, N, Piece, Tail),
    N > 0.
match_list(First, Last, N, Piece, [Head | Tail]) :-
    First > 1, 
    NewFirst is First - 1,
    NewLast is Last - 1,
    match_list(NewFirst, NewLast, N, Piece, Tail).construct_diagonal(start(U, V), end(U, V), Board, [Piece]) :-
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
try_wrapper([A, B], A, B).
enclosing_piece(_, _, _, _, _, _, _, []).
enclosing_piece(X, Y, Player, Board, U, V, N, [Head | Tail]) :-
    try_wrapper(Head, U, V),
    enclosing_piece_try(X, Y, Player, Board, U, V, N);
    enclosing_piece(X, Y, Player, Board, U, V, N, Tail).
enclosing_piece(X, Y, Player, Board, U, V, N) :-
    find_all_piece_locations(Board, Player, List, 1, 1),
    not(List = []),
    enclosing_piece(X, Y, Player, Board, U, V, N, List).