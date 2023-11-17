:- module(othello, [row/3, column/3, square/4, empty_square/3, empty_board/1, enclosing_piece/7]).
:- use_module([game_logic, board, io, fill, heuristics, winner]).

%Board representation
player_1_character('*').
player_2_character('o').
blank_character(' ').

%some abstractions
valid_coord(1).
valid_coord(2).
valid_coord(3).
valid_coord(4).
valid_coord(5).
valid_coord(6).
valid_coord(7).
valid_coord(8).

corner(1).
corner(8).

% predicates to identify symbols
% is_black(?Symbol)
% Succeeds if Symbol can be unified with player 1's character
is_black(Symbol) :- player_1_character(Symbol).

% is_white(?Symbol)
% Succeeds if Symbol can be unified with player 2's character
is_white(Symbol) :- player_2_character(Symbol).

% is_empty(?Symbol)
% Succeeds if Symbol unifies with the empty character
is_empty(Symbol) :- blank_character(Symbol).

% is_piece(?Character)
% Succeeds if Symbol unifies with the black of white symbol.
is_piece(Symbol) :- is_black(Symbol); is_white(Symbol).

% is_opponent(?Character, ?Character)
% Succeeds if Player 1 symbol unifies with black and Player 2 with white or vice versa. 
other_player(Player_1_symbol, Player_2_symbol) :- is_black(Player_1_symbol), is_white(Player_2_symbol); is_black(Player_2_symbol), is_white(Player_1_symbol).

% row(?Integer, ?Board_state, ?Row)
% Succeeds if the row can be unified with the Integer-th row of Board_state
row(Row_number, Board_state, row(Row_number, A, B, C, D, E, F, G, H )) :- 
    valid_coord(Row_number),
    nth1(Row_number, Board_state, [A, B, C, D, E, F, G, H]).

% get_i_j_value(?Integer, ?Integer, ?Board_state, ?Variable)
% Succeeds if Variable can be unified to the value stored in the Integer-th row and Integer-th column of Board_state
get_x_y_value(X, Y, Board_state, Variable) :-
    valid_coord(X), valid_coord(Y),
    nth1(Y, Board_state, Row),
    nth1(X, Row, Variable).

% column(+Integer, +Board, -ColumnList)
% Succeeds if List can be unified with the Integer-th column of Board_state
column(_, [], []).
column(Column_number, [Head_Row | Tail_rows], [Head_variable | Tail_variable]) :-
    nth1(Column_number, Head_Row, Head_variable),
    column(Column_number, Tail_rows , Tail_variable).
% column(?Integer, ?Board_state, ?List)
% Succeeds if List can be unified with the Integer-th column of Board_state
column(Column_number, Board_state, col(Column_number, A, B, C, D, E, F, G, H)) :- 
    valid_coord(Column_number),
    column(Column_number, Board_state, [A, B, C, D, E, F, G, H]).

% square(?Integer-X, ?Integer-Y, +Board_state, ?squ(?Integer-X, ?Integer-Y, ?Piece))
% Succeeds if Coordinates X, Y can be unified as long as the piece (black/white/empty) can be unified at that position
square(CoordX, CoordY, Board_state, squ(CoordX, CoordY, Piece)) :- 
    valid_coord(CoordX), 
    valid_coord(CoordY),
    get_x_y_value(CoordX, CoordY, Board_state, Piece).

% empty_square(?Integer-X, ?Integer-Y, +Board_state)
% Succeeds if the coordinates can be unified with an empty piece on the board
empty_square(CoordX, CoordY, Board_state) :- 
    is_empty(Piece),
    square(CoordX, CoordY, Board_state, squ(CoordX, CoordY, Piece)).

% initial_board(?Board_state)
% Succeeds if Board_state can be unified with the initial board state
initial_board([
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ','o','*',' ',' ',' '],
    [' ',' ',' ','*','o',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ]).

% empty_board(?Board_state)
% Succeeds if all the squares of the board van be unified anything.
empty_board([
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_]]).

% count_pieces_row(+List, -BlackPieces, -WhitePieces)
% counts the amount of pieces in a list (row)
count_pieces_row([], 0, 0).
count_pieces_row([Head | Tail], Black, White) :-
    is_black(Head), count_pieces_row(Tail, PrevBlack, White), Black is PrevBlack + 1;
    is_white(Head), count_pieces_row(Tail, Black, PrevWhite), White is PrevWhite + 1;
    is_empty(Head), count_pieces_row(Tail, Black, White).
% count_pieces(+Board, -Black, -White)
% counts the amount of pieces of black and white on the board
count_pieces([], 0, 0).
count_pieces([Head | Tail], Black, White) :- 
    count_pieces_row(Head, RowBlack, RowWhite),
    count_pieces(Tail, RestBlack, RestWhite),
    Black is RowBlack + RestBlack,
    White is RowWhite + RestWhite.

% decide_winner(+Black, +White, -Winner)
% given the amount of black and white pieces, returns which one is the winner and reports it
% or reports a draw if there is a draw. 
decide_winner(Black, White, Winner) :- 
    Black > White, is_black(Winner), report_winner(Winner);
    Black < White, is_white(Winner), report_winner(Winner);
    Black = White, is_empty(Winner), 
    report_stalemate.

% and_the_winner_is(+Board, -Winner)
% counts the amount of pieces on the board and declares a winner (or draw)
and_the_winner_is(Board, Winner) :- 
    count_pieces(Board, Black, White), 
    decide_winner(Black, White, Winner).

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



% play
% starts the othello game with black as the starting player
play :- 
    welcome,
    initial_board(Board),
    display_board(Board),
    is_black(Black),
    play(Black, Board).

% play(+Player, +Board)
% recursively play a turn of othello and switch the players' turn
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
    choose_move(Player, X, Y, Board_state),
    fill_and_flip_squares( X, Y, Player, Board_state, NewBoard),
    report_move(Player, X, Y),
    display_board(NewBoard),
    is_black(OtherPlayer),
    play(OtherPlayer, NewBoard).

% Testing section

testing_board([
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ','o','o','*',' ',' ',' '],
    [' ',' ',' ','o','*',' ',' ',' '],
    [' ',' ',' ','*','o','*',' ',' '],
    [' ',' ',' ',' ',' ','o',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ']
    ]).

empty_testing_board([
    ['*','*','o','*','*','*','o','*'],
    ['*','*','o','*','*','*','o','*'],
    ['*','*','o','o','*','*','o','*'],
    ['o','*','*','o','*','*','o','*'],
    ['o','o','*','*','*','*','o','*'],
    ['o','*','o','*','*','*','o','*'],
    ['o','o','*','*','o','*',' ','*'],
    ['*','*','*','*','*','*','o','*']
    ]).

test_is_black :-
    is_black(Piece),
    assertion(Piece == '*').

test_is_white :-
    is_white(Piece),
    assertion(Piece == 'o').
    

test_square :-
    testing_board(Board),
    is_black(Piece),
    square(X, Y, Board, squ(X, Y, Piece)),
    assertion(X == 4),
    assertion(Y == 5).
    
test_empty_square :-
    empty_testing_board(Board), 
    empty_square(X, Y, Board),
    assertion(X == 7),
    assertion(Y == 7).

test_row :-
    testing_board(Board),
    row(3, Board, Row),
    assertion(Row == row(3, ' ', ' ', 'o', 'o', '*',' ',' ',' ' )).

test_column :-
    testing_board(Board), 
    column(5, Board, Column),
    assertion(Column == column(5,' ', ' ', '*', '*', 'o',' ',' ',' '  )).

board_tests :-
    test_is_black,
    test_is_white,
    test_square,
    test_empty_square,
    test_row,
    test_column.


test_winner:-
    count_pieces([
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ','o',' ','o',' ','*',' ',' '],
        [' ','o','o',' ',' ',' ',' ',' '],
        [' ','o','o','*','o',' ',' ',' '],
        [' ','*','o',' ','o','o',' ','o'],
        [' ','o',' ','o',' ','o','o','o'],
        [' ',' ','o',' ',' ','o','o',' ']
        ], B, W),
    assertion(B == 3),
    assertion(W == 19),
    count_pieces([
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' '],
        [' ','o',' ','o',' ','*',' ',' '],
        [' ','*','*',' ',' ',' ',' ',' '],
        [' ','o','o','*','o',' ',' ',' '],
        [' ','*','*',' ','*','*',' ','o'],
        [' ','o',' ','*',' ','*','*','o'],
        [' ',' ','o',' ',' ','o','o',' ']
        ], B1, W1),
    assertion(B1 == W1),
    decide_winner(B, W, Winner),
    is_white(Winner1),
    assertion(Winner == Winner1),
    decide_winner(B1, W1, Winner2),
    is_empty(No_Winner),
    assertion(No_Winner == Winner2).



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