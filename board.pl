:- module(board, [row/3, column/3, is_black/1, is_white/1, is_empty/1, initial_board/1, other_player/2, square/4, empty_square/3, empty_board/1, valid_coord/1, corner/1]).
:- use_module([library(lists)]).

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

board_tests.