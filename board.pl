:- module(board, [row/3, column/3, is_black/1, is_white/1, is_empty/1, initial_board/1, other_player/2, square/4, empty_square/3, empty_board/1, initial_board_test/1, valid_coord/1, corner/1]).
:- use_module([library(lists),
io,
fill]).

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
% is_black(?Character) :- Succeeds if Character is player 1's symbol
is_black(Symbol) :- player_1_character(Symbol).

% is_white(?Character) :- Succeeds if Character is player 2's symbol
is_white(Symbol) :- player_2_character(Symbol).

% is_empty(?Character) :- Succeeds if Character is the empty square symbol
is_empty(Symbol) :- blank_character(Symbol).

% is_piece(?Character) :- Succeeds if Character is a piece (black or white)
is_piece(Symbol) :- is_black(Symbol); is_white(Symbol).

% is_opponent(?Character, ?Character) :- Succeeds if both arguments are different pieces but not the blank piece.
other_player(Player_1_symbol, Player_2_symbol) :- is_black(Player_1_symbol), is_white(Player_2_symbol); is_black(Player_2_symbol), is_white(Player_1_symbol).

% row(?Integer, ?Board_state, ?Row) :- Succeeds if the row can be unified with the Integer-th row of Board_state
row(Row_number, Board_state, row(Row_number, A, B, C, D, E, F, G, H )) :- 
    valid_coord(Row_number),
    nth1(Row_number, Board_state, [A, B, C, D, E, F, G, H]).

% get_i_j_value(?Integer, ?Integer, ?Board_state, ?Variable) :- Succeeds if Variable can be unified to the value stored in the Integer-th row and Integer-th column of Board_state
get_x_y_value(X, Y, Board_state, Variable) :-
    valid_coord(X), valid_coord(Y),
    nth1(Y, Board_state, Row),
    nth1(X, Row, Variable).

% column(?Any, ?Board_state, ?Empty_List) :- Base case for the recursive call of column/4
column(_, [], []).
% column(?Integer, ?Board_state, ?List) :- Succeeds if List can be unified with the Integer-th column of Board_state
column(Column_number, [Head_Row | Tail_rows], [Head_variable | Tail_variable]) :-
    nth1(Column_number, Head_Row, Head_variable),
    column(Column_number, Tail_rows , Tail_variable).
% column(?Integer, ?Board_state, ?List) :- Succeeds if List can be unified with the Integer-th column of Board_state
column(Column_number, Board_state, col(Column_number, A, B, C, D, E, F, G, H)) :- 
    valid_coord(Column_number),
    column(Column_number, Board_state, [A, B, C, D, E, F, G, H]).

% square(?Integer-X, ?Integer-Y, ?Board_state, ?squ(?Integer-X, ?Integer-Y, ?Piece)) :- Succeeds if Piece can be unified with the value stored in the Integer-th row and Integer-th column of Board_state. The coordinates must also be between 0 and 9.
square(CoordX, CoordY, Board_state, squ(CoordX, CoordY, Piece)) :- 
    valid_coord(CoordX), 
    valid_coord(CoordY),
    get_x_y_value(CoordX, CoordY, Board_state, Piece).

% empty_square(?Integer-X, ?Integer-Y, ?Board_state) :- Succeeds if Piece can be unified the empty square symbol and the value of the square at the given coordinates. The coordinates must also be between 0 and 9.
empty_square(CoordX, CoordY, Board_state) :- 
    is_empty(Piece),
    square(CoordX, CoordY, Board_state, squ(CoordX, CoordY, Piece)).

%initial_board(?Board_state) :- Succeeds if Board_state can be unified with the initial board state
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

%board_of_Xpieces(?Board_state, ?Piece) :- Succeeds if Board_state can be unified with a board state where all the squares are filled with Piece.
board_of_Xpieces([], _).
board_of_Xpieces([Head_rows | Rest_rows], Piece) :- 
    list_to_set(Head_rows, [Piece]),
    board_of_Xpieces(Rest_rows, Piece).

%empty_board(?Board_state) :- Succeeds if all the squares of the board van be unified with Piece and the empty square character.
empty_board([
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_]]).