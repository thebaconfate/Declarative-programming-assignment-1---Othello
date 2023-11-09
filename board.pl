:- module(board, [row/3]).
:- use_module([library(lists),
io,
fill]).

%Board representation
player_1_character('*').
player_2_character('o').
blank_character(' ').

%some abstractions
between0_9(Number) :- 0 < Number, Number < 9.

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
    between0_9(Row_number), 
    nth1(Row_number, Board_state, [A, B, C, D, E, F, G, H]).

% get_i_j_value(?Integer, ?Integer, ?Board_state, ?Variable) :- Succeeds if Variable can be unified to the value stored in the Integer-th row and Integer-th column of Board_state
get_i_j_value(I, J, Board_state, Variable) :-
    nth1(I, Board_state, Row),
    nth1(J, Row, Variable).

% column(?Any, ?Board_state, ?Empty_List) :- Base case for the recursive call of column/4
column(_, [], []).
% column(?Integer, ?Board_state, ?List) :- Succeeds if List can be unified with the Integer-th column of Board_state
column(Column_number, [Head_Row | Tail_rows], [Head_variable | Tail_variable]) :-
    nth1(Column_number, Head_Row, Head_variable),
    column(Column_number, Tail_rows , Tail_variable).
% column(?Integer, ?Board_state, ?List) :- Succeeds if List can be unified with the Integer-th column of Board_state
column(Column_number, Board_state, col(Column_number, A, B, C, D, E, F, G, H)) :- 
    between0_9(Column_number),
    column(Column_number, Board_state, [A, B, C, D, E, F, G, H]).

% square(?Integer-X, ?Integer-Y, ?Board_state, ?squ(?Integer-X, ?Integer-Y, ?Piece)) :- Succeeds if Piece can be unified with the value stored in the Integer-th row and Integer-th column of Board_state. The coordinates must also be between 0 and 9.
square(CoordX, CoordY, Board_state, squ(CoordX, CoordY, Piece)) :- 
    between0_9(CoordX), 
    between0_9(CoordY),
    get_i_j_value(CoordX, CoordY, Board_state, Piece).

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
    [AA,AB,AC,AD,AE,AF,AG,AH],
    [BA,BB,BC,BD,BE,BF,BG,BH],
    [CA,CB,CC,CD,CE,CF,CG,CH],
    [DA,DB,DC,DD,DE,DF,DG,DH],
    [EA,EB,EC,ED,EE,EF,EG,EH],
    [FA,FB,FC,FD,FE,FF,FG,FH],
    [GA,GB,GC,GD,GE,GF,GG,GH],
    [HA,HB,HC,HD,HE,HF,HG,HH]
    ]).

count_pieces_row([], 0, 0).
count_pieces_row([Head | Tail], Black, White) :-
    is_black(Head), count_pieces_row(Tail, PrevBlack, White), Black is PrevBlack + 1;
    is_white(Head), count_pieces_row(Tail, Black, PrevWhite), White is PrevWhite + 1;
    count_pieces_row(Tail, Black, White).
count_pieces([], 0, 0).
count_pieces([Head | Tail], Black, White) :- 
    count_pieces_row(Head, RowBlack, RowWhite),
    count_pieces(Tail, RestBlack, RestWhite),
    Black is RowBlack + RestBlack,
    White is RowWhite + RestWhite.
