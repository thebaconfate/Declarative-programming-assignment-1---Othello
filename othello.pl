:- use_module( [library(lists),
io,
fill] ).

player_1_character('*').
player_2_character('o').
blank_character(' ').


%some abstractions
between0_9(Number) :- 0 < Number, Number < 9.


% predicates to identify symbols
is_black(Symbol) :- player_1_character(Symbol).
is_white(Symbol) :- player_2_character(Symbol).
is_empty(Symbol) :- blank_character(Symbol).
is_piece(Symbol) :- is_black(Symbol); is_white(Symbol).
other_player(Player_1_symbol, Player_2_symbol) :- is_black(Player_1_symbol), is_white(Player_2_symbol); is_black(Player_2_symbol), is_white(Player_1_symbol).


% 
row(Row_number, Board_state, row(Row_number, A, B, C, D, E, F, G, H )) :- 
    between0_9(Row_number), 
    nth1(Row_number, Board_state, [A, B, C, D, E, F, G, H]).

get_col_value(Row_number, Column_number, Board_state, Variable) :-
    nth1(Row_number, Board_state, Row),
    nth1(Column_number, Row, Variable).

%extra abstraction for readability
get_i_j_value(I, J, Board_state, Variable) :-
    get_col_value(I, J, Board_state, Variable).

column(_ , _ , _ , []).
column(Row, Column_number, Board_state, [Head | Tail]) :-
    get_col_value(Row, Column_number, Board_state, Head),
    New_row is Row + 1,
    column(New_row, Column_number, Board_state, Tail).

column(Column_number, Board_state, col(Column_number, A, B, C, D, E, F, G, H)) :- 
    between0_9(Column_number),
    Starting_row = 1,
    column(Starting_row, Column_number, Board_state, [A, B, C, D, E, F, G, H]).

square(CoordX, CoordY, Board_state, squ(CoordX, CoordY, Piece)) :- 
    between0_9(CoordX), 
    between0_9(CoordY),
    get_i_j_value(CoordX, CoordY, Board_state, Piece).

empty_square(CoordX, CoordY, Board_state) :- 
    square(CoordX, CoordY, Board_state, squ(CoordX, CoordY, ' ')).

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

