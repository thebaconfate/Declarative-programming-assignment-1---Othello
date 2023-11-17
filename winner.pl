:- module(winner, [count_pieces/3, and_the_winner_is/2]).
:- use_module([board, io]).

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