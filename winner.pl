:- module(winner, [count_pieces/3, and_the_winner_is/2]).
:- use_module([board, io]).


count_pieces_row([], 0, 0).
count_pieces_row([Head | Tail], Black, White) :-
    is_black(Head), count_pieces_row(Tail, PrevBlack, White), Black is PrevBlack + 1;
    is_white(Head), count_pieces_row(Tail, Black, PrevWhite), White is PrevWhite + 1;
    is_empty(Head), count_pieces_row(Tail, Black, White).
count_pieces([], 0, 0).
count_pieces([Head | Tail], Black, White) :- 
    count_pieces_row(Head, RowBlack, RowWhite),
    count_pieces(Tail, RestBlack, RestWhite),
    Black is RowBlack + RestBlack,
    White is RowWhite + RestWhite.

decide_winner(Black, White, Winner) :- 
    Black > White, is_black(Winner), report_winner(Winner);
    Black < White, is_white(Winner), report_winner(Winner);
    Black = White, is_empty(Winner), 
    format( 'It\'s a draw black: ~w, white : ~w!\n\n', [Black, White]).

and_the_winner_is(Board, Winner) :- 
    count_pieces(Board, Black, White), 
    decide_winner(Black, White, Winner).
