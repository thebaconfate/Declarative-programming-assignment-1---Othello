:- module(winner, [count_pieces/3]).
:- use_module([board]).


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
