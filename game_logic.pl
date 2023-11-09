:- use_module([library(lists), io, fill, board, winner]).
:- module(game_logic, []).


play :- 
    welcome, 
    initial_board(Board),
    display_board(Board),
    is_black(Black).
    % TODO implement the predicate below:
    % play(Black, Board).




