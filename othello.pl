:- module(othello, [row/3, column/3, square/4, empty_square/3, empty_board/1, enclosing_piece/7]).
:- use_module([game_logic, board, io, fill, heuristics, winner]).

play :- 
    welcome,
    initial_board(Board),
    display_board(Board),
    is_black(Black),
    play(Black, Board).
play.

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
    heuristics:choose_move(Player, X, Y, Board_state),
    fill_and_flip_squares( X, Y, Player, Board_state, NewBoard),
    report_move(Player, X, Y),
    display_board(NewBoard),
    is_black(OtherPlayer),
    play(OtherPlayer, NewBoard).