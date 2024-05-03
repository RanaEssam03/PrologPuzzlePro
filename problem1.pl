:-dynamic board/1.
:-dynamic check/1.
%[[yellow,yellow,yellow,red],[blue,yellow,blue,yellow],[blue,blue,blue,yellow],[blue,blue,blue,yellow]].
%[[red,red,red],[red,red,red],[blue,yellow,yellow],[yellow,yellow,yellow]].

%Input

input:-
    retractall(board(_)),
    write("please enter your board"),nl,
    read(Value),
    assert(board(Value)).

getBoard(Value):-
    board(Value).
% Moves
move([X,Y], [X1,Y]) :- X1 is X + 1, valid(X1,Y).
move([X,Y], [X,Y1]) :- Y1 is Y + 1, valid(X,Y1).
move([X,Y], [X1,Y]) :- X1 is X - 1, valid(X1,Y).
move([X,Y], [X,Y1]) :- Y1 is Y - 1, valid(X,Y1).

% Check if cell is valid heigh:N width:M
valid(X,Y) :- getBoard(Board),length(Board, N),nth1(1, Board, FirstRow), length(FirstRow, M), X > 0, Y > 0, X =< N, Y =< M.

last(X, [X]) :- !.
last(X, [_|T]) :- last(X, T).



first(X, [X|_]) :- !.



% Define colors
color(red).
color(yellow).
color(blue).

% Define color of a cell
color_of_cell([X,Y], Color) :-
   getBoard(Board),
    nth1(X, Board, Row),
    nth1(Y, Row, Color).

% Define cycle of color
color_cycle([X,Y], Color, Visited, Path) :-
    color_of_cell([X,Y], Color),
    \+ member([X,Y], Visited),
    append([[X,Y]], Visited, NewVisited),
    move([X,Y], [X1,Y1]),
    color_cycle([X1,Y1], Color, NewVisited, NewPath),
    append([[X,Y]], NewPath, Path).

color_cycle([X,Y], Color, Visited, [[X,Y]]) :-
    color_of_cell([X,Y], Color),
    \+ member([X,Y], Visited).

% Check if cycle exists

cycle_exists(Color, Path):-
    color_cycle([X,Y] , Color,[],Path),
    length(Path, N), N >= 4,last([X1,Y1], Path),move([X,Y] , [X1,Y1]).

% Search for color cycles
search_color_cycles(Color) :-
    cycle_exists(Color, Path),
    first([X,Y], Path),
    append(Path, [[X,Y]], Path1),
    retractall(check(_)),
    assertz(check(true)),
    output(Color,Path1),!.

search_color_cycles(_).

print_path([]).
print_path([H]):-
    write(H).
print_path([H|T]):-
    write(H),
    write('=>'),
    print_path(T).

output(Color,Path):-
    write('Found a '),
    write(Color),
    write(' cycle: '),
    print_path(Path), nl.
find_color_cycles :-
    input(),
    color(Color),
    search_color_cycles(Color),
    fail.
find_color_cycles.

% Main predicate to find all color cycles

find_cycle:-
    retractall(check(_)),
    assertz(check(false)),
    find_color_cycles,
        check(Bool),
        (Bool == false -> write('No cycles exist.'), nl ; true).






