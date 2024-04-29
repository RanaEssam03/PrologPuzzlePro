% Input
input([
    [red, red, blue],
    [red, red, yellow],
    [yellow, yellow, yellow],
    [yellow, yellow, yellow]
]).


% Moves
move([X,Y], [X1,Y]) :- X1 is X + 1, valid(X1,Y).
move([X,Y], [X,Y1]) :- Y1 is Y + 1, valid(X,Y1).
move([X,Y], [X1,Y]) :- X1 is X - 1, valid(X1,Y).
move([X,Y], [X,Y1]) :- Y1 is Y - 1, valid(X,Y1).

% Check if cell is valid heigh:N width:M
valid(X,Y) :- input(Board), length(Board, N),nth1(1, Board, FirstRow), length(FirstRow, M), X > 0, Y > 0, X =< N, Y =< M.

last(X, [X]) :- !.
last(X, [_|T]) :- last(X, T).
% Define colors
color(red).
color(yellow).
color(blue).

% Define color of a cell
color_of_cell([X,Y], Color) :- 
    input(Board), 
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
    write('Found a '), write(Color), write(' cycle: '), write(Path), nl, !.

search_color_cycles(_).

% Main predicate to find all color cycles
find_color_cycles :-
    color(Color),
    search_color_cycles(Color),
    fail.

find_color_cycles :-
    write('No cycles exist.'), nl.