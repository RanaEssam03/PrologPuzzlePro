% Input
input([
    [red, red, blue , blue],
    [red, red, yellow , yellow],
    [blue, yellow, yellow , yellow]
]).


% Moves
move([X,Y], [X1,Y]) :- X1 is X + 1, valid(X1,Y).
move([X,Y], [X,Y1]) :- Y1 is Y + 1, valid(X,Y1).
move([X,Y], [X1,Y]) :- X1 is X - 1, valid(X1,Y).
move([X,Y], [X,Y1]) :- Y1 is Y - 1, valid(X,Y1).

% Check if cell is valid heigh:N width:M
valid(X,Y) :- input(Board), length(Board, N),nth1(1, Board, FirstRow),length(FirstRow , M), X > 0, Y > 0, X =< N, Y =< M.

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
    color_of_cell([X,Y], Color),  % Check if cell is of the right color
    \+ member([X,Y], Visited),   % Check if cell has not been visited
    append([[X,Y]], Visited, NewVisited), % Add cell to visited list
    move([X,Y], [X1,Y1]), % Move to next cell
    color_cycle([X1,Y1], Color, NewVisited, NewPath), % Recursively check next cell
    append([[X,Y]], NewPath, Path). % Add cell to path

color_cycle([X,Y], Color, Visited, [[X,Y]]) :-
    color_of_cell([X,Y], Color),  % Check if cell is of the right color
    \+ member([X,Y], Visited).  % Check if cell has not been visited

% Check if cycle exists
cycle_exists(Color, Path) :-
    color_cycle([1,1], Color, [], Path),
    length(Path, Len),
    Len >= 4.

% Search for color cycles
search_color_cycles(Color) :- % If cycle exists, print it and stop
    cycle_exists(Color, Path), 
    write('Found a '), write(Color), write(' cycle: '), write(Path), nl, !.

search_color_cycles(_). % Do nothing if no cycle exists

% Main predicate to find all color cycles
find_color_cycles :-
    color(Color),
    search_color_cycles(Color),
    fail. % Fail to backtrack and find all cycles

find_color_cycles :-
    write('No cycles exist.'), nl.