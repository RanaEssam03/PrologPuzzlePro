:- dynamic board/1.
:- dynamic algorithm/1.

% Input
input :-
    retractall(board(_)),
    retractall(algorithm(_)),
    write("Please enter your board"), nl,
    read(Value),nl,
    write("BFS OR DFS"),nl,
    read(Algo),
    assert(board(Value)),
    assert(algorithm(Algo)).


select_algorithm(Color, Algorithm) :-
    Algorithm == "bfs" ->
    (bfs_search_color_cycles(Color));
    (dfs_search_color_cycles(Color)).

% Moves
move([X,Y], [X1,Y]) :- X1 is X + 1, valid(X1,Y).
move([X,Y], [X,Y1]) :- Y1 is Y + 1, valid(X,Y1).
move([X,Y], [X1,Y]) :- X1 is X - 1, valid(X1,Y).
move([X,Y], [X,Y1]) :- Y1 is Y - 1, valid(X,Y1).

% Check if cell is valid height:N width:M
valid(X,Y) :-
    board(Board),
    length(Board, N),
    nth1(1, Board, FirstRow),
    length(FirstRow, M),
    X > 0, Y > 0, X =< N, Y =< M.

last(X, [X]) :- !.
last(X, [_|T]) :- last(X, T).

first(X, [X|_]) :- !.

% Define colors
color(red).
color(yellow).
color(blue).

% Define color of a cell
color_of_cell([X,Y], Color) :-
    board(Board),
    nth1(X, Board, Row),
    nth1(Y, Row, Color).


insert_between(_, [X], [X]).

insert_between(Element, [X|Xs], [X, Element|Rest]) :-
    insert_between(Element, Xs, Rest).


% ------------------------------------------------------------------------



% BFS for cycle of color
bfs_search_cycle(Color, Path) :-
    findall([X,Y], (board(Board), nth1(X, Board, Row), nth1(Y, Row, Color)), StartPositions),
    member(StartPos, StartPositions),
    bfs_cycle(Color, [[StartPos]], [], Path).

bfs_cycle(_, [], _, []) :- !.

bfs_cycle(_, [[Pos|RestPath]|_], _,Path) :-
    length([Pos|RestPath], N),
    N >= 4,
    last([X1,Y1], [Pos|RestPath]),
    move([X1,Y1], _),
    reverse([Pos|RestPath], Path).

bfs_cycle(Color, [[Pos|RestPath]|Queue], Visited, Path) :-
    \+ member(Pos, Visited),
    append(Visited, [Pos], NewVisited),
    findall([NewPos| [Pos|RestPath]], (move(Pos, NewPos),
    color_of_cell(NewPos, Color),
    \+ member(NewPos, [Pos|RestPath])), NewPaths),
    append(Queue, NewPaths, NewQueue),
    bfs_cycle(Color, NewQueue, NewVisited, Path).

bfs_cycle(Color, [_|Queue], Visited, Path) :-
    bfs_cycle(Color, Queue, Visited, Path).



% ------------------------------------------------------------------------

% DFS for cycle of color
dfs_color_cycle([X,Y], Color, Visited, Path) :-
    color_of_cell([X,Y], Color),
    \+ member([X,Y], Visited),
    append([[X,Y]], Visited, NewVisited),
    move([X,Y], [X1,Y1]),
    dfs_color_cycle([X1,Y1], Color, NewVisited, NewPath),
    append([[X,Y]], NewPath, Path).

dfs_color_cycle([X,Y], Color, Visited, [[X,Y]]) :-
    color_of_cell([X,Y], Color),
    \+ member([X,Y], Visited).

% Check if cycle exists

cycle_exists(Color, Path):-
    dfs_color_cycle([X,Y] , Color,[],Path),
    length(Path, N), N >= 4,last([X1,Y1], Path),move([X,Y] , [X1,Y1]).


% Search for color cycles
bfs_search_color_cycles(Color) :-
    bfs_search_cycle(Color, Path) ,
    first([X,Y], Path),
    append(Path, [X,Y], Path1),
    write('Found a '),
    write(Color),
    write(' cycle: '),
    insert_between("=>", Path1,FinalPath),
    write(FinalPath), nl, !.

bfs_search_color_cycles(_).

dfs_search_color_cycles(Color) :-
    cycle_exists(Color, Path),
    first([X,Y], Path),
    append(Path, [X,Y], Path1),
    write('Found a '),
    write(Color),
    write(' cycle: '),
    insert_between("=>", Path1,FinalPath),
    write(FinalPath), nl, !.

dfs_search_color_cycles(_).



% ------------------------------------------------------------------------

% Main predicate to find all color cycles
find_color_cycles :-
    input(),
    color(Color),
    algorithm(Algo),
    select_algorithm(Color,Algo),
    fail.

find_color_cycles :-
    write('No cycles exist.'), nl.
