% Input
input([
    [blue, blue,  red, red , yellow],
    [blue, yellow,  red , red , yellow],
    [blue, yellow, yellow , yellow]
]).


% Moves
move(X,Y , X1 , Y1) :- X1 is X + 1 , Y1 is Y , valid(X1,Y1).
move(X,Y , X1 , Y1) :- X1 is X - 1 , Y1 is Y , valid(X1,Y1).
move(X,Y , X1 , Y1) :- X1 is X , Y1 is Y + 1 , valid(X1,Y1).
move(X,Y , X1 , Y1) :- X1 is X , Y1 is Y - 1 , valid(X1,Y1).

% Check if cell is valid heigh:N width:M
valid(X,Y) :- input(Board), length(Board, N),nth1(1, Board, FirstRow),length(FirstRow , M), X > 0, Y > 0, X =< N, Y =< M.

color_of_cell(X,Y, Color) :- 
    input(Board), 
    nth1(X, Board, Row), 
    nth1(Y, Row, Color).

is_cycle(Color , Path , Visited , X,Y):-
    move(X,Y, X1,Y1),
    color_of_cell(X1,Y1, Color),
    member([X1,Y1], Visited),
    append(Path , [[X1,Y1]] , NewPath),
    Path = NewPath.

is_cycle(Color , Path , Visited , X,Y):-
    move(X,Y, X1,Y1),
    color_of_cell(X1,Y1, Color),
    append(Visited , [[X1,Y1]] , NewVisited),
    append(Path , [[X1,Y1]] , NewPath),
    is_cycle(Color , NewPath , NewVisited , X1,Y1).


% print all cycles in the board
print_path(X, Y):-
    color_of_cell(Start, Color),
    is_cycle(Color , [[X,Y]] , [[X,Y]] , X,Y),
    write('Cycle: '),
    write([Start]),
    write(' -> '),
    fail.

   




