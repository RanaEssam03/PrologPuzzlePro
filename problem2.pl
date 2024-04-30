% puzzle problem

% Implement the A* algorithm to solve the puzzle problem
% The puzzle problem is to move the blank tile to the goal state
% given start and goal , get the possible paths

:- dynamic cell/3.




%% input design list of cells
%cell(_, _, _).  %% X: row number; Y: column number; Color: color of the cell
% 

% Add a new cell
add_cell(X,Y,Color) :-
    assert(cell(X, Y, Color)).

% Delete a cell
delete_cell(X,Y,Color) :-
    retract(cell(X, Y, Color)).



% N is the number of rows and M is the number of columns
% Start is the start state of the board
% Board is the board with the colors of the cells
% Path is the path from the start state to the goal state
% The path is a list of cells

generate_cells(_,_,[]).


generate_cells(Current,M,[H|T]):-
    X is Current div M + 1,
    Y is Current mod M + 1,
    add_cell(X,Y,H),
    Next is Current + 1,
    generate_cells(Next,M,T).


delete_cells(_,_,[]).


delete_cells(Current,M,[H|T]):-
    X is Current div M + 1,
    Y is Current mod M + 1,
    delete_cell(X,Y,H),
    Next is Current + 1,
    delete_cells(Next,M,T).
     




start_game(_, M, Start, Goal,Board):-
    generate_cells(0,M,Board),
    search([[Start, null, 0, 0, 0]], [],Goal),
    delete_cells(0,M,Board).




search(Open, Closed, Goal):-
    getBestState(Open, [CurrentState,Parent,G,H,F], _), % Step 1
    CurrentState = Goal, % Step 2
    write("Search is complete!"), nl,
    printSolution([CurrentState,Parent,G,H,F], Closed), !.

search(Open, Closed, Goal):-
    getBestState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode,TmpOpen,Closed,Goal,Children), % Step 3
    addChildren(Children, TmpOpen, NewOpen), % Step 4
    append(Closed, [CurrentNode], NewClosed), % Step 5.1
    search(NewOpen, NewClosed, Goal). % Step 5.2


% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Goal, Children):-
    findall(Next, getNextState(Node,Open,Closed,Goal,Next),
    Children).

getNextState([State,_,G,_,_],Open,Closed,Goal,[Next,State,NewG,NewH,NewF]):-
    move(State, Next),
    calculateH(Next, Goal, NewH),
    NewG is G + 1,
    NewF is NewG + NewH,
    not(member([Next,_,_,_,_], Open)),
    not(member([Next,_,_,_,_], Closed)).


% Implementation of addChildren and getBestState
addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).



printSolution([cell(X,Y,_), null, 0, 0, 0],_):-
    write([X,Y]),!.

printSolution([cell(X,Y,_), Parent, _, _, _], Closed):-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write(" -> "),write([X,Y]).

% Implementation of the algorithm A*

findMin([X], X):- !.
findMin([Head|T], Min):-
    findMin(T, TmpMin),
    Head = [_,_,_,_,HeadF],
    TmpMin = [_,_,_,_,TmpF],
    (TmpF < HeadF -> Min = TmpMin ; Min = Head).




calculateH(State, State, 0):- !.

calculateH(State, Goal, H):-
    State = cell(X1, Y1, _),
    Goal = cell(X2, Y2, _),
    H is abs(X1 - X2) + abs(Y1 - Y2).



% get the next moves of the current state

% Move right
move(cell(X1, Y1, Color), cell(NewX,NewY,Color)):-
    X2 is X1 + 1, Y2 is Y1,
    cell(X2,Y2,Color),
    NewX is X2,NewY is Y2.

% Move left
move(cell(X1, Y1, Color), cell(NewX,NewY,Color)):-
    X2 is X1 - 1, Y2 is Y1,
    cell(X2,Y2,Color),
    NewX is X2,NewY is Y2.


% Move down
move(cell(X1, Y1, Color), cell(NewX,NewY,Color)):-
    X2 is X1, Y2 is Y1 + 1,
    cell(X2,Y2,Color),
    NewX is X2,NewY is Y2.

% Move up
move(cell(X1, Y1, Color), cell(NewX,NewY,Color)):-
    X2 is X1, Y2 is Y1 - 1,
    cell(X2,Y2,Color),
    NewX is X2,NewY is Y2.



