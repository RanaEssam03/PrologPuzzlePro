% puzzle problem

% Implement the A* algorithm to solve the puzzle problem
% The puzzle problem is to move the blank tile to the goal state
% given start and goal , get the possible paths

%% input design list of cells
cell(X, Y, Color).  %% X: row number; Y: column number; Color: color of the cell
% 



% N is the number of rows and M is the number of columns
% Start is the start state of the board
% Board is the board with the colors of the cells
% Path is the path from the start state to the goal state
% The path is a list of cells

start_game(N, M, Start, Board, Path):-
    search([[Start, [], 0, 0, 0]], [], Board, Path).





    


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
    move(State, Next, MoveCost),
    calculateH(Next, Goal, NewH),
    NewG is G + MoveCost,
    NewF is NewG + NewH,
    not(member([Next,_,_,_,_], Open)),
    not(member([Next,_,_,_,_], Closed)).

% Implementation of addChildren and getBestState
addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

% Implementation of the algorithm A*

findMin([X], X):- !.
findMin([Head|T], Min):-
    findMin(T, TmpMin),
    Head = [_,_,_,HeadH,HeadF],
    TmpMin = [_,_,_,TmpH,TmpF],
    (TmpF < HeadF -> Min = TmpMin ; Min = Head).


getNextState([State,_,G,_,_],Open,Closed,Goal,[Next,State,NewG,NewH,NewF]):-
    move(State, Next, MoveCost),
    calculateH(Next, Goal, NewH),
    NewG is G + MoveCost,
    NewF is NewG + NewH,
    ( not(member([Next,_,_,_,_], Open)) ; memberButBetter(Next,Open,NewF) ),
    ( not(member([Next,_,_,_,_],Closed));memberButBetter(Next,Closed,NewF)).



memberButBetter(Next, List, NewF):-
    findall(F, member([Next,_,_,_,F], List), Numbers),
    min_list(Numbers, MinOldF),
    MinOldF > NewF.




calculateH(State, State, 0); !.
calculateH(State, Goal, H):-
    State = cell(X1, Y1, _),
    Goal = cell(X2, Y2, _),
    H is abs(X1 - X2) + abs(Y1 - Y2).



% get the next moves of the current state
move(cell(X1, Y1, Color), cell(X2, Y2, Color), N, M):-
    X2 is X1 + 1, Y2 is Y1,
    isValid(cell(X2, Y2, _), N, M).

move(cell(X1, Y1, Color), cell(X2, Y2, Color), N, M):-
    X2 is X1 - 1, Y2 is Y1,
    isValid(cell(X2, Y2, _), N, M).

move(cell(X1, Y1, Color), cell(X2, Y2, Color), N, M):-
    X2 is X1, Y2 is Y1 + 1,
    isValid(cell(X2, Y2, _), N, M).

move(cell(X1, Y1, Color), cell(X2, Y2, Color), N, M):-
    X2 is X1, Y2 is Y1 - 1,
    isValid(cell(X2, Y2, _), N, M).

isValid(Cell, _, _) :- \

isValid(cell(X, Y, _), N, M):-
    X >= 0, X < N,
    Y >= 0, Y < M.






