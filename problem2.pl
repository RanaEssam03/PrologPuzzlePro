% puzzle problem 2
:- dynamic cell/3.

% this is the rule which represents the cell in the board
%cell(X, Y, Color).  %% X: row number; Y: column number; Color: color of the cell


% this is the main predicate to start the game which takes the N and M as the number of rows and columns
% Start is the start state of the board and Goal is the goal state of the board

% EX: start_game(2,2,cell(1,1,'R'),cell(2,2,'R'), ['R', 'G', 'R', 'R'] ).
start_game(_, M, Start, Goal,Board):-
    generate_cells(0,M,Board),
    search([[Start, null, 0, 0, 0]], [],Goal),
    delete_cells(0,M,Board).


%_______________________________________________________________________________________________________________________

% Add a new cell
add_cell(X,Y,Color) :-
    assert(cell(X, Y, Color)).

% Delete a cell
delete_cell(X,Y,Color) :-
    retract(cell(X, Y, Color)).

%____________________________________________________________________________________________________________


generate_cells(_,_,[]):- !.
% Generate the cells of the board with the colors of the cells in as a facts in the knowledge base 
generate_cells(Current,M,[H|T]):-
    X is Current div M + 1,  % get the row number from the index of the cell in the list
    Y is Current mod M + 1,  % get the column number from the index of the cell in the list
    add_cell(X,Y,H),
    Next is Current + 1,
    generate_cells(Next,M,T).

%__________________________________________________________________________________________________________________

delete_cells(_,_,[]):- !.
% Delete the cells of the board with the colors of the cells in as a facts in the knowledge base to clean the board after the game
delete_cells(Current,M,[H|T]):-
    X is Current div M + 1, % get the row number from the index of the cell in the list
    Y is Current mod M + 1, % get the column number from the index of the cell in the list
    delete_cell(X,Y,H),
    Next is Current + 1,
    delete_cells(Next,M,T),!.
     

%_______________________________________________________________________________________________________________________

% Search for the solution of the game using the A* algorithm to find the shortest path to the goal state
search(Open, Closed, Goal):-
    getBestState(Open, [CurrentState,Parent,G,H,F], _), % Step 1
    CurrentState = Goal, % Step 2
    write("The correct path is: "), % print the correct path to the goal state after the search is done 
    printSolution([CurrentState,Parent,G,H,F], Closed),!.


search(Open, Closed, Goal):- 
    getBestState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode,TmpOpen,Closed,Goal,Children), % Step 3
    addChildren(Children, TmpOpen, NewOpen), % Step 4
    append(Closed, [CurrentNode], NewClosed), % Step 5.1
    search(NewOpen, NewClosed, Goal),!. % Step 5.2


% If no solution is found then print "No solution found!"
search(_, _, _):-
    write("No path exists!"), nl.

%_______________________________________________________________________________________________________________________

% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Goal, Children):-
    findall(Next, getNextState(Node,Open,Closed,Goal,Next),
    Children),!.

%__________________________________________________________________________________________________
getNextState([State,_,G,_,_],Open,Closed,Goal,[Next,State,NewG,NewH,NewF]):-
    move(State, Next),
    calculateH(Next, Goal, NewH),
    NewG is G + 1, % every move has a cost of 1 so the new G is the old G + 1 
    NewF is NewG + NewH, 
    not(member([Next,_,_,_,_], Open)), % check if the state is not in the open list
    not(member([Next,_,_,_,_], Closed)). % check if the state is not in the closed list

%_______________________________________________________________________________________________________________________
% Implementation of addChildren and getBestState
addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

%_______________________________________________________________________________________________________________________

getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).


%_______________________________________________________________________________________________________________________
printSolution([cell(X,Y,_), null, 0, 0, 0],_):- % print the first state of the solution
    write([X,Y]),!.

printSolution([cell(X,Y,_), Parent, _, _, _], Closed):- % print the rest of the solution
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write(" -> "),write([X,Y]), !.

%_______________________________________________________________________________________________________________________

% Implementation of the algorithm A*
findMin([X], X):- !.
findMin([Head|T], Min):-
    findMin(T, TmpMin),
    Head = [_,_,_,_,HeadF],
    TmpMin = [_,_,_,_,TmpF],
    (TmpF < HeadF -> Min = TmpMin ; Min = Head).


%_______________________________________________________________________________________________________________________

% if the state is the goal state then the heuristic is 0
calculateH(State, State, 0):- !.

% calculate the heuristic of the state s according to Manhattan distance heuristic 
calculateH(State, Goal, H):-
    State = cell(X1, Y1, _),
    Goal = cell(X2, Y2, _),
    H is abs(X1 - X2) + abs(Y1 - Y2).

%_______________________________________________________________________________________________________________________

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



