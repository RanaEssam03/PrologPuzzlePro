search(Open, Closed, Goal):-
    getState(Open, [CurrentState,Parent], _), % Step 1
    CurrentState = Goal, !, % Step 2
    write("Search is complete!"), nl,
    printSolution([CurrentState,Parent], Closed).


search(Open, Closed, Goal):-
    getState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode,TmpOpen,Closed,Children), % Step3
    addChildren(Children, TmpOpen, NewOpen), % Step 4
    append(Closed, [CurrentNode], NewClosed), % Step 5.1
    search(NewOpen, NewClosed, Goal). % Step 5.2

% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Children):-
    findall(Next, getNextState(Node, Open, Closed, Next), Children).

getNextState([State,_], Open, Closed, [Next,State]):-
    move(State, Next),
    not(member([Next,_], Open)),
    not(member([Next,_], Closed)).
% Implementation of getState and addChildren determine the search alg.
% BFS

getState([CurrentNode|Rest], CurrentNode, Rest).
addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

% Implementation of printSolution to print the actual solution path
printSolution([State, null],_):-
    write(State), nl.


printSolution([State, Parent], Closed):-
    member([Parent, GrandParent], Closed),
    printSolution([Parent, GrandParent], Closed),
    write(State), nl.

move(State, Next):-
%% here we implement the logic of the problem 