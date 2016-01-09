% Prolog implementation of Go (Tromp-Taylor rules)


% There are two players, black and white
player(black).
player(white).

% They alternate turns
after(black,white).
after(white,black).

% A square board.
point([X, Y]) :-
  size(N),
  Limit is N-1,
  between(0,Limit,X),
  between(0,Limit,Y).

points(L) :- findall(Point, point(Point), L).
coloredPoints(Color,Board,L) :- findall(Point, pointColor(Board, Point, Color), L).

% We'll play on a 9x9 board
size(2).

% We'll use directions to define adjacencies of points
direction(up).
direction(down).
direction(left).
direction(right).

neighbor([X, Y], [X1, Y1], up) :-
  X1 = X,
  Y1 = Y + 1,
  point([X1, Y1]).
neighbor([X, Y], [X1, Y1], down) :-
  X1 = X,
  Y1 = Y - 1,
  point([X1, Y1]).
neighbor([X, Y], [X1, Y1], left) :-
  X1 = X - 1,
  Y1 = Y,
  point([X1, Y1]).
neighbor([X, Y], [X1, Y1], right) :-
  X1 = X + 1,
  Y1 = Y,
  point([X1, Y1]).
   
% Points can be empty or occupied by a player.
color(empty).
color(_) :-
  player(_).

%Utilities to relate colors to strings for display
colorString(empty, "empty").
colorString(black, "black").
colorString(white, "white").

colorChar(empty, "-").
colorChar(black, "#").
colorChar(white, "O").

%TODO FIX
completeColors() :- forall(color(X), colorString(X,_)).

completeColors().

% A point is alive if it is next to an empty point, or is connected to a live point.

pointColor(Board, [X,Y], Color) :-
  point([X,Y]),
  nth0(X, Board, Row),
  nth0(Y, Row, Color). 

reachesEmpty(Point, Board) :- 
  write(Point),
  pointColor(Board, Point, empty).
reachesEmpty(Point, Board) :-
  neighbor(Point, Neighbor, _),
  reachesEmpty(Neighbor, Board).

% This is inefficient.
legalBoard(Board) :-
  points(L),
  forall(member(Elem, L), reachesEmpty(Elem, Board)).

% After placing a stone, all the other points are the same
sameColor(Board, Point, NewBoard) :-
  pointColor(Board, Point, Color),
  pointColor(NewBoard, Point, Color).

mask(Board, Point, NewBoard) :-
  points(L),
  delete(L, Point, L1),
  forall(member(Elem, L1), sameColor(Board, Elem, NewBoard)).

% A stone must either reach empty, or be removed in the new board
notSurrounded(Board, _, Elem) :-
  reachesEmpty(Elem, Board).

notSurrounded(Board, NewBoard, Elem) :-
  \+ reachesEmpty(Elem, Board),
  pointColor(NewBoard, Elem, empty).

% True if all points in Board for Color that were not reachable are removed in NewBoard
isConsistent(Board, Color, NewBoard) :-
  coloredPoints(Color, Board, L),
  forall(member(Elem, L), notSurrounded(Board, NewBoard, Elem)).
  
evolve(Board, Point, Player, NewBoard) :-
  mask(Board, Point, IntermediateBoard),
  pointColor(IntermediateBoard, Point, Player),
  after(Player, Opponent),
  isConsistent(IntermediateBoard, Opponent, OpponentBoard),
  isConsistent(OpponentBoard, Player, NewBoard).
  
replicate(N,X,Xs) :-
    length(Xs,N),
    maplist(=(X),Xs).

newBoard(Board) :-
  size(N),
  replicate(N, empty, Row),
  replicate(N, Row, Board).

showRow([]).
showRow([H|T]) :-
  color(H),
  colorChar(H, C),
  write(C),
  showRow(T).

showBoard([]).
showBoard([H|T]) :-
 showRow(H), !, nl, showBoard(T).

showState(Board, Player) :-
  showBoard(Board),
  colorString(Player, String),
  write(String), nl.
