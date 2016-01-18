% Prolog implementation of Go (Tromp-Taylor rules)


% There are two players, black and white
player(black).
player(white).

% They alternate turns
after(black,white).
after(white,black).

% They play on a square board.
point([X, Y]) :-
  size(N),
  Limit is N-1,
  between(0,Limit,X),
  between(0,Limit,Y).

points(L) :- findall(Point, point(Point), L).
coloredPoints(Color,Board,L) :- findall(Point, pointColor(Board, Point, Color), L).

% We'll play on a 9x9 board
size(9).

% We'll use directions to define adjacencies of points
direction(up).
direction(down).
direction(left).
direction(right).

neighbor([X, Y], [X1, Y1], up) :-
  X1 is X,
  Y1 is Y + 1,
  point([X1, Y1]).
neighbor([X, Y], [X1, Y1], down) :-
  X1 is X,
  Y1 is Y - 1,
  point([X1, Y1]).
neighbor([X, Y], [X1, Y1], left) :-
  X1 is X - 1,
  Y1 is Y,
  point([X1, Y1]).
neighbor([X, Y], [X1, Y1], right) :-
  X1 is X + 1,
  Y1 is Y,
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

% A point is alive if it is next to an empty point, or is connected to a live point.
pointColor(Board, [X,Y], Color) :-
  point([X,Y]),
  nth0(X, Board, Row),
  nth0(Y, Row, Color). 

reaches(Point, Board, Color) :-
  reaches(Point, Board, []), !.

reaches(Point, Board, Color, _) :- 
  pointColor(Board, Point, empty).

reaches(Point, Board, Color, _) :-
  neighbor(Point, Neighbor, _),
  pointColor(Board, Neighbor, empty).

reaches(Point, Board, Color, Visited) :-
  neighbor(Point, Neighbor, _),
  pointColor(Board, Point, Color),
  pointColor(Board, Neighbor, Color),
  \+ member(Neighbor, Visited),
  append(Visited, [Neighbor], NewVisited),
  !,
  reaches(Neighbor, Board, Color, NewVisited).

% After placing a stone, all the other points are the same
sameColor(Board, Point, NewBoard) :-
  pointColor(Board, Point, Color),
  pointColor(NewBoard, Point, Color).

stonePlaced(Board, [X, Y], Color, NewBoard) :-
  pointColor(Board, [X, Y], empty), % Intersections must be empty to be played on
  nth0(X, Board, Row),
  nth0(Y, Row, _, RemovedRow),
  nth0(Y, NewRow, Color, RemovedRow),
  nth0(X, Board, _, RemovedBoard),
  nth0(X, NewBoard, NewRow, RemovedBoard).

pointConsistent(Board, NewBoard, Point) :- 
  reaches(Point, Board, empty),
  pointColor(Board, Point, Color),
  pointColor(NewBoard, Point, Color),
  !.

pointConsistent(Board, NewBoard, Point) :-
  \+ reaches(Point, Board, empty),
  pointColor(NewBoard, Point, empty).

pointColorConsistent(Board, NewBoard, Color, Point) :-
  pointColor(Board, Point, OriginalColor),
  not(OriginalColor = Color),
  pointColor(NewBoard, Point, OriginalColor),
  !.

pointColorConsistent(Board, NewBoard, Color, Point) :-
  pointColor(Board, Point, Color),
  pointConsistent(Board, NewBoard, Point).

% True if all points in Board for Color that do not reach empty are removed in NewBoard
isConsistent(Board, Color, NewBoard) :-
  points(L),
  maplist(pointColorConsistent(Board, NewBoard, Color), L). % pointColorConsistent(Board, NewBoard) is a Curried Predicate
  
sameLength(L1, L2) :-
  length(L1, X),
  length(L2, X).

sameShape(A1, A2) :-
  sameLength(A1,A2),
  maplist(sameLength, A1, A2).

evolve(Board, Point, Player, NewBoard) :-
  stonePlaced(Board, Point, Player, IntermediateBoard),
  after(Player, Opponent),
  isConsistent(IntermediateBoard, Opponent, OpponentBoard),
  isConsistent(OpponentBoard, Player, NewBoard),
  sameShape(Board,NewBoard).
  
replicate(N,X,Xs) :-
  length(Xs,N),
  maplist(=(X),Xs).

newBoard(Board) :-
  size(N),
  replicate(N, empty, Row),
  replicate(N, Row, Board).

showRow([]) :-
  nl.
showRow([H|T]) :-
  color(H),
  colorChar(H, C),
  write(C),
  showRow(T).

showBoard([]) :- nl.
showBoard([H|T]) :-
 showRow(H), showBoard(T).

showState(Board, Player) :-
  showBoard(Board),
  colorString(Player, String),
  write(String), nl.

main() :-
 newBoard(Board),
 main([Board], black).

main([H|T], Player) :-
  coloredPoints(empty, H, L),
  random_member(Point, L),    
  evolve(H, Point, Player, NewBoard),
  !,
  showBoard(NewBoard),
  after(Player, Other),
  main([NewBoard, H | T], Other).

