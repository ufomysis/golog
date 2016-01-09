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
  X >= 0,
  X < N,
  Y >= 0,
  Y < N.

% We'll play on a 9x9 board
size(9).

% We'll use directions to define adjacencies of points
direction(up).
direction(down).
direction(left).
direction(right).

neighbor([X, Y], [X1, Y1], up) :-
  X1 = X,
  Y1 = Y+1,
  point([X1, Y1]).
neighbor([X, Y], [X1, Y1], down) :-
  X1 = X,
  Y1 = Y-1,
  point([X1, Y1]).
neighbor([X, Y], [X1, Y1], left) :-
  X1 = X-1,
  Y1 = Y,
  point([X1, Y1]).
neighbor([X, Y], [X1, Y1], right) :-
  X1 = X+1,
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

%alive([X,Y], Board) :-


game(Board, ToPlay) :-
  after(Played, ToPlay),
  move(Board, ToPlay, UpdatedBoard),
  !,
  game(UpdatedBoard, Played).

showBoard([AA,AB,AC,BA,BB,BC,CA,CB,CC], Player) :-
  write([AA,AB,AC]),nl,
  write([BA,BB,BC]),nl,
  write([CA,CB,CC]),nl,
  colorString(Player, String),
  write(String),nl. 
