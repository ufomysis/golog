Golog is a toy implementation of Go in Prolog. 

There are many variations of the rules for Go. This implementation adopts the [Tromp-Taylor rules](http://tromp.github.io/go.html). These are sometimes referred to as the 'Logical Rules of Go' and are written in a mostly declarative manner. This makes them an excellent target for a Prolog implementation of Go.

Golog currently supports:
* Playing on a 5x5 board. This can be changed by setting a constant in go.pl. 5x5 was chosen to demonstrate functionality without being too large.
* Alternating turns between Black and White
* Playing stones on points
* Capturing surrounded groups
* Scoring for each player 
* Positional super-ko
* Displaying the board state

Requirements
------------
Golog was written for SWI-prolog. With Homebrew installed, SWI-Prolog can be installed like
```
brew tap homebrew/x11
brew install swi-prolog
```

Running
-------
The `main` rule will randomly play stones until it breaks the super-ko rule:
```
$ swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.2.3)
Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- [go].
true.

?- main.

-----
-----
----#
-----
-----

Black's Score: 25
White's Score: 0
White to play
===============

-O---
-----
----#
-----
-----

Black's Score: 24
White's Score: 23
Black to play
===============
.
.
.
```
