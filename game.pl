/* Prosta gra w Prologu: Przemieszczanie się po pomieszczeniu 5x5 */

/* Deklaracje dynamiczne i inicjalizacja */
:- dynamic position/2.
:- retractall(position(_, _)).

/* Pozycja startowa */
position(3, 3).

/* Rozmiar pomieszczenia */
room_size(5, 5).

/* Zasady dotyczące poruszania się */
up :- move(0, -1).
down :- move(0, 1).
left :- move(-1, 0).
right :- move(1, 0).

/* Logika ruchu gracza */
move(Dx, Dy) :-
        position(X, Y),
        NewX is X + Dx,
        NewY is Y + Dy,
        room_size(MaxX, MaxY),
        within_bounds(NewX, NewY, MaxX, MaxY),
        retract(position(X, Y)),
        assert(position(NewX, NewY)),
        look,
	!, nl.

move(_, _) :-
        write('You hit the wall!'),
	nl.

/* Sprawdzanie granic pomieszczenia */
within_bounds(X, Y, MaxX, MaxY) :-
        X > 0, X =< MaxX,
        Y > 0, Y =< MaxY.

/* Opisanie pozycji */
look :-
        position(X, Y),
        write('Your current location is ('), write(X), write(','), write(Y), write(').'), nl.

/* Wyświetlanie rozmiaru pomieszczenia */
give_room_info :-
        room_size(MaxX, MaxY),
        write('You are in a room of size '), write(MaxX), write('x'), write(MaxY), write('.'), nl,
	write('There are illegal immigrants here'), nl.


/* Instrukcje gry */
instructions :-
        nl,
        write('Use the following commands to move:'), nl,
        write('up.       -- move up'), nl,
        write('down.     -- move down'), nl,
        write('left.     -- move left'), nl,
        write('right.    -- move right'), nl,
        write('look.     -- show current position'), nl,
        write('instructions. -- show this message again'), nl,
        write('halt.     -- exit the game'), nl,
        nl.

/* Rozpoczęcie gry */
start :-
        instructions,
	give_room_info,
        look.

