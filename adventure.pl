/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, has/1.
:- dynamic mission/2, mission_completed/1.

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(mission_completed(_)).

/* Player's starting location */
i_am_at(lobby).


/* Missions definition */
mission(drill, construction_site).
mission(car, penthouse).
mission(weapon, gang_hideout).

path(construction_site)


/* Rule of choosing a mission */
choose_mission(Thing) :-
        mission(Thing, Location),
        \+ mission_completed(Thing),  /* true if mission not completed */
        retract(i_am_at(lobby)),
        assert(i_am_at(Location)),
        write('You have chosen the mission to get the '), write(Thing), write('.'), nl,
        look.
    
choose_mission(_) :-
        write('Invalid mission or mission already completed.'), nl.
    

/* Reguła zakończenia misji */
complete_mission(Thing) :-
        mission(Thing, Location),
        /* i_am_at(Location), */
        has(Thing),
        assert(mission_completed(Mission)),
        write('You have completed the mission to get the '), write(Mission), write('.'), nl,
        return_to_lobby.
    
complete_mission(_) :-
        write('You are not at the correct location to complete this mission or did not found the correct object'), nl.
    

/* Reguła powrotu do lobby */
return_to_lobby :-
        retract(i_am_at(_)),
        assert(i_am_at(lobby)),
        write('You finished your mission and returned to the lobby.'), nl,
        write('Congrats. Now choose what to do next'), nl,
        look.


/* These rules describe how to pick up an object. */
take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */
drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */
n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('You can''t go that way.').


/* This rule tells how to look about you. */
look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */
notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


/* This rule tells how to die. */
die :-
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */
finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


/* This rule just writes out game instructions. */
instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This is description of game plot */
'game description' :-
        nl,
        write('You are about to rob the biggest bank of Los Santos.'), nl,
        write('You have to be well prepared.'), nl,
        write('You need a solid drill, fast car and a weapon.'), nl,
        write('You need to get them before robbery.'), nl,
        write('Choose first mission - getting drill, car or weapon.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */
start :-
        instructions,
        'game description',
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(lobby) :-
        write('You are in the lobby. You can choose a mission: drill, car, or weapon.'), nl.

describe(construction_site) :-
        write('You are at the construction site. Find the drill.'), nl.

describe(penthouse) :-
        write('You are at the penthouse. Find the car.'), nl.

describe(gang_hideout) :-
        write('You are at the gang hideout. Find the weapon.'), nl.

