/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, has/1, knows/3.
:- dynamic mission/2, mission_completed/1.

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(mission_completed(_)).

/* Player's starting location */
i_am_at(lobby).

/* Missions definition */
mission(drill, construction_site_south_gate).
mission(car, penthouse).
mission(weapon, gang_hideout).

path(construction_site_south_gate, n, construction_site).
path(construction_site, s, construction_site_south_gate).
path(construction_site, e, row_of_toolboxes).
path(row_of_toolboxes, w, construction_site).
path(construction_site, w, building_site).
path(building_site, e, construction_site).

path(row_of_toolboxes, e, toolbox1).
path(toolbox1, w, row_of_toolboxes).
path(toolbox1, e, toolbox2).
path(toolbox2, w, toolbox1).
path(toolbox2, e, toolbox3).
path(toolbox3, w, toolbox2).

at(supervisor, construction_site_south_gate).
at(containers, row_of_toolboxes).
at(workers, building_site).
at(building, building_site).

at(drill, toolbox2).

knows(supervisor, building, 'We are building new lifeinvader headquarters').
knows(supervisor, drill, 'You probably need that drill for heist. I am calling the cops').
knows(supervisor, toolbox, 'What is in those containers? Well, construction equipment. 
        If I remember correctly, there is a concrete mixer machine in the yellow one, 
        a concrete drill in the blue one and some steel beams in the white one. 
        I’m not sure about other ones though').
knows(workers, building, 'Doing great! Can’t wait to see this beauty finished! If u need sth, let us know').
knows(workers, drill, 'Yeah, there should be some old drills in one of our toolboxes on the construction site over there').


/* Rule of choosing a mission */
choose_mission(Thing) :-
        mission(Thing, Location),
        \+ mission_completed(Thing),  /* true if mission not completed */
        retract(i_am_at(lobby)),
        assert(i_am_at(Location)),
        write('You have chosen the mission to get the '), write(Thing), write('.'), nl,
        look,
        !, nl.

choose_mission(_) :-
        write('Invalid mission or mission already completed.'), nl.
    

/* Reguła zakończenia misji */
complete_mission(Thing) :-
        mission(Thing, _Location),
        has(Thing),
        assert(mission_completed(Thing)),
        % retract(mission(_, _)),
        write('You have completed the mission to get the '), write(Thing), write('.'), nl,
        return_to_lobby,
        !, nl.
    
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
        has(X),
        write('You''re already has it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(has(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */
drop(X) :-
        has(X),
        i_am_at(Place),
        retract(has(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


ask(Person, Thing) :-
        i_am_at(Place),
        at(Person, Place),      % those 2 conditions guarantees that someone can be asked only at place they are met
        knows(Person, Thing, Response),
        write(Person), write('says: '), write(Response),
        !, nl.

ask(_, _) :-
        write('They don\'t know anything about that.'), nl.


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
        write('start.             	-- to start the game.'), nl,
        write('n.  s.  e.  w.     	-- to go in that direction.'), nl,
	write('choose_mission(mission).	-- to start mission.'), nl,
        write('take(Object).      	-- to pick up an object.'), nl,
        write('drop(Object).      	-- to put down an object.'), nl,
        write('look.              	-- to look around you again.'), nl,
        write('instructions.      	-- to see this message again.'), nl,
        write('ask.              	-- to ask other characters about things.'), nl,
        write('halt.              	-- to end the game and quit.'), nl,

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

describe(construction_site_south_gate) :-
        write('You are in front of south gate of a construction site.'), nl,
        write('There is a supervisor next to you. You can talk with him.'), nl, 
        write('On the construction site at north there are workers in a building 
                and containers with various construction equipment. Find the drill.'), nl.

describe(construction_site) :-
        write('You entered construction site area.'), nl,
        write('There is a row of colorful toolboxes to your right site.'), nl,
        write('You can also take a closer look at construction workers, 
                as well as the uncompleted building on your left.'), nl.

describe(row_of_toolboxes) :-
        write('You are standing in front of 3 containers.'), nl.

describe(toolbox1) :-
        write('You are now standing in front of container 1.'), nl,
        write('There is wire knife here.'), nl.

describe(toolbox2) :-
        write('You are now standing in front of container 2.'), nl,
        write('There is drill here. You can have fun with it'), nl.

describe(toolbox3) :-
        write('You are now standing in front of container 3.'), nl,
        write('There is silver tape here'), nl.
        
describe(building_site) :-
        write('You are in front of building full of workers.'), nl,
        write('You can have a little chat with them.'), nl.
        


describe(penthouse) :-
        write('You are at the penthouse. Find the car.'), nl.

describe(gang_hideout) :-
        write('You are at the gang hideout. Find the weapon.'), nl.

