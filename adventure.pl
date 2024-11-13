/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, has/1, knows/3, obtainable/2, access_code/3.
:- dynamic mission/2, mission_completed/1, finish_conditions/2.

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).
:- retractall(mission_completed(_)), retractall(has(_)), retractall(access_code(_, _)).

has(nothing).

/* Player's starting location */
i_am_at(lobby).

finish_conditions(construction_site_south_gate, drill).
finish_conditions(construction_site_east_gate, drill).

access_code(18black, mansion, "mansion frontyard").
access_code(crowbar, terrace, house).

/* Missions definition */
mission(drill, construction_site_south_gate).
mission(car, mansion).
mission(weapon, gang_hideout).

path(construction_site_south_gate, n, construction_site).
path(construction_site, s, construction_site_south_gate).
path(construction_site, e, containers).
path(containers, w, construction_site).
path(containers, e, construction_site_east_gate).
path(construction_site_east_gate, w, containers).

path("mansion frontyard", e, "wooden outbuilding").
path("wooden outbuilding", w, "mansion frontyard").
path("mansion frontyard", n, "mansion backyard").
path("mansion backyard", s, "mansion frontyard").

at("white container", containers).
at("green container", containers).
at("blue container", containers).
at("red container", containers).
at("black container", containers).
at("yellow container", containers).

at(supervisor, construction_site_south_gate).
at(workers, construction_site).
at(building, construction_site).

at(envelope, mansion).

at("BMW M760", "mansion frontyard").
at("Porsche 911", "mansion frontyard").
at("Mercedes G63", "mansion frontyard").

at("swimming pool", "mansion backyard").
at("garden", "mansion backyard").
at("terrace", "mansion backyard").

at(first_drawer, house).
at(second_drawer, house).
at(third_drawer, house).
at(fourth_drawer, house).
at(fifth_drawer, house).

obtainable(beams, containers).
obtainable(barrow, containers).
obtainable(hammers, containers).
obtainable(drill, containers).
obtainable(windows, containers).

obtainable(crowbar, "mansion backyard").

obtainable("G63 keys", house).
obtainable("Porsche keys", house).
obtainable("BMW keys", house).

knows(supervisor, building, 'We are building new lifeinvader headquarters').
knows(supervisor, drill, 'You probably need that drill for heist. I am calling the cops').
knows(supervisor, containers, 'What is in those containers? Well, construction equipment. 
        If I remember correctly, there is a concrete mixer machine in the yellow one, 
        a concrete drill in the blue one and some steel beams in the white one. 
        I’m not sure about the other ones though').
knows(workers, building, 'Doing great! Can’t wait to see this beauty finished! If u need sth, let us know').
knows(workers, drill, 'Yeah, there should be some old drills in one of our toolboxes on the construction site over there').



/* Rule of choosing a mission */
choose_mission(Thing) :-
	i_am_at(lobby),
	mission(Thing, Location),
	\+ mission_completed(Thing),  /* true if mission not completed */
	retract(i_am_at(lobby)),
	assert(i_am_at(Location)),
	write('You have chosen the mission to get the '), write(Thing), write('.'), nl,
	look,
	!, nl.

choose_mission(_) :-
	i_am_at(lobby),
        write('Invalid mission name or mission already completed.'),
	!, nl.
    
choose_mission(_) :-
        write('Missions can only be started in lobby.'), nl.



/* Reguła zakończenia misji */
finish_mission(Thing) :-
	i_am_at(Location),
	has(Thing),
	finish_conditions(Location, Thing),
	assert(mission_completed(Thing)),
	% retract(mission(_, _)),
	write('You have completed the mission to get the '), write(Thing), write('.'), nl,
	return_to_lobby,
	!, nl.
    
finish_mission(_) :-
	write('You are not at the correct location to complete this mission or did not found the correct object'), nl.
    


/* Reguła powrotu do lobby */
return_to_lobby :-
	retract(i_am_at(_)),
	assert(i_am_at(lobby)),
	write('You finished your mission and returned to the lobby.'), nl,
	write('Congrats. Now choose what to do next'), nl,
	look.



/* These rules describe how to pick up an object. */
take(Thing) :-
	has(nothing),                  % Gracz nie może mieć nic w ekwipunku
	i_am_at(Location),             % Lokalizacja gracza
	obtainable(Thing, Location),   % Przedmiot musi być dostępny w tej lokalizacji
	retract(has(nothing)),         % Usuwamy informację, że gracz ma pusty ekwipunek
	assert(has(Thing)),            % Dodajemy przedmiot do ekwipunku
	write('You took the '), write(Thing), write('.'), nl,
	!, nl.

% Jeżeli gracz już coś nosi, nie może podnieść innego przedmiotu
take(_) :-
	has(Something), Something \= nothing,
	write('You are already carrying something: '), write(Something), write('.'), nl,
	!, nl.

% Jeżeli w lokalizacji nie ma przedmiotu
take(Thing) :-
	i_am_at(Location),
	\+ obtainable(Thing, Location),
	write('There is no '), write(Thing), write(' here.'), nl,
	!, nl.



/* These rules describe how to put down an object. */
% Reguła odpowiadająca za upuszczenie przedmiotu
drop :-
    has(nothing),                 % Jeżeli gracz nic nie posiada
    write('You are not carrying anything to drop.'), nl, !. % Nie można nic upuścić

drop :-
    has(Thing),                   % Gracz posiada jakiś przedmiot
    i_am_at(Location),            % Gracz jest w określonej lokalizacji
    retract(has(Thing)),          % Usuwamy przedmiot z ekwipunku
    assert(has(nothing)),         % Zmieniamy stan ekwipunku na "nic"
    assert(obtainable(Thing, Location)), % Dodajemy przedmiot do dostępnych w lokalizacji
    write('You dropped the '), write(Thing), write(' here.'), nl.



ask(Person, Thing) :-
        i_am_at(Place),
        at(Person, Place),      % those 2 conditions guarantee that someone can be asked only at place they are met
        knows(Person, Thing, Response),
        write(Person), write('says: '), write(Response),
        !, nl.

ask(Person, _) :-
        i_am_at(Place),
	at(Person, Place), 
        write('They don\'t know anything about that.'),
	!, nl.


ask(_, _) :-
        write('There is no such person here'), nl.



enter(Code, Place) :-
        i_am_at(Place),
        access_code(Code, Place, Entered_Place),
        wirte('Access granted. You are now in '), write(Entered_Place), nl,
        drop,   % thanks to that any equipment needed to get into some place is left outside that place
        retract(i_am_at(Place)),
	assert(i_am_at(Entered_Place)),
        look,
        !, nl.

enter(_, _) :-
        write('Access denied'), nl.



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



/* This rule tells how to look around you. */
look :-
        i_am_at(Place),
        examine(Place),
        nl,
        notice_objects_at(Place),
        !, nl.



/* These rules set up a loop to mention all the objects
   in your vicinity. */
notice_objects_at(Place) :-
        (at(X, Place) ; obtainable(X, Place)),
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
"game description" :-
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
        "game description",
        look.



/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
examine(lobby) :-
        write('You are in the lobby. You can choose a mission: drill, car, or weapon.'), nl.

examine(construction_site_south_gate) :-
        write('You are in front of south gate of a construction site.'), nl,
        write('There is a supervisor next to you. You can talk with him.'), nl, 
        write('On the construction site at north there are workers in a building'), nl,
        write('and containers with various construction equipment. Find the drill.'), nl.

examine(construction_site_east_gate) :-
	write('You reached the east gate of construction site.'), nl,
	write('There''s not much here, but this place seems like'), nl,
	write('another escape point'), nl.

examine(construction_site) :-
        write('You entered construction site area.'), nl,
        write('There is a row of colorful containers to your right site.'), nl,
        write('You can also take a closer look at construction workers'), nl,
        write('as well as the uncompleted building on your left.'), nl.

examine(containers) :-
        write('You are now standing in front of row of containers.'), nl.

examine("white container") :-
        write('You take a look inside of the white container'), nl,
        write('Unfortunately it is empty'), nl.
        
examine("black container") :-
        write('You take a quick look inside of the black container'), nl,
        write('You find few beams inside'), nl.

examine("red container") :-
        write('You take a glance inside of the red container'), nl,
        write('You find hammers inside'), nl.

examine("blue container") :-
        write('You take a glimpse of contents of the blue container'), nl,
        write('You find drill inside'), nl.

examine("green container") :-
        write('You you open the door of the green container'), nl,
        write('You find windows inside'), nl.

examine("yellow container") :-
        write('You take a look inside of the yellow container'), nl,
        write('You find a barrow inside'), nl.



examine("mansion") :-
        write('You are in front of a luxury mansion in expensive 
                neighbourhood of Los Santos - Vinewood hills.'), nl, 
        write('There is a closed entrance, that requires a password.'), nl,
        write('You look around the neighbourhood and notice, that number of the house to the left is 18, and a mailbox with an 
                envelope sticking out of it.'), nl,
        write('Find keys to the car you want to steal'), nl.

examine("envelope") :-
        write('Dear residents of Mayfair St.'), nl,
        write('Due to scheduled replacement of intercoms in 
        upcoming week, we kindly inform you,that access passwords will be changed
        to combination of your address’ number and house’s roof colour (example: 3218black).'), nl,
        write('We are sorry for the inconvenience'), nl, 
        write('Best regards'), nl,
        write('VH housing'), nl.

examine("neighbourhood") :-
        write('You notice an interesting pattern.'), nl,
        write('Every third house on the other side of the road has a blue roof, 
                other ones have red roofs.'), nl,
        write('What’s more, houses with blue roof have a black-roofed house in front of 
                them, other ones have green roofs.'), nl,
        write('You also notice that the leftmost house on the  
                other side of the road has a blue roof and house number 1.'), nl,
        write('On one side on the road there are only even house numbers, on the other only odd'), nl.

examine("mansion frontyard") :-
        write('You entered mansion frontyard.'), nl,
        write('You notice 3 cars standing on a driveway.'), nl, 
        write('Mercedes G63 - a quick SUV, capable of driving through more remote terrain.'), nl,
        write('Porsche 911 - sports coupe, that can go through paved roads very quickly.'), nl,
        write('BMW M760 - armored version that can withstand gunshots, at cost of not being too fast.'), nl,
        write('There is also a pavement leading to the back of the house and a wooden outbuilding on the right side.'), nl.

examine("BMW M760") :-
        write('An armored version of this car, that can withstand gunshots, at cost of not being too fast.'), nl.

examine("Mercedes G63") :-
        write('A quick SUV, capable of driving through more remote terrain.'), nl.

examine("Porsche 911")
        write('A sports coupe, that can go through paved roads very quickly.'), nl.

examine("wooden outbuilding") :-
        write('You are next to the wooden outbilding. There should be some tools beside the entrance to the outbuilding'), nl.

examine("mansion backyard") :-
        write('You silently walk around the house and notice an outdoor swimming pool, a cozy garden and a terrace.'), nl.

examine("swimming pool") :-
        write(), nl.

examine("garden") :-
        write(), nl.

examine(terrace) :-
        write('You take a closer look at the terrace and notice a half-opened window.'), nl,
        write('You can’t go through it, but maybe you can find some useful tools to help yourself with opening it?'), nl.

examine(house) :-
        write('You successfully make it inside the house (and dropped crowbar at the terrace not to arouse suspicion).'), nl,
        write('You see a very spacious living room connected with the kitchen.'), nl,
        write('Kitchen has 5 drawers right at the enterance...'), nl.

examine(first_drawer) :-
        write('You find G63 keys'), nl.

examine(second_drawer) :-
        write('You find BMW keys'), nl.

examine(third_drawer) :-
        write('You find Porsche keys'), nl.

examine(fourth_drawer) :-
        write('You find a knife'), nl.

examine(fifth_drawer) :-
        write('You find a tape'), nl.



examine(gang_hideout) :-
        write('You are at the gang hideout. Find the weapon.'), nl.

