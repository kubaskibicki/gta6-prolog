/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, has/1, knows/3, findable/2, access_code/3, leaving/2.
:- dynamic mission/2, mission_completed/1, finish_conditions/2, askable/2, obtainable/2.

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).
:- retractall(mission_completed(_)), retractall(has(_)), retractall(access_code(_, _)), retractall(leaving(_, _)).

/* f it we ball, from now on, findable means shit lays somewhere undiscovered
obtainable means shit was found and could be stolen */

has(nothing).

/* Player's starting location */
i_am_at(lobby).

/* List of finished missions */
mission_completed([]).

/* Missions definition */
mission(drill, construction_site_south_gate).
mission(car, mansion).
mission(weapon, gang_hideout).

finish_conditions(construction_site_south_gate, drill).
finish_conditions(construction_site_east_gate, drill).

finish_conditions("mansion frontyard", car).

path(construction_site_south_gate, n, construction_site).
path(construction_site, s, construction_site_south_gate).
path(construction_site, e, containers).
path(containers, w, construction_site).
path(containers, e, construction_site_east_gate).
path(construction_site_east_gate, w, containers).

path("mansion frontyard", e, outbuilding).
path(outbuilding, w, "mansion frontyard").
path("mansion frontyard", n, "mansion backyard").
path("mansion backyard", s, "mansion frontyard").
path("mansion backyard", w, terrace).
path(terrace, e, "mansion backyard").
path("mansion backyard", e, pool).
path(pool, w, "mansion backyard").
path("mansion backyard", n, garden).
path(garden, s, "mansion backyard").
path(house, n, kitchen).
path(kitchen, s, house).

at("white container", containers).
at("green container", containers).
at("blue container", containers).
at("red container", containers).
at("black container", containers).
at("yellow container", containers).

at(neighbourhood, mansion).
at(envelope, mansion).

at(bmw, "mansion frontyard").
at(porsche, "mansion frontyard").
at(jeep, "mansion frontyard").

at(outbuilding_shelf, outbuilding).

at("first drawer", kitchen).
at("second drawer", kitchen).
at("third drawer", kitchen).
at("fourth drawer", kitchen).
at("fifth drawer", kitchen).

askable(supervisor, construction_site_south_gate).
askable(worker, construction_site).

findable(beams, "black container").
findable(barrow, "yellow container").
findable(hammers, "red container").
findable(drill, "blue container").
findable(windows, "green container").

findable(drill, outbuilding_shelf).
findable(crowbar, outbuilding_shelf).
findable("Jeep keys", "first drawer").
findable("Porsche keys", "third drawer").
findable("BMW keys", "fifth drawer").

access_code(black20, mansion, "mansion frontyard").
access_code(crowbar, terrace, house).

leaving("mansion frontyard", mansion).
leaving(house, "mansion backyard").
leaving(kitchen, "mansion backyard").

/* Rule of choosing a mission */
choose_mission(Thing) :-
	i_am_at(lobby),
	mission(Thing, Location),
        mission_completed(CompletedMissions),
	\+ member(Thing, CompletedMissions),  /* true if mission not completed */
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
        mission_completed(CompletedMissions),
        retract(mission_completed(CompletedMissions)),
        assert(mission_completed([Thing | CompletedMissions])),
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
    has(nothing),
    i_am_at(Location),
    obtainable(Thing, Location),
    retract(obtainable(Thing, Location)),
    retract(has(nothing)),
    assert(has(Thing)),
    write('You took the '), write(Thing), write('.'), nl,
    !, nl.

take(_) :-
    has(nothing),
    write('There is no such thing here'), nl,
    !, nl.

% Jeśli gracz już coś nosi:
take(_) :-
    write('You are already carrying something'),
    nl.


/* These rules describe how to put down an object. */
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




ask(Person) :-
        i_am_at(Location),
        askable(Person, Location),      % those 2 conditions guarantee that someone can be asked only at Location they are met
        write(Person), write(' says: '), speech(Person),
        !, nl.

ask(_) :-
        write('There is no such person here'), nl.





enter(Code, Location) :-
        i_am_at(Location),
        access_code(Code, Location, Entered_Location),
        write('Access granted. You are now in '), write(Entered_Location), nl,
        drop,   % thanks to that any equipment needed to get into some place is left outside that place
        retract(i_am_at(Location)),
	assert(i_am_at(Entered_Location)),
        look,
        !, nl.

enter(_, _) :-
        write('Access denied'), nl.



leave(Location) :-
        i_am_at(Location),
        leaving(Location, Exit),
        retract(i_am_at(Location)),
	assert(i_am_at(Exit)),
        write('You exited '), write(Location), nl,
        write('You are now in '), write(Exit),
        !, nl.

leave(_) :-
        write('You are not in the closed location that can be left like that.').


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
        i_am_at(Location),
        examine(Location),
        nl,
        notice_objects_at(Location),
        !, nl.


notice_objects_inside(Location) :-
    findall(Z, findable(Z, Location), Findable),
    (
        Findable \= [] ->
        mention_findable(Findable)
    ;   true
    ),
    !, nl.

mention_findable([]).
mention_findable([H|T]) :-
    i_am_at(Location),
    findable(H, Sub),
    retract(findable(H, Sub)),
    write('You discovered '), write(H), nl,
    assert(obtainable(H, Location)),
    nl,
    mention_findable(T).


/* These rules set up a loop to mention all the objects
   in your vicinity. */
notice_objects_at(Location) :-
    findall(X, at(X, Location), Objects),
    (
        Objects \= [] ->
        mention_objects(Objects)
    ;   true
    ),
    findall(Person, askable(Person, Location), People),
    (
        People \= [] ->
        mention_people(People)
    ;   true
    ),
    findall(Y, obtainable(Y, Location), Obtainable),
    (
        Obtainable \= [] ->
        mention_obtainable(Obtainable)
    ;   true
    ).

mention_objects([]).
mention_objects([H|T]) :-
    write('There is a '), write(H), write(' here.'), nl,
    mention_objects(T).

mention_people([]).
mention_people([H|T]) :-
    write('You meet '), write(H), nl,
    nl,
    mention_people(T).

mention_obtainable([]).
mention_obtainable([H|T]) :-
    write('There is a '), write(H), write(' laying here.'), nl,
    mention_obtainable(T).



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
	    write('choose_mission(Mission).	-- to start mission.'), nl,
	    write('finish_mission(Mission).  -- to finish mission (after completing required tasks).'), nl,
        write('take(Object).      	     -- to pick up an object.'), nl,
        write('drop(Object).      	     -- to put down an object.'), nl,
        write('enter(Thing, Place)       -- to enter a (probably closed) Place using Thing (tool or code)'), nl,
        write('leave(Place)              -- to leave a place only if a place was entered with use od enter() command'), nl,
        write('look.              	     -- to look around you again.'), nl,
        write('instructions.      	     -- to see this message again.'), nl,
        write('ask.              	     -- to ask other characters about things.'), nl,
        write('halt.              	     -- to end the game and quit.'), nl,
        nl.



/* This is description of game plot */
game_description :-
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
        game_description,
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
        notice_objects_inside("white container"), nl.

examine("black container") :-
        write('You take a quick look inside of the black container'), nl,
        notice_objects_inside("black container"), nl.

examine("red container") :-
        write('You take a glance inside of the red container'), nl,
        notice_objects_inside("red container"), nl.

examine("blue container") :-
        write('You take a glimpse of contents of the blue container'), nl,
        notice_objects_inside("blue container"), nl.

examine("green container") :-
        write('You open the door of the green container'), nl,
        notice_objects_inside("green container"), nl.

examine("yellow container") :-
        write('You take a look inside of the yellow container'), nl,
        notice_objects_inside("yellow container"), nl.



examine(mansion) :-
        write('You are in front of a luxury mansion in expensive neighbourhood of Los Santos - Vinewood hills.'), nl, 
        write('There is a closed entrance, that requires a password.'), nl,
        write('You look around the neighbourhood and notice, that number of the house to the left is 18, '), nl,
        write('and a mailbox with an envelope sticking out of it.'), nl,
        write('Find keys to the car you want to steal, take them and go to the choosen car, then you can finish mission'), nl.

examine(envelope) :-
        write('Dear residents of Mayfair St.'), nl,
        write('Due to scheduled replacement of intercoms in upcoming week, we kindly inform you, '), nl,
        write('that access passwords will be changed to combination of your house’s roof colour and address’ number(example: black1234).'), nl,
        write('We are sorry for the inconvenience'), nl, 
        write('Best regards'), nl,
        write('VH housing'), nl.

examine(neighbourhood) :-
        write('You notice an interesting pattern.'), nl,
        write('Every third house on the other side of the road has a blue roof, other ones have red roofs.'), nl,
        write('What’s more, houses with blue roof have a black-roofed house in front of them, other ones have green roofs.'), nl,
        write('You also notice that the leftmost house on the other side of the road has a blue roof and house number 1.'), nl,
        write('On one side on the road there are only even house numbers, on the other only odd'), nl.

examine("mansion frontyard") :-
        write('You entered mansion frontyard.'), nl, 
        % write('Jeep - a quick SUV, capable of driving through more remote terrain.'), nl,
        % write('Porsche 911 - sports coupe, that can go through paved roads very quickly.'), nl,
        % write('BMW M760 - armored version that can withstand gunshots, at cost of not being too fast.'), nl,
        write('There is a pavement leading to the back of the house and a wooden outbuilding on the right side.'), nl,
        write('You also notice 3 cars standing on a driveway: .'), nl.

examine(bmw) :-
        write('An armored version of this car, that can withstand gunshots, at cost of not being too fast.'), nl.

examine(jeep) :-
        write('A quick SUV, capable of driving through more remote terrain.'), nl.

examine(porsche) :-
        write('A sports coupe, that can go through paved roads very quickly.'), nl.

examine(outbuilding) :-
        write('You are inside of a wooden outbilding. You notice a shelf, there should be some tools around you.'), nl.

examine(outbuilding_shelf) :-
        write('You took a closer look at the shelf.'), nl,
        notice_objects_inside(outbuilding_shelf), nl.

examine("mansion backyard") :-
        write('You silently walk around the house and now are in backyard.'), nl,
        write('You notice an outdoor swimming pool, a cozy garden and a terrace.'), nl.

examine(pool) :-
        write('You see a oval-shaped pool with sunbeds in front of it.'), nl,
        write('There is also a breathtaking view at downtown Los Santos from here.'), nl,
        write('Owner of this house must be really living the life!'), nl.

examine(garden) :-
        write('The garden has freshly cut lawns and vibrant flowerbeds.'), nl,
        write('It''s symmetrically arranged around central fountain.'), nl.

examine(terrace) :-
        write('You take a closer look at the terrace and notice a half-opened window.'), nl,
        write('You can’t go through it, but maybe you can find some useful tools to help yourself with opening it?'), nl.

examine(house) :-
        write('You successfully make it inside the house (and dropped crowbar at the terrace not to arouse suspicion).'), nl,
        write('You see a very spacious living room connected with the kitchen in front of you.'), nl.

examine(kitchen) :-
        write('You notice that kitchen has 5 drawers right at the enterance...'), nl.

examine("first drawer") :-
        write('You took a closer look at the first drawer.'), nl,
        notice_objects_inside("first drawer"), nl.

examine("second drawer") :-
        write('You took a closer look at the second drawer.'), nl,
        notice_objects_inside("first drawer"), nl.

examine("third drawer") :-
        write('You took a closer look at the third drawer.'), nl,
        notice_objects_inside("first drawer"), nl.

examine("fourth drawer") :-
        write('You took a closer look at the fourth drawer.'), nl,
        notice_objects_inside("first drawer"), nl.

examine("fifth drawer") :-
        write('You took a closer look at the fifth drawer.'), nl,
        notice_objects_inside("first drawer"), nl.

examine(crowbar) :-
        write('You take a closer look at the crowbar'), nl,
        write('It looks handy. And it''s red!'), nl.

examine(drill) :-
        write('You see a drill on one of the shelves in front of you'), nl,
        write('Too bad it''s just a regular drill, not a concrete one...'), nl.

examine(gang_hideout) :-
        write('You are at the gang hideout. Find the weapon.'), nl.

speech(supervisor) :-
        write('Hey how are you?'), nl,
        write('It''s beautiful weather out there isn''t it?'), nl.

speech(worker) :-
        write('Hey, you looking for something?'), nl,
        write('Check out this building, it''s beautiful, isn''t it?'), nl,
        write('I can''t wait till we finish it!'), nl.
