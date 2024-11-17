/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, has/1, knows/3, findable/2, access_code/3, leaving/2.
:- dynamic mission/2, mission_completed/1, finish_conditions/2, askable/2, obtainable/2.

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).
:- retractall(mission_completed(_)), retractall(has(_)), retractall(access_code(_, _)), retractall(leaving(_, _)).
:- retractall(obtainable(_, _)), retractall(askable(_, _)), retractall(findable(_, _)).

/* f it we ball, from now on, findable means thing lays somewhere undiscovered
obtainable means thing was found and could be stolen */

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

finish_conditions("mansion frontyard", "Jeep keys").
finish_conditions("mansion frontyard", "Porsche keys").
finish_conditions("mansion frontyard", "BMW keys").

finish_conditions(gang_hideout, handgun).
finish_conditions(room, handgun).
finish_conditions(gang_hideout, rifle).
finish_conditions(room, rifle).

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
at(gate, mansion).

at(bmw, "mansion frontyard").
at(porsche, "mansion frontyard").
at(jeep, "mansion frontyard").

at(shelf, outbuilding).

at(window, terrace).

at("first drawer", kitchen).
at("second drawer", kitchen).
at("third drawer", kitchen).
at("fourth drawer", kitchen).
at("fifth drawer", kitchen).

at(table, gang_hideout).
at(lamp, gang_hideout).
at(painting, gang_hideout).
at("closed door", gang_hideout).

askable(supervisor, construction_site_south_gate).
askable(worker, construction_site).

findable(beams, "black container").
findable(barrow, "yellow container").
findable(hammers, "red container").
findable(drill, "blue container").
findable(windows, "green container").

findable(drill, shelf).
findable(crowbar, shelf).
findable("Jeep keys", "first drawer").
findable("Porsche keys", "third drawer").
findable("BMW keys", "fifth drawer").

obtainable("key 7", "safe interior").
obtainable("key 8", "safe interior").
obtainable("key 9", "safe interior").

obtainable(handgun, room).
obtainable(rifle, room).
obtainable(knife, room).
obtainable("pepper spray", room).

access_code(gate, black20, mansion, "mansion frontyard").
access_code(window, crowbar, terrace, house).
access_code(safe, 1893, gang_hideout, "safe interior").
access_code("closed door", "key 7", gang_hideout, room).

leaving("mansion frontyard", mansion).
leaving(house, "mansion backyard").
leaving(kitchen, "mansion backyard").
leaving("safe interior", gang_hideout).
leaving(room, gang_hideout).

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
        retract(has(Thing)),
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
    write('You are not carrying anything.'), nl, !. % Nie można nic upuścić

drop :-
    has(Thing),                   % Gracz posiada jakiś przedmiot
    i_am_at(Location),            % Gracz jest w określonej lokalizacji
    retract(has(Thing)),          % Usuwamy przedmiot z ekwipunku
    assert(has(nothing)),         % Zmieniamy stan ekwipunku na "nic"
    assert(obtainable(Thing, Location)), % Dodajemy przedmiot do dostępnych w lokalizacji
    write('You dropped the '), write(Thing), write(' here.'), nl, !.



ask(Person) :-
        i_am_at(Location),
        askable(Person, Location),      % those 2 conditions guarantee that someone can be asked only at Location they are met
        write(Person), write(' says: '), speech(Person),
        !, nl.

ask(_) :-
        write('There is no such person here'), nl.



open(Thing, Tool) :-
        i_am_at(Location),
        access_code(Thing, Tool, Location, Entered_Location),
        at(Thing, Location),
        (findable(Tool, _) ; obtainable(Tool, _) -> has(Tool) ; true),
        (has(Tool) -> drop ; true),     % thanks to that any equipment needed to get into some place is left outside that place
        write('Access granted. You are now in '), write(Entered_Location), nl,   
        retract(i_am_at(Location)),
	assert(i_am_at(Entered_Location)),
        look,
        !, nl.

open(_, _) :-
        write('Access denied.'), nl.



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
        nl,
        Objects \= [] ->
        mention_objects(Objects)
    ;   true
    ),
    findall(Person, askable(Person, Location), People),
    (
        nl,
        People \= [] ->
        mention_people(People)
    ;   true
    ),
    findall(Y, obtainable(Y, Location), Obtainable),
    (
        nl,
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
    mention_people(T).

mention_obtainable([]).
mention_obtainable([H|T]) :-
    write('There is a '), write(H), write(' laying here.'), nl,
    mention_obtainable(T).



/* This rule tells how to end game. */
end_game :-
        mission_completed(CompletedMissions),
        member(drill, CompletedMissions),
        member(car, CompletedMissions),
        member(weapon, CompletedMissions),
        write('Congrats, you successfully completed all 3 missions - thanks to you bank robbery was a total success.'), nl,
        finish, !.

end_game :-
        write('Unfortunatelly you didn’t manage to complete all 3 missions.'), nl,
        write('Thanks for that gameplay.'), nl,
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */
finish :-
        nl,
        write('The game is over.'), nl,
        write('Please enter the "halt." command.'),
        nl.



/* This rule just writes out game instructions. */
instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             	-- to start the game.'), nl,
        write('n.  s.  e.  w.     	-- to go in that direction.'), nl,
	write('choose_mission(Mission).	-- to start mission (options: car, drill, weapon).'), nl,
	write('finish_mission(Mission). -- to finish mission (after completing required tasks - options: car, drill, weapon).'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('open(Thing, Tool/Code)   -- to open a secured (probably closed) Thing (can be gate, window or safe) using Tool or Code)'), nl,
        write('leave(Place)             -- to leave a place only if a place was entered with use od open() command'), nl,
        write('look.              	-- to look around you again.'), nl,
        write('instructions.      	-- to see this message again.'), nl,
        write('ask(Person).             -- to ask other characters (specified Person or group) about things.'), nl,
        write('end_game.                -- to end game (you win if all missions are complited)'), nl,
        write('halt.              	-- to end the game and quit.'), nl,
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
        i_am_at(lobby),
        write('You are in the lobby. You can choose a mission: drill, car, or weapon.'),
        nl, !.

examine(construction_site_south_gate) :-
        i_am_at(construction_site_south_gate),
        write('You are in front of south gate of a construction site.'), nl,
        write('There is a supervisor next to you. You can talk with him.'), nl, 
        write('On the construction site at north there are workers in a building'), nl,
        write('and containers with various construction equipment. Find the drill.'),
        nl, !.

examine(construction_site_east_gate) :-
        i_am_at(construction_site_east_gate),
	write('You reached the east gate of construction site.'), nl,
	write('There''s not much here, but this place seems like'), nl,
	write('another escape point'),
        nl, !.

examine(construction_site) :-
        i_am_at(construction_site),
        write('You entered construction site area.'), nl,
        write('There is a row of colorful containers to your right site.'), nl,
        write('You can also take a closer look at construction workers'), nl,
        write('as well as the uncompleted building on your left.'),
        nl, !.

examine(containers) :-
        i_am_at(containers),
        write('You are now standing in front of row of containers.'),
        nl, !.

examine("white container") :-
        i_am_at(containers),
        write('You take a look inside of the white container'), nl,
        notice_objects_inside("white container"),
        nl, !.

examine("black container") :-
        i_am_at(containers),
        write('You take a quick look inside of the black container'), nl,
        notice_objects_inside("black container"),
        nl, !.

examine("red container") :-
        i_am_at(containers),
        write('You take a glance inside of the red container'), nl,
        notice_objects_inside("red container"),
        nl, !.

examine("blue container") :-
        i_am_at(containers),
        write('You take a glimpse of contents of the blue container'), nl,
        notice_objects_inside("blue container"),
        nl, !.

examine("green container") :-
        i_am_at(containers),
        write('You open the door of the green container'), nl,
        notice_objects_inside("green container"),
        nl, !.

examine("yellow container") :-
        i_am_at(containers),
        write('You take a look inside of the yellow container'), nl,
        notice_objects_inside("yellow container"),
        nl, !.



examine(mansion) :-
        i_am_at(mansion),
        write('You are in front of a luxury mansion in expensive neighbourhood of Los Santos - Vinewood hills.'), nl, 
        write('There is a closed gate, that requires a password.'), nl,
        write('You look around the neighbourhood and notice, that number of the house to the left is 18, '), nl,
        write('and a mailbox with an envelope sticking out of it.'), nl,
        write('Find keys to the car you want to steal, take them and go to the choosen car, then you can finish mission'),
        nl, !.

examine(envelope) :-
        i_am_at(mansion),
        write('Dear residents of Mayfair St.'), nl,
        write('Due to scheduled replacement of intercoms in upcoming week, we kindly inform you, '), nl,
        write('that access password to mansion gate will be changed to combination of your mansion’s roof colour and address’ number(example: black1234).'), nl,
        write('We are sorry for the inconvenience'), nl, 
        write('Best regards'), nl,
        write('VH housing'),
        nl, !.

examine(neighbourhood) :-
        i_am_at(mansion),
        write('You notice an interesting pattern.'), nl,
        write('Every third house on the other side of the road has a blue roof, other ones have red roofs.'), nl,
        write('What’s more, houses with blue roof have a black-roofed house in front of them, other ones have green roofs.'), nl,
        write('You also notice that the leftmost house on the other side of the road has a blue roof and house number 1.'), nl,
        write('On one side on the road there are only even house numbers, on the other only odd'),
        nl, !.

examine(gate) :-
        i_am_at(mansion),
        write('The mansion’s gate is secured. You need a password to get through.'),
        nl, !.

examine("mansion frontyard") :-
        i_am_at("mansion frontyard"),
        write('You entered mansion frontyard.'), nl, 
        write('There is a pavement leading to the back of the house and a wooden outbuilding on the right side.'), nl,
        write('You also notice 3 cars standing on a driveway: .'),
        nl, !.

examine(bmw) :-
        i_am_at("mansion frontyard"),
        write('An armored version of this car, that can withstand gunshots, at cost of not being too fast.'),
        nl, !.

examine(jeep) :-
        i_am_at("mansion frontyard"),
        write('A quick SUV, capable of driving through more remote terrain.'),
        nl, !.

examine(porsche) :-
        i_am_at("mansion frontyard"),
        write('A sports coupe, that can go through paved roads very quickly.'),
        nl, !.

examine(outbuilding) :-
        i_am_at(outbuilding),
        write('You are inside of a wooden outbuilding. You notice a shelf, there should be some tools around you.'),
        nl, !.

examine(shelf) :-
        i_am_at(outbuilding),
        write('You took a closer look at the shelf.'), nl,
        notice_objects_inside(shelf),
        nl, !.

examine("mansion backyard") :-
        i_am_at("mansion backyard"),
        write('You silently walk around the house and now are in backyard.'), nl,
        write('You notice an outdoor swimming pool on your right, a cozy garden in front of you and a terrace on the left.'),
        nl, !.

examine(pool) :-
        (i_am_at(pool) ; i_am_at("mansion backyard")),
        write('You see a oval-shaped pool with sunbeds in front of it.'), nl,
        write('There is also a breathtaking view at downtown Los Santos from here.'), nl,
        write('Owner of this house must be really living the life!'),
        nl, !.

examine(garden) :-
        (i_am_at(garden) ; i_am_at("mansion backyard")),
        write('The garden has freshly cut lawns and vibrant flowerbeds.'), nl,
        write('It''s symmetrically arranged around central fountain.'),
        nl, !.

examine(terrace) :-
        (i_am_at(terrace) ; i_am_at("mansion backyard")),
        write('You take a closer look at the terrace.'),
        nl, !.

examine(window) :-
        i_am_at(terrace),
        write('The window is half- opened. You can’t go through it, but maybe you can find some useful tools to help yourself with opening it?'),
        nl, !.

examine(house) :-
        i_am_at(house),
        write('You are inside the house (crowbar was dropped at the terrace not to arouse suspicion).'), nl,
        write('You see a very spacious living room connected with the kitchen in front of you.'),
        nl, !.

examine(kitchen) :-
        (i_am_at(kitchen) ; i_am_at(house)),
        write('You notice that kitchen has 5 drawers right at the enterance...'),
        nl, !.

examine("first drawer") :-
        i_am_at(kitchen),
        write('You took a closer look at the first drawer.'), nl,
        notice_objects_inside("first drawer"),
        nl, !.

examine("second drawer") :-
        i_am_at(kitchen),
        write('You took a closer look at the second drawer.'), nl,
        notice_objects_inside("first drawer"),
        nl, !.

examine("third drawer") :-
        i_am_at(kitchen),
        write('You took a closer look at the third drawer.'), nl,
        notice_objects_inside("first drawer"),
        nl, !.

examine("fourth drawer") :-
        i_am_at(kitchen),
        write('You took a closer look at the fourth drawer.'), nl,
        notice_objects_inside("first drawer"),
        nl, !.

examine("fifth drawer") :-
        i_am_at(kitchen),
        write('You took a closer look at the fifth drawer.'), nl,
        notice_objects_inside("first drawer"),
        nl, !.



examine(gang_hideout) :-
        i_am_at(gang_hideout),
        write('You arrive at the gang hideout.'), nl,
        write('It’s a middle-sized room with a table in the middle, few chairs and sofas around it.'), nl,
        write('You also notice a table in the corner with a lamp on it and closed door to your left.'), nl,
        write('You turn it on and notice a painting and smoke in the air.'), nl,
        write('Someone must have been here recently. Search the place and find a weapon.'),
        nl, !.

examine(table) :-
        i_am_at(gang_hideout),
        write('You take a closer look at the table.'), nl,
        write('You notice playing cards, empty liquor bottles and a mirror with suspiciously looking white powder spilled on it.'),
        nl, !.

examine(lamp) :-
        i_am_at(gang_hideout),
        write('You pick up the lamp.'), nl,
        write('There’s nothing special about it, however when you’re about to put it back, you notice a small note hidden under it.'), nl,
        write('THE YEAR THE PAINTING WAS CREATED'),
        nl, !.
  
examine(painting) :-
        i_am_at(gang_hideout),
        write('After taking a closer look at the painting you identify it as replica of “The Scream” by Edvard Munch.'), nl,
        write('You also notice hinges on its side, moving painting to the side reveals a hidden safe behind it.'), nl,
        write('You need a code to open it.'), nl,
        assert(at(safe, gang_hideout)),
        nl, !.

examine("safe interior") :-
        i_am_at("safe interior"),
        write('It occurs that safe was only an entrance to a hidden room.'), nl,
        write('You notice 3 keys hanging on the wall in fornt of you with labels: "key 7", "key 8", "key 9".'), nl,
        write('You also see a poster on wall on your left.'),
        nl, !.

examine(poster) :-
        i_am_at("safe interior"),
        write('This is an old movie poster - David Fincher’s movie about _ deadly sins. The title is erased.'), nl,
        write('You notice small message above the erased title - it’s number of deadly sins.'),
        nl, !.

examine(room) :-
        i_am_at(room),
        write('You entered the gang’s weapon magazine. You drooped the key in previous room before entering that one.'), nl,
        write('You notice a gun cabinet with various weapons inside, including a handgun, a rifle, pepper spray and knife.'),
        nl, !.

examine(handgun) :-
        i_am_at(Location),
        (obtainable(handgun, Location) ; findable(handgun, Location)),
        write('That is handgun. It causes a lot of harm'),
        nl, !.

examine(rifle) :-
        i_am_at(Location),
        (obtainable(rifle, Location) ; findable(rifle, Location)),
        write('That is rifle. It is loud and fancy'),
        nl, !.

examine(knife) :-
        i_am_at(Location),
        (obtainable(knife, Location) ; findable(knife, Location)),
        write('That is knife. Take it if you like to get dirty at work.'),
        nl, !.

examine("pepper spray") :-
        i_am_at(Location),
        (obtainable("pepper spray", Location) ; findable("pepper spray", Location)),
        write('That is pepper spray. It is actually quite useless.'),
        nl, !.

examine(_) :-
        write('You cannot examine that object here.'), nl.



speech(supervisor) :-
        write('Hey how are you?'), nl,
        write('It''s beautiful weather out there isn''t it?'), nl.

speech(worker) :-
        write('Hey, you looking for something?'), nl,
        write('Check out this building, it''s beautiful, isn''t it?'), nl,
        write('I can''t wait till we finish it!'), nl.
