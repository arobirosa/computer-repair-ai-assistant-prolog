%!
% It proposes the possible cause of the malfunction by asking you the observed symptoms.
%
% Predicates
%   start   Starts a new repair case. It removes all old facts entered by the user.
%
% TODO The following predicates must be implemented
%   report  Generates a report
%   change  Modifies the answers
%   save(repairCaseID)  Saves the case into the database
%   load(repairCaseID)  Loads the case from the database

%%%%%%% DEFINITION OF SYMPTOMS

% TODO Add conditions which are negated.
symptom(battery_light_do_not_turn_on) :- verify("Is the battery light turned off after the power button is turned on?").
symptom(battery_light_turns_on) :- verify("Is the battery light turned on after the power button is turned on?").
symptom(hard_disk_spins_on_a_second_motherboard) :- verify("Does the hard disk spin when connected to a second motherboard?").
symptom(known_working_hard_disk_do_not_spin_on_motherboard) :- verify("Does a hard disk which is known to work don't spin on the motherboard?").
symptom(ram_modules_not_compatible_with_motherboard) :- verify("The speed and form of the RAM modules are not documented on the manual of the motherboard?").
symptom(ram_modules_were_updated_lately) :- verify("Was the RAM module changed lately?").
symptom(screen_is_not_displaying_anything) :- verify("Is display blank? It doesn't display anything").
symptom(system_do_not_turn_on) :- verify("The system don't turn on, right?").
symptom(system_turns_on) :- verify("Does the system turn on?").
symptom(system_do_not_work_after_reinserting_power_cable_connector) :- verify("The system don't turn on after taking out and reinserting the power connector from the PSU to the motherboard, right?").
symptom(system_works_after_reinserting_power_cable_connector) :- verify("The system turns on after taking out and reinserting the power connector from the PSU to the motherboard, right?").
symptom(system_do_not_work_with_another_power_cord) :- verify("The system don't turn on with another power cord, right?").
symptom(system_has_a_power_supply_unit) :- verify("Does the system have a power supply unit? Laptops don't have one, while desktops do.").
symptom(system_works_after_reinserting_the_ram_modules) :- verify("After you remove the RAM module(s) and insert them in the slot, does the system work?").
symptom(system_works_on_another_outlet) :- verify("Does the system work in another power outlet?").
symptom(system_works_with_a_new_motherboard) :- verify("Does the computer work with a new motherboard?").
symptom(system_works_with_a_new_power_supply_unit) :- verify("Does the computer work with a new power supply?").
symptom(system_works_with_another_power_cord) :- verify("Does the computer work with a another power cord?").
symptom(system_works_with_old_modules) :- verify("Does the computer work with the old RAM module(s)?").

%%%%%%% DEFINITION OF BROKEN COMPONENT

brokenComponent(no_electricity_on_the_power_outlet) :-
    symptom(system_do_not_turn_on),
    symptom(battery_light_do_not_turn_on),
    symptom(system_works_on_another_outlet).

brokenComponent(power_cord) :-
    symptom(system_do_not_turn_on),
    symptom(battery_light_do_not_turn_on),
    symptom(system_works_with_another_power_cord).

brokenComponent(motherboard_power_cable_is_disconnected) :-
    symptom(system_do_not_turn_on),
    symptom(battery_light_do_not_turn_on),
    symptom(system_do_not_work_with_another_power_cord), % TODO Add conditions which are negated.
    symptom(system_works_after_reinserting_power_cable_connector),
    symptom(hard_disk_spins_on_a_second_motherboard).

brokenComponent(motherboard_is_burnt) :-
    symptom(system_do_not_turn_on),
    symptom(battery_light_do_not_turn_on),
    symptom(system_do_not_work_with_another_power_cord), % TODO Add conditions which are negated.
    symptom(system_do_not_work_after_reinserting_power_cable_connector),
    % The next line shows how two tests are possible to diagnose. It applies an OR condition
    (symptom(hard_disk_spins_on_a_second_motherboard);symptom(known_working_hard_disk_do_not_spin_on_motherboard)),
    symptom(system_works_with_a_new_motherboard).

brokenComponent(power_supply_unit_is_burnt) :-
    symptom(system_do_not_turn_on),
    symptom(battery_light_do_not_turn_on),
    symptom(system_do_not_work_with_another_power_cord), % TODO Add conditions which are negated.
    symptom(system_has_a_power_supply_unit),
    symptom(system_do_not_work_after_reinserting_power_cable_connector),
    symptom(system_works_with_a_new_power_supply_unit).

brokenComponent(incompatible_ram_modules) :-
    symptom(system_turns_on), % TODO Add conditions which are negated.
    symptom(battery_light_turns_on),
    symptom(screen_is_not_displaying_anything),
    symptom(ram_modules_were_updated_lately),
    symptom(ram_modules_not_compatible_with_motherboard),
    symptom(system_works_with_old_modules).

brokenComponent(disconnected_ram_modules) :-
    symptom(system_turns_on), % TODO Add conditions which are negated.
    symptom(battery_light_turns_on),
    symptom(screen_is_not_displaying_anything),
    symptom(ram_modules_were_updated_lately),
    symptom(system_works_after_reinserting_the_ram_modules).


%%%%%% INTERNAL PREDICATES OF THE EXPERT SYSTEM. DO NOT TOUCH.

% Informs the engine that these predicates will change during execution. They are inputted by the user.
:- dynamic yes/1,no/1.

go :-
    brokenComponent(Disease),
    write_list(['The broken component probably is ',Disease,'.']),nl.

go :-
    write('I can not diagnose the broken component. Please contact a technician.'),nl.

verify(S) :-
   (yes(S) -> true ;
    (no(S) -> fail ;
     ask(S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.

start :- undo, go.

%%%%%%% Other predicates %%%%%%%%%

% Prints the elements of the list
write_list([]).
write_list([Term| Terms]) :- write(Term),
write_list(Terms).

% Predicate to read a yes/no answer
ask(Question) :-
	write_list([Question, ' (yes/y/no/n) ']),
	read_line_to_string(user_input, N),
	( (N == "yes" ; N == "y") -> assert(yes(Question)) ;
       assert(no(Question)), fail).
