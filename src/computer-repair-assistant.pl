%!
% It proposes the possible cause of the malfunction by asking you the observed symptoms.
%
% Predicates
%   start   Starts a new repair case. It removes all old facts entered by the user.
%   report  Generates a report with present and absent symptoms
% TODO The following predicates must be implemented
%
%   change  Modifies the answers
%   save(repairCaseID)  Saves the case into the database
%   load(repairCaseID)  Loads the case from the database

:- discontiguous symptom/1.

%%%%%%% DEFINITION OF SYMPTOMS

symptom(A) :- check(A).
symptom(B) :- check(B).

symptom(battery_light_turns_on) :- check("Is the battery light turned on after the power button is turned on?").
symptom(hard_disk_spins_on_a_second_motherboard) :- check("Does the hard disk spin when connected to a second motherboard?").
symptom(known_working_hard_disk_do_not_spin_on_motherboard) :- check("Does a hard disk which is known to work don't spin on the motherboard?").
symptom(ram_modules_not_compatible_with_motherboard) :- check("The speed and form of the RAM modules are not documented on the manual of the motherboard?").
symptom(ram_modules_were_updated_lately) :- check("Was the RAM module changed lately?").
symptom(screen_is_not_displaying_anything) :- check("Is display blank? It doesn't display anything").
symptom(system_do_not_turn_on) :- check("The system don't turn on, right?").
symptom(system_turns_on) :- check("Does the system turn on?").
symptom(system_do_not_work_after_reinserting_power_cable_connector) :- check("The system don't turn on after taking out and reinserting the power connector from the PSU to the motherboard, right?").
symptom(system_works_after_reinserting_power_cable_connector) :- check("The system turns on after taking out and reinserting the power connector from the PSU to the motherboard, right?").
symptom(system_do_not_work_with_another_power_cord) :- check("The system don't turn on with another power cord, right?").
symptom(system_has_a_power_supply_unit) :- check("Does the system have a power supply unit? Laptops don't have one, while desktops do.").
symptom(system_works_after_reinserting_the_ram_modules) :- check("After you remove the RAM module(s)" ,
    " and insert them in the slot, does the system work?").
symptom(system_works_on_another_outlet) :- check("Does the system work in another power outlet?").
symptom(system_works_with_a_new_motherboard) :- check("Does the computer work with a new motherboard?").
symptom(system_works_with_a_new_power_supply_unit) :- check("Does the computer work with a new power supply?").
symptom(system_works_with_another_power_cord) :- check("Does the computer work with a another power cord?").
symptom(system_works_with_old_modules) :- check("Does the computer work with the old RAM module(s)?").

%%%%%%% DEFINITION OF BROKEN COMPONENT
brokenComponent(yesAyesB) :- symptom("A"), symptom("B").
brokenComponent(yesAnoB) :- symptom("A"), is_absent("B").
brokenComponent(noAyesB) :- is_absent("A"), symptom("B").
brokenComponent(noAnoB) :- is_absent("A"), is_absent("B").

brokenComponent(no_electricity_on_the_power_outlet) :-
    symptom(system_do_not_turn_on),
    is_absent(symptom(battery_light_turns_on)),
    symptom(system_works_on_another_outlet).

brokenComponent(power_cord) :-
    symptom(system_do_not_turn_on),
    is_absent(symptom(battery_light_turns_on)),
    symptom(system_works_with_another_power_cord).

brokenComponent(motherboard_power_cable_is_disconnected) :-
    symptom(system_do_not_turn_on),
    is_absent(symptom(battery_light_turns_on)),
    symptom(system_do_not_work_with_another_power_cord), % TODO Add conditions which are negated.
    symptom(system_works_after_reinserting_power_cable_connector),
    symptom(hard_disk_spins_on_a_second_motherboard).

brokenComponent(motherboard_is_burnt) :-
    symptom(system_do_not_turn_on),
    is_absent(symptom(battery_light_turns_on)),
    symptom(system_do_not_work_with_another_power_cord), % TODO Add conditions which are negated.
    symptom(system_do_not_work_after_reinserting_power_cable_connector),
    % The next line shows how two tests are possible to diagnose. It applies an OR condition
    (symptom(hard_disk_spins_on_a_second_motherboard);symptom(known_working_hard_disk_do_not_spin_on_motherboard)),
    symptom(system_works_with_a_new_motherboard).

brokenComponent(power_supply_unit_is_burnt) :-
    symptom(system_do_not_turn_on),
    is_absent(symptom(battery_light_turns_on)),
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
:- dynamic symptom_present/1,symptom_absent/1.

check(S) :- (symptom_present(S) -> true ; (symptom_absent(S) -> fail ; ask_and_store_answer(S))).
is_absent(S) :- (symptom_present(S) -> fail ; (symptom_absent(S) -> true ; ask_and_store_answer(S))).
delete_all_symptoms :- ((retractall(symptom_present(_)), retractall(symptom_absent(_)),fail) ; true).

start :- delete_all_symptoms, diagnose.

diagnose :-
    brokenComponent(Disease),
    write_all(['The broken component probably is ',Disease,'.']),nl.

diagnose :-
    write('I can not diagnose the broken component. Please contact a technician.'),nl.

% Print all symptom_present/1 predicates
print_symptom_present :-
    write('=== Questions answered with yes === '), nl,
    forall(symptom_present(Symptom), writeln(Symptom)),
    nl.
print_symptom_absent :-
    write('=== Questions answered with no ==='), nl,
    forall(symptom_absent(Symptom), writeln(Symptom)),
    nl.
report :-
    print_symptom_present,
    print_symptom_absent,
    write('=== End of report ==='), nl.

%%%%%%% Other predicates %%%%%%%%%

% Prints the elements of the list
write_all([]).
write_all([Term| Terms]) :- write(Term),
write_all(Terms).

% Predicate to read a yes/no answer and store the answer in the knowledge base
ask_and_store_answer(Question) :-
	write_all([Question, ' (yes/y/no/n) ']),
	read_line_to_string(user_input, N),
	( (N == "yes" ; N == "y") -> assert(symptom_present(Question)) ;
       assert(symptom_absent(Question)), fail).
