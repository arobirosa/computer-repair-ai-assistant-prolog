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

:- consult('translations_en.pl').
:- consult('translations_es.pl').
:- consult('translations_de.pl').

%%%%%%% DEFINITION OF SYMPTOMS

%%%%%%% DEFINITION OF BROKEN COMPONENT

brokenComponent(no_electricity_on_the_power_outlet) :-
    is_absent(system_turns_on),
    is_absent(battery_light_turns_on),
    symptom(system_works_on_another_outlet).

brokenComponent(power_cord_is_broken) :-
    is_absent(system_turns_on),
    is_absent(battery_light_turns_on),
    symptom(system_works_with_another_power_cord).

brokenComponent(motherboard_power_cable_is_disconnected) :-
    is_absent(system_turns_on),
    is_absent(battery_light_turns_on),
    is_absent(system_works_with_another_power_cord),
    symptom(system_works_after_reinserting_power_cable_connector),
    symptom(hard_disk_spins_on_a_second_motherboard).

brokenComponent(motherboard_is_burnt) :-
    is_absent(system_turns_on),
    is_absent(battery_light_turns_on),
    is_absent(system_works_with_another_power_cord),
    is_absent(system_works_after_reinserting_power_cable_connector),
    % The next line shows how two tests are possible to diagnose. It applies an OR condition
    (symptom(hard_disk_spins_on_a_second_motherboard);symptom(known_working_hard_disk_do_not_spin_on_motherboard)),
    symptom(system_works_with_a_new_motherboard).

brokenComponent(power_supply_unit_is_burnt) :-
    is_absent(system_turns_on),
    is_absent(battery_light_turns_on),
    is_absent(system_works_with_another_power_cord),
    symptom(system_has_a_power_supply_unit),
    is_absent(system_works_after_reinserting_power_cable_connector),
    symptom(system_works_with_a_new_power_supply_unit).

brokenComponent(incompatible_ram_modules) :-
    symptom(system_turns_on),
    symptom(battery_light_turns_on),
    symptom(screen_is_not_displaying_anything),
    symptom(ram_modules_were_updated_lately),
    symptom(ram_modules_not_compatible_with_motherboard),
    symptom(system_works_with_old_modules).

brokenComponent(disconnected_ram_modules) :-
    symptom(system_turns_on),
    symptom(battery_light_turns_on),
    symptom(screen_is_not_displaying_anything),
    symptom(ram_modules_were_updated_lately),
    symptom(system_works_after_reinserting_the_ram_modules).


%%%%%% INTERNAL PREDICATES OF THE EXPERT SYSTEM. DO NOT TOUCH.

% Informs the engine that these predicates will change during execution. They are inputted by the user.
:- dynamic symptom_present/1,symptom_absent/1.

symptom(S) :- (symptom_present(S) -> true ; (symptom_absent(S) -> fail ; ask_and_store_answer(S))).
is_absent(S) :- (symptom_present(S) -> fail ; (symptom_absent(S) -> true ; ask_and_store_answer(S))).
delete_all_symptoms :- ((retractall(symptom_present(_)), retractall(symptom_absent(_)),fail) ; true).

start :- start("en"). % The default locale is English
start(Locale) :- store_locale(Locale), delete_all_symptoms, diagnose.

diagnose :-
    brokenComponent(Disease),
    print_localized_all([diagnose_is,Disease]),nl.

diagnose :-
    print_localized_message(unable_to_diagnose_broken_component).

% Print all symptom_present/1 predicates
print_symptom_present :-
    print_localized_message(questions_answered_yes),
    forall(symptom_present(Symptom), print_localized_message(Symptom)),
    nl.
print_symptom_absent :-
    print_localized_message(questions_answered_no),
    forall(symptom_absent(Symptom), print_localized_message(Symptom)),
    nl.
report :-
    print_symptom_present,
    print_symptom_absent,
    print_localized_message(questions_answered_report_end).

%%%%%%% Other predicates %%%%%%%%%
print_localized_message(Key) :-
    current_locale(Locale),
    print_localized_message(Key, Locale).

print_localized_message(Key, Locale) :-
    MessageTerm =.. [Key, Locale],
    print_message(information, MessageTerm).

% Store the current locale. If it isn't supported, fallback to English.
:- dynamic current_locale/1.
store_locale(Locale) :- (member(Locale, [en,es,de]) ; fail),
    retractall(current_locale(_)), assert(current_locale(Locale)).
store_locale(_Locale) :- store_locale(en).

% Prints the elements of the list
print_localized_all([]).
print_localized_all([Term| Terms]) :- print_localized_message(Term),
print_localized_all(Terms).

% Predicate to read a yes/no answer and store the answer in the knowledge base
ask_and_store_answer(Question) :-
	print_localized_all([Question, answer_keys_list]),
	read_line_to_string(user_input, N),
	current_locale(Locale),
	answer_yes_possibilities(Locale, ValidAnswers),
	( member(N, ValidAnswers) -> assert(symptom_present(Question)) ;
       assert(symptom_absent(Question)), fail).

% To inform prolog, that there are many predicates with different locales, these must be together in the same file
answer_yes_possibilities(en, [ "yes", "y", "Yes", "Y"]).
answer_yes_possibilities(de, ["ja", "Ja", "j", "J"]).
answer_yes_possibilities(es, [ "si", "s", "Si", "Sí", "sí"]).
