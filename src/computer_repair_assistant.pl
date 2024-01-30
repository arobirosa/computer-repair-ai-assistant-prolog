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

:- consult('internal.pl').
:- consult('localization.pl').
:- consult('translations_en.pl').
:- consult('translations_es.pl').
:- consult('translations_de.pl').


%%%%%%% DEFINITION OF REPAIR ISSUES

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
