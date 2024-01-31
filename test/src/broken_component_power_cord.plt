%!
%   Copyright 2024 Antonio Robirosa <ai.prolog@murak.eu>
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%

% Test all the predicates related to internationisation
%
% Run this test using the following command from the repository's root':
%
%    swipl -g run_tests -t halt test/src/broken_component_power_cord.plt
%

:- begin_tests(power_cord_is_broken).
:- consult('../../src/computer_repair_assistant.pl').

test(all_conditions_true) :-
    asserta(symptom_absent(system_turns_on)),
    asserta(symptom_absent(battery_light_turns_on)),
    asserta(symptom_present(system_works_with_another_power_cord)),
    brokenComponent(power_cord_is_broken),
    retract(symptom_absent(system_turns_on)),
    retract(symptom_absent(battery_light_turns_on)),
    retract(symptom_present(system_works_with_another_power_cord)).

test(system_turns_on, [fail]) :-
    asserta(symptom_present(system_turns_on)),
    asserta(symptom_absent(battery_light_turns_on)),
    brokenComponent(power_cord_is_broken),
    retract(symptom_present(system_turns_on)),
    retract(symptom_absent(battery_light_turns_on)).

test(battery_light_turns_on, [fail]) :-
    asserta(symptom_absent(system_turns_on)),
    asserta(symptom_present(battery_light_turns_on)),
    brokenComponent(power_cord_is_broken),
    retract(symptom_absent(system_turns_on)),
    retract(symptom_present(battery_light_turns_on)).

:- end_tests(power_cord_is_broken).
