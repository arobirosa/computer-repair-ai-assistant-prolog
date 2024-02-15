%!
%   Copyright 2024 Antonio Robirosa <expert.system@go.areko.consulting>
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
:- consult('../../src/areko_internal.pl').

:- dynamic symptom_present/1,symptom_absent/1.

brokenComponent(power_cord_is_broken) :-
    is_absent(system_turns_on),
    is_absent(battery_light_turns_on),
    symptom(system_works_with_another_power_cord).

:- begin_tests(absent_present_symptoms).

test(all_conditions_true) :-
    test_retract_all_symptoms_present,
    test_retract_all_symptoms_absent,
    test_assert_symptom_absent(system_turns_on),
    test_assert_symptom_absent(battery_light_turns_on),
    test_assert_symptom_present(system_works_with_another_power_cord),
    brokenComponent(power_cord_is_broken).

test(one_symptom_present) :-
    test_retract_all_symptoms_present,
    test_assert_symptom_present(system_works_with_another_power_cord),
    symptom(system_works_with_another_power_cord).

test(one_symptom_absent) :-
    test_retract_all_symptoms_present,
    test_retract_all_symptoms_absent,
    test_assert_symptom_absent(system_turns_on),
    is_absent(system_turns_on),
    test_retract_all_symptoms_absent.

test(system_turns_on, [fail]) :-
    test_retract_all_symptoms_present,
    test_retract_all_symptoms_absent,
    test_assert_symptom_present(system_turns_on),
    test_assert_symptom_absent(battery_light_turns_on),
    brokenComponent(power_cord_is_broken).

test(battery_light_turns_on, [fail]) :-
    test_assert_symptom_absent(system_turns_on),
    test_assert_symptom_present(battery_light_turns_on),
    brokenComponent(power_cord_is_broken),
    retract(symptom_absent(system_turns_on)),
    retract(symptom_present(battery_light_turns_on)).

:- end_tests(absent_present_symptoms).
