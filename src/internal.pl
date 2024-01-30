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

%% INTERNAL PREDICATES OF THE EXPERT SYSTEM. DO NOT TOUCH.

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
