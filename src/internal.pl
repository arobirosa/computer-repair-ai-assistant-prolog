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

:- use_module(library(webconsole)).

% At startup we start the server and show the browser window
:- store_locale(en), wc_start([title("Computer Repair AI Asisstant"),port(8080)]).

% Informs the engine that these predicates will change during execution. They are inputted by the user.
:- dynamic symptom_present/1,symptom_absent/1.

symptom(S) :- (symptom_present(S) -> true ; (symptom_absent(S) -> fail ; ask_and_store_answer(S))).
is_absent(S) :- (symptom_present(S) -> fail ; (symptom_absent(S) -> true ; ask_and_store_answer(S))).
delete_all_symptoms :- ((retractall(symptom_present(_)), retractall(symptom_absent(_)),fail) ; true).

start :- start(en). % The default locale is English
start(Locale) :- store_locale(Locale),
    delete_all_symptoms, diagnose.

diagnose :-
    html_output_localized_message(diagnose_start),
    brokenComponent(Disease),
    html_output_localized_messages([diagnose_is,Disease]),nl.

diagnose :-
    html_output_localized_message(unable_to_diagnose_broken_component).

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
	current_locale(Locale),
	answer_yes_label(Locale, YesLabel),
	answer_no_label(Locale, NoLabel),
    load_translation_text(Locale, Question, HtmlQuestion),
	wc_ask([ answer(Answer) ], [ p(HtmlQuestion),
	    button([name(answer), type(submit), value("Yes"), accesskey("y")], YesLabel),
        button([name(answer), type(submit), value("No"), accesskey("n")], NoLabel)
                                                                    ]),
	( Answer == "Yes" -> assert(symptom_present(Question)) ;
       assert(symptom_absent(Question)), fail).

% To inform prolog, that there are many predicates with different locales, these must be together in the same file
answer_yes_label(en, "Yes").
answer_yes_label(de, "Ja").
answer_yes_label(es, "Sí").
answer_no_label(en, "No").
answer_no_label(de, "Nein").
answer_no_label(es, "No").
