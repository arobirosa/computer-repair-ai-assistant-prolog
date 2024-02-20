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

%% INTERNAL PREDICATES OF THE EXPERT SYSTEM. DO NOT TOUCH.
:- module(areko_internal,
	  [ start_server/0,
	    start_server_without_browser/0,
	    symptom/1,
	    is_absent/1,
	    start/0,
	    report/0,
	    test_assert_symptom_present/1,
        test_assert_symptom_absent/1,
        test_retract_all_symptoms_present/0,
        test_retract_all_symptoms_absent/0
	  ]).

% Load packs of this project
:- attach_packs(packs, [replace(true)]).
:- use_module(library(http/html_write)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(webconsole)).
:- use_module("areko_localization.pl").
:- use_module("areko_html_pages.pl").

% At startup we start the server and show the browser window
start_server :- wc_start([title("Computer Repair AI Assistant"),port(8080)]).

start_server_without_browser :- http_server(http_dispatch, [port(8080)]).

% Informs the engine that these predicates will change during execution. They are entered by the user.
% They can only be changed inside this module.
:- dynamic symptom_present/1,symptom_absent/1.

symptom(S) :- (symptom_present(S) -> true ; (symptom_absent(S) -> fail ;
        (ask_and_store_answer(S),symptom_present(S)))).
is_absent(S) :- (symptom_present(S) -> fail ; (symptom_absent(S) -> true ;
        (ask_and_store_answer(S),symptom_absent(S)))).
delete_all_symptoms :- ((retractall(symptom_present(_)), retractall(symptom_absent(_)),fail) ; true).

start :- delete_all_symptoms, diagnose, ask_what_to_do.

diagnose :-
    html_output_localized_message(diagnose_start),
    brokenComponent(Disease),
    html_output_localized_messages([diagnose_is,Disease]),nl.

diagnose :-
    html_output_localized_message(unable_to_diagnose_broken_component).


% Print all symptoms entered by the user
find_all_symptoms(AllSymptomsRows) :-
    find_all_symptom_present(PresentSymptomsRows),
    find_all_symptom_absent(AbsentSymptomsRows),
    append(PresentSymptomsRows,AbsentSymptomsRows, AllSymptomsRows).

find_all_symptom_present(SymptomsRows):-
    current_locale(Locale),
    answer_yes_label(Locale, YesLabel),
    findall((HtmlQuestion, YesLabel, Symptom), (symptom_present(Symptom), load_translation_text(Locale, Symptom, HtmlQuestion)), SymptomsRows).

find_all_symptom_absent(SymptomsRows):-
    current_locale(Locale),
    answer_no_label(Locale, NoLabel),
    findall((HtmlQuestion, NoLabel, Symptom), (symptom_absent(Symptom), load_translation_text(Locale, Symptom, HtmlQuestion)), SymptomsRows).

generate_html_symptom_rows([]) --> [].
generate_html_symptom_rows([H|T]) --> generate_html_symptom_row(H), generate_html_symptom_rows(T).

generate_html_symptom_row((HtmlQuestion, AnswerLabel, Symptom)) -->
	html(tr([td(HtmlQuestion),
		 td(AnswerLabel),
		 td(Symptom)
		])).

report :-
    find_all_symptoms(AllSymptomsRows),
    report(AllSymptomsRows),
    ask_what_to_do.

report(AllSymptomsRows) :-
    current_locale(Locale),
	question_label(Locale, QuestionLabel),
	answer_label(Locale, AnswerLabel),
	code_label(Locale, CodeLabel),
    wc_html(p(div(class("container"), div(class("row"),
        table(class("table table-warning table-striped table-hover table-bordered table-responsive"),
            [thead(
                tr([
                    th(QuestionLabel),
                    th(AnswerLabel),
                    th(CodeLabel)
                ])
            ),
            tbody(
                [\generate_html_symptom_rows(AllSymptomsRows)])]
        ))))).

%%%%%%% Other predicates %%%%%%%%%
html_output_answer(Question, YesLabel) :-
    atomic_list_concat([Question, YesLabel], " ", QuestionWithAnswer),
    wc_html(p(QuestionWithAnswer)).

% Predicate to read a yes/no answer and store the answer in the knowledge base
ask_and_store_answer(Question) :-
    print_message(debug, ask_and_store_answer(Question)),
	current_locale(Locale),
	answer_yes_label(Locale, YesLabel),
	answer_no_label(Locale, NoLabel),
    load_translation_text(Locale, Question, HtmlQuestion),
	wc_ask([ answer(Answer) ], [ p(HtmlQuestion),
	    div([class("btn-group"),role("group")],
        	        [button([name(answer), type(submit), class("btn btn-success"), value("Yes"), accesskey("y")], YesLabel),
                     button([name(answer), type(submit), class("btn btn-danger"), value("No"), accesskey("n")], NoLabel)]
        	    )]),
	( Answer == 'Yes' -> html_output_answer(HtmlQuestion, YesLabel), assert(symptom_present(Question)) ;
       html_output_answer(HtmlQuestion, NoLabel), assert(symptom_absent(Question))).

ask_what_to_do :-
	current_locale(Locale),
	answer_diagnose_label(Locale, DiagnoseLabel),
	answer_report_label(Locale, ReportLabel),
    load_translation_text(Locale, what_to_do_next, HtmlQuestion),
	wc_ask([ answer(Answer) ], [ p(HtmlQuestion),
	    div([class("btn-group"),role("group")],
	        [button([name(answer), type(submit), class("btn btn-info"), value("Diagnose"), accesskey("d")], DiagnoseLabel),
             button([name(answer), type(submit), class("btn btn-success"), value("Report"), accesskey("r")], ReportLabel)]
	    )]),
	( Answer == 'Diagnose' -> start; report).

% Predicates used for testing
test_assert_symptom_present(S) :- assert(symptom_present(S)).
test_assert_symptom_absent(S) :- assert(symptom_absent(S)).
test_retract_all_symptoms_present :- retractall(symptom_present(_)).
test_retract_all_symptoms_absent :- retractall(symptom_absent(_)).

% To inform prolog, that there are many predicates with different locales, these must be together in the same file
answer_yes_label(en, "Yes").
answer_yes_label(de, "Ja").
answer_yes_label(es, "Sí").
answer_no_label(en, "No").
answer_no_label(de, "Nein").
answer_no_label(es, "No").

question_label(en, "Question").
question_label(de, "Frage").
question_label(es, "Pregunta").
answer_label(en, "Answer").
answer_label(de, "Antwort").
answer_label(es, "Respuesta").
code_label(en, "Code").
code_label(de, "Code").
code_label(es, "Código").

answer_diagnose_label(en, "Diagnose a new case").
answer_diagnose_label(de, "Diagnostizieren einen neuen Fall").
answer_diagnose_label(es, "Diagnosticar un nuevo caso").
answer_report_label(en, "Generate report").
answer_report_label(de, "Bericht generieren").
answer_report_label(es, "Generar un informe").
