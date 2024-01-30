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
%    swipl -g run_tests -t halt test/src/i18n_localized_messages.plt
%

:- begin_tests(i18n).
:- consult('../../src/computer_repair_assistant.pl').

:- multifile user:message_hook/3.
:- dynamic captured_string/1.

% We intercept the output stream
user:message_hook(_Message, information, Lines) :-
            format('Message intercepted: ~w~n', [Lines]),
            with_output_to(string(String), write(Lines)),
            assert(captured_string(String)),
            true. % Continue with the default printing

test(translationsInEnglishAreLoad) :-
    retractall(captured_string(_)),
    print_localized_message(battery_light_turns_on, en),
    (captured_string("[Is the battery light turned on after the power button is turned on?]") -> true;
        (forall(captured_string(Symptom), (write("Test failed. Captured: '"), write(Symptom), writeln("'")))), fail).

test(translationsInEnglishFallbackToTheCode) :-
    retractall(captured_string(_)),
    print_localized_message(unknown_translation_code, en),
    (captured_string("[Unknown message: ~p-[unknown_translation_code(en)]]") -> true;
        (forall(captured_string(Symptom), (write("Test failed. Captured: '"), write(Symptom), writeln("'")))), fail).

:- end_tests(i18n).
