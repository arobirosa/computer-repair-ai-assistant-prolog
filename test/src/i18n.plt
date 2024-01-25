%! Test all the predicates related to internationisation
%
% Run this test using the following command from the repository's root':
%
%    swipl -g run_tests -t halt test/src/i18n.plt
%

:- begin_tests(i18n).
:- consult('../../src/computer-repair-assistant.pl').

:- multifile user:message_hook/3.
:- dynamic captured_string/1.

% We intercept the output stream
user:message_hook(Message, information, Lines) :-
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

% Predicate to test the output of print_message
/*test_print_message(Message, ExpectedOutput) :-
    with_output_to(string(CapturedOutput), print_message(information, Message)),
    ( CapturedOutput = ExpectedOutput ->
        writeln('Test passed.')
    ;
        writeln('Test failed.'),
        format('Expected "~w", but got "~w".', [ExpectedOutput, CapturedOutput])
    ).*/

/*test(defaultLocaleIsEnglish) :-
    store_locale("en"),
    current_locale(en).

test(fallbackToEnglishIfTheLocaleIsUnknown) :-
    store_locale("en"),
    current_locale(en).

test(setsSpanishLocale) :-
    store_locale("es"),
    current_locale(es).*/

:- end_tests(i18n).
