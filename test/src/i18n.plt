%! Test all the predicates related to internationisation
%
% Run this test using the following command from the repository's root':
%
%    swipl -g run_tests -t halt test/src/i18n.plt
%

:- begin_tests(i18n).
:- consult('../../src/computer-repair-assistant.pl').

test(translationsInEnglishAreLoad) :-
    print_localized_message(battery_light_turns_on, en).

test(translationsInEnglishFallbackToTheCode) :-
    print_localized_message(unknown_translation_code, en).

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
