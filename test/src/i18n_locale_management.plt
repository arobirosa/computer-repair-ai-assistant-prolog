%! Test all the predicates related to internationisation
%
% Run this test using the following command from the repository's root':
%
%    swipl -g run_tests -t halt test/src/i18n_locale_management.plt
%

:- begin_tests(i18n).
:- consult('../../src/computer-repair-assistant.pl').

test(defaultLocaleIsEnglish) :-
    store_locale(en),
    current_locale(en), !.

test(fallbackToEnglishIfTheLocaleIsUnknown) :-
    store_locale(en),
    current_locale(en), !.

test(setsSpanishLocale) :-
    store_locale(es),
    current_locale(es), !.

:- end_tests(i18n).
