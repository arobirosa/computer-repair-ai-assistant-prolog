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
%    swipl -g run_tests -t halt test/src/i18n_locale_management.plt
%
:- use_module('../../src/areko_localization.pl').

:- begin_tests(i18n).

test(defaultLocaleIsEnglish) :-
    store_locale(en),
    current_locale(en).

test(fallbackToEnglishIfTheLocaleIsUnknown) :-
    store_locale(dk),
    current_locale(en).

test(setsSpanishLocale) :-
    store_locale(es),
    current_locale(es).

:- end_tests(i18n).
