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
print_localized_message(Key) :-
    current_locale(Locale),
    print_localized_message(Key, Locale).

print_localized_message(Key, Locale) :-
    MessageTerm =.. [Key, Locale],
    print_message(information, MessageTerm).

% Store the current locale. If it isn't supported, fallback to English.
:- dynamic current_locale/1.
store_locale(Locale) :- (member(Locale, [en,es,de]) ; fail),
    retractall(current_locale(_)), assert(current_locale(Locale)).
store_locale(_Locale) :- store_locale(en).

% Prints the elements of the list
print_localized_all([]).
print_localized_all([Term| Terms]) :- print_localized_message(Term),
print_localized_all(Terms).
