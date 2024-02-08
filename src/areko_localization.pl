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
:- module(areko_localization,
	  [ print_localized_message/1,
	    print_localized_message/2,
	    store_locale/1,
	    html_output_localized_message/1,
	    load_translation_text/3,
	    html_output_localized_messages/1,
	    current_locale/1
	  ]).
:- use_module(library(http/html_write)).
:- reexport(library(webconsole)).
:- consult('translations_en.pl').
:- consult('translations_es.pl').
:- consult('translations_de.pl').

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

html_output_localized_message(Key) :-
    current_locale(Locale),
    html_output_localized_message(Key, Locale).

html_output_localized_message(Key, Locale) :-
   load_translation_text(Locale, Key, Translation),
   wc_html(p(Translation)).

load_translation_text(Locale, Key, Translation) :-
     MessageTerm =.. [Key, Locale],
     message_to_string(MessageTerm, Translation).

load_translation_texts(Locale, [FirstTerm | Terms], [FirstTranslation | TranslatedTerms]) :-
    load_translation_text(Locale, FirstTerm, FirstTranslation),
    load_translation_texts(Locale, Terms, TranslatedTerms).
load_translation_texts(_Locale, [], []) :- !.

html_output_localized_messages([]).
html_output_localized_messages(Keys) :-
    current_locale(Locale),
    load_translation_texts(Locale, Keys, TranslatedTerms),
    atomic_list_concat(TranslatedTerms, "", MergedTranslation),
    wc_html(p(MergedTranslation)).


