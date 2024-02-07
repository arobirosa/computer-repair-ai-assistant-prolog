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
:- module(localization,
	  [ print_localized_message//1,
	    print_localized_message//2,
	    store_locale//1,
	    html_output_localized_message//1,
	    html_output_localized_message//2,
	    load_html_translations//3,
	    to_html_paragraphs//2,
        print_localized_all//1
	  ]).

:- use_module('translations_de.pl').
:- use_module('translations_en.pl').
:- use_module('translations_es.pl').

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
   load_html_translations(Key, Locale, HtmlParagraphs),
   wc_html(HtmlParagraphs).

load_html_translations(Key, Locale, HtmlParagraphs) :-
     MessageTerm =.. [Key, Locale],
     message_translation(MessageTerm, TranslationList),
     to_html_paragraphs(TranslationList, HtmlParagraphs).

to_html_paragraphs([], []):- !.
to_html_paragraphs([String|OtherStrings], [Parragraph|OtherParragraphs]):-
    Parragraph = p(String),
    to_html_paragraphs(OtherStrings, OtherParragraphs).

% Prints the elements of the list
print_localized_all([]).
print_localized_all([Term| Terms]) :- print_localized_message(Term),
print_localized_all(Terms).
