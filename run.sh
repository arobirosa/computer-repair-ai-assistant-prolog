#!/usr/bin/env swipl
%
%   Computer Repair AI Assistant using Prolog
%
%   Author:        Antonio Robirosa
%   E-mail:        expert.system@go.areko.consulting
%   Website:       https://areko.consulting
%   Copyright 2024, Antonio Robirosa
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
% This PrologScrit process the arguments and starts the assistent.
:- initialization(main, main).
:- consult('src/computer_repair_assistant.pl').

opt_help(help(header), "Computer Repair AI Assistant v1.0.0").
opt_help(help(footer), [ansi(bold, '\n~w', ["Please start this script without parameters to start the diagnose of your computer on an browser."])]).
opt_help(language,"Switches the language of the application. If none is given, all questions are shown in English (en)").

opt_type(language, language, oneof([en, de, es])).

main(Argv) :-
    % Parse the command-line arguments
    argv_options(Argv, _PositionalArgs, ReceivedArguments),
    option(language(Language), ReceivedArguments, en),
    format('Locale set to: ~w~n', [Language]),
    halt.

