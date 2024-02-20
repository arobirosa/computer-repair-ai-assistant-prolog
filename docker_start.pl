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
% This prolog script is the entry point to for the docker image
:- initialization(main, main).
:- consult('src/computer_repair_assistant.pl').

main :-
    areko_localization:store_locale(en),
    areko_internal:start_server,
    start.

