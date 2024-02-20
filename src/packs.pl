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
:- module(packs, []).
:- use_module(library(prolog_pack)).
:- attach_packs(packs, [replace(true)]).

:- initialization(install_or_upgrade, main).

% pack_install_local is not available on SWI-Prolog version 9.0.4
pack(webconsole, [version('0.9.1')]).

install_or_upgrade :-
    % pack_install_local is not available on SWI-Prolog version 9.0.4
    pack_install(webconsole, [interactive(false), upgrade(true), global(false), package_directory(packs)]).
