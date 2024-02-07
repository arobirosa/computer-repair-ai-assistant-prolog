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

% This file contain all the translations in American English. This language will be used when the current locale don't have any translations.
% All these messages must be translated to the locales supported by the assistant.
:- module(translations_en,
	  [ message_translation/2]).

:- multifile
        prolog:message//1, message_translation//2.

:- discontiguous message_translation/2.

message_translation(battery_light_turns_on(en),["Is the battery light turned on after the power button is turned on?"]).
message_translation(hard_disk_spins_on_a_second_motherboard(en),["Does the hard disk spin when connected to a second motherboard?"]).
message_translation(known_working_hard_disk_do_not_spin_on_motherboard(en),["Does a hard disk which is known to work don't spin on the motherboard?"]).
message_translation(ram_modules_not_compatible_with_motherboard(en),["The speed and form of the RAM modules are not documented on the manual of the motherboard?"]).
message_translation(ram_modules_were_updated_lately(en),["Was the RAM module changed lately?"]).
message_translation(screen_is_not_displaying_anything(en),["Is display blank? It doesn't display anything"]).
message_translation(system_turns_on(en),["Does the system turn on?"]).
message_translation(system_works_after_reinserting_power_cable_connector(en),["The system turns on after taking out and reinserting the power connector from the PSU to the motherboard, right?"]).
message_translation(system_has_a_power_supply_unit(en),["Does the system have a power supply unit? Laptops don't have one, while desktops do."]).
message_translation(system_works_after_reinserting_the_ram_modules(en),["After you remove the RAM module(s)" ,
    " and insert them in the slot, does the system work?"]).
message_translation(system_works_on_another_outlet(en),["Does the system work in another power outlet?"]).
message_translation(system_works_with_a_new_motherboard(en),["Does the computer work with a new motherboard?"]).
message_translation(system_works_with_a_new_power_supply_unit(en),["Does the computer work with a new power supply?"]).
message_translation(system_works_with_another_power_cord(en),["Does the computer work with a another power cord?"]).
message_translation(system_works_with_old_modules(en),["Does the computer work with the old RAM module(s)?"]).

message_translation(no_electricity_on_the_power_outlet(en),["No electricity on the power outlet"]).
message_translation(power_cord_is_broken(en),["The power cord is broken"]).
message_translation(motherboard_power_cable_is_disconnected(en),["The cable which gives electricity to the motherboard was disconnected"]).
message_translation(motherboard_is_burnt(en),["The motherboard is burnt"]).
message_translation(power_supply_unit_is_burnt(en),["The power supply unit is burnt"]).
message_translation(incompatible_ram_modules(en),["The RAM module is incompatible with the motherboard"]).
message_translation(disconnected_ram_modules(en),["The RAM module was disconnected from the motherboard"]).

prolog:message(questions_answered_yes(en)) --> ["=== Questions answered with yes === ", nl].
prolog:message(questions_answered_no(en)) --> ["=== Questions answered with no === ", nl].
prolog:message(questions_answered_report_end(en)) --> ["=== End of report ===", nl].

message_translation(diagnose_is(en),["The issue is: "]).
message_translation(unable_to_diagnose_broken_component(en),["I can not diagnose the broken component. Please contact a technician."]).
