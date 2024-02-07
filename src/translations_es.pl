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

% This file contain all the translations in Spanish.
:- module(translations_es,
	  [ message_translation/2]).

:- multifile
        prolog:message//1, message_translation//2.

:- discontiguous message_translation/2.

message_translation(battery_light_turns_on(es),["¿Se enciende la luz de la batería después de presionar el botón de encendido?"]).
message_translation(hard_disk_spins_on_a_second_motherboard(es),["¿Gira el disco duro cuando se conecta a una segunda placa madre?"]).
message_translation(known_working_hard_disk_do_not_spin_on_motherboard(es),["¿Un disco duro que se sabe que funciona no gira con esa placa madre?"]).
message_translation(ram_modules_not_compatible_with_motherboard(es),["¿La velocidad y la forma de los módulos RAM no están documentadas en el manual de la placa base?"]).
message_translation(ram_modules_were_updated_lately(es),["¿Se ha cambiado el módulo RAM recientemente?"]).
message_translation(screen_is_not_displaying_anything(es),["¿La pantalla está en blanco? No muestra nada"]).
message_translation(system_turns_on(es),["¿Se enciende el sistema?"]).
message_translation(system_works_after_reinserting_power_cable_connector(es),["El sistema se enciende después de sacar y volver a insertar el conector de alimentación de la PSU a la placa base, ¿verdad?"]).
message_translation(system_has_a_power_supply_unit(es),["¿El sistema tiene una fuente de alimentación (PSU)? ",
    "Los portátiles no la tienen, mientras que los ordenadores de sobremesa sí"]).
message_translation(system_works_after_reinserting_the_ram_modules(es),["Después de retirar el módulo o módulos de RAM" ,
    " e insertarlos en la ranura, ¿funciona el sistema?"]).
message_translation(system_works_on_another_outlet(es),["¿Funciona el sistema en otra toma de corriente?"]).
message_translation(system_works_with_a_new_motherboard(es),["¿Funciona el ordenador con una nueva placa base?"]).
message_translation(system_works_with_a_new_power_supply_unit(es),["¿Funciona el ordenador con una nueva fuente de alimentación?"]).
message_translation(system_works_with_another_power_cord(es),["¿Funciona el ordenador con otro cable de alimentación?"]).
message_translation(system_works_with_old_modules(es),["¿Funciona el ordenador con los módulos RAM antiguos?"]).

message_translation(no_electricity_on_the_power_outlet(es),["No hay electricidad en el tomacorriente"]).
message_translation(power_cord_is_broken(es),["El cable de alimentación está roto"]).
message_translation(motherboard_power_cable_is_disconnected(es),["El cable que da electricidad a la placa base se ha desconectado"]).
message_translation(motherboard_is_burnt(es),["La placa base está quemada"]).
message_translation(power_supply_unit_is_burnt(es),["La fuente de alimentación está quemada"]).
message_translation(incompatible_ram_modules(es),["El módulo RAM es incompatible con la placa base"]).
message_translation(disconnected_ram_modules(es),["El módulo RAM se ha desconectado de la placa base"]).

prolog:message(questions_answered_yes(es)) --> ["=== Preguntas contestadas con sí === ", nl].
prolog:message(questions_answered_no(es)) --> ["=== Preguntas contestadas con no === ", nl].
prolog:message(questions_answered_report_end(es)) --> ["=== Fin del informe ===", nl].

message_translation(diagnose_is(es),["La causa del problema es: "]).
message_translation(unable_to_diagnose_broken_component(es),["No puedo diagnosticar el problema. ", "Póngase en contacto con un técnico."]).
