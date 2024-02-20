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

% This file contain all the translations in German from Germany.

:- multifile
          prolog:message//1.

prolog:message(battery_light_turns_on(de)) --> ["Schaltet sich das Batterielicht ein, nachdem der Netzschalter eingeschaltet wurde?"].
prolog:message(hard_disk_spins_on_a_second_motherboard(de)) --> ["Läuf die Festplatte, wenn sie an ein zweites Motherboard angeschlossen ist?"].
prolog:message(known_working_hard_disk_do_not_spin_on_motherboard(de)) --> ["Läuf eine Festplatte, von der bekannt ist, dass sie funktioniert, nicht auf dem Motherboard?"].
prolog:message(ram_modules_not_compatible_with_motherboard(de)) --> ["Die Geschwindigkeit und Typ der RAM-Module sind nicht im Handbuch des Motherboards dokumentiert?"].
prolog:message(ram_modules_were_updated_lately(de)) --> ["Wurde das RAM-Modul in letzter Zeit ausgetauscht?"].
prolog:message(screen_is_not_displaying_anything(de)) --> ["Ist der Bildschirm leer? Er zeigt nichts an"].
prolog:message(system_turns_on(de)) --> ["Schaltet sich das System ein?"].
prolog:message(system_works_after_reinserting_power_cable_connector(de)) --> ["Das System schaltet sich ein, nachdem der Stromstecker vom Netzteil zum Motherboard herausgenommen und wieder eingesteckt wurde, richtig?"].
prolog:message(system_has_a_power_supply_unit(de)) --> ["Verfügt das System über ein Netzteil? Laptops haben keins, aber Desktops schon."].
prolog:message(system_works_after_reinserting_the_ram_modules(de)) --> ["Nachdem Sie das RAM-Modul entfernt" ,
    " und in den Steckplatz eingesetzt haben, funktioniert das System?"].
prolog:message(system_works_on_another_outlet(de)) --> ["Funktioniert das System an einer anderen Steckdose?"].
prolog:message(system_works_with_a_new_motherboard(de)) --> ["Funktioniert der Computer mit einem neuen Motherboard?"].
prolog:message(system_works_with_a_new_power_supply_unit(de)) --> ["Funktioniert der Computer mit einem neuen Netzteil?"].
prolog:message(system_works_with_another_power_cord(de)) --> ["Funktioniert der Computer mit einem anderen Netzkabel?"].
prolog:message(system_works_with_old_modules(de)) --> ["Funktioniert der Computer mit dem/den alten RAM-Modul?"].

prolog:message(no_electricity_on_the_power_outlet(de)) --> ["Kein Strom an der Steckdose"].
prolog:message(power_cord_is_broken(de)) --> ["Das Netzkabel ist kaputt"].
prolog:message(motherboard_power_cable_is_disconnected(de)) --> ["Das Kabel, das die Motherboard mit Strom versorgt, wurde abgezogen"].
prolog:message(motherboard_is_burnt(de)) --> ["Die Motherboard ist verbrannt"].
prolog:message(power_supply_unit_is_burnt(de)) --> ["Das Netzteil ist verbrannt"].
prolog:message(incompatible_ram_modules(de)) --> ["Das RAM-Modul ist nicht mit der Hauptplatine kompatibel"].
prolog:message(disconnected_ram_modules(de)) --> ["Das RAM-Modul saß falsch auf dem Memory Slot"].

prolog:message(questions_answered_yes(de)) --> ["=== Mit ja beantwortete Fragen === ", nl].
prolog:message(questions_answered_no(de)) --> ["=== Mit nein beantwortete Fragen === ", nl].
prolog:message(questions_answered_report_end(de)) --> ["=== Ende des Berichts ===", nl].

prolog:message(answer_keys_list(de)) --> [" (ja/j/nein/n) "].
prolog:message(diagnose_is(de)) --> ["Die Diagnose ist: "].
prolog:message(unable_to_diagnose_broken_component(de)) --> ["Ich kann nicht herausfinden, welche Komponente kaputt ist. ",
    "Bitte wenden Sie sich an einen Techniker.", nl].
prolog:message(diagnose_start(de)) --> ["Fangen wir mit der Diagnose an!"].
prolog:message(what_to_do_next(de)) --> ["Was möchten Sie machen?"].
