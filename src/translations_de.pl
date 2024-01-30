%!
% This file contain all the translations in German from Germany.

:- multifile
        prolog:message//1.

prolog:message(battery_light_turns_on(de)) --> ["Schaltet sich das Batterielicht ein, nachdem der Netzschalter eingeschaltet wurde?"].
prolog:message(hard_disk_spins_on_a_second_motherboard(en)) --> ["Läuf die Festplatte, wenn sie an ein zweites Motherboard angeschlossen ist?"].
prolog:message(known_working_hard_disk_do_not_spin_on_motherboard(de)) --> ["Läuf eine Festplatte, von der bekannt ist, dass sie funktioniert, nicht auf dem Motherboard?"].
prolog:message(ram_modules_not_compatible_with_motherboard(en)) --> ["Die Geschwindigkeit und Typ der RAM-Module sind nicht im Handbuch des Motherboards dokumentiert?"].
prolog:message(ram_modules_were_updated_lately(en)) --> ["Wurde das RAM-Modul in letzter Zeit ausgetauscht?"].
prolog:message(screen_is_not_displaying_anything(de)) --> ["Ist der Bildschirm leer? Er zeigt nichts an"].
prolog:message(system_do_not_turn_on(de)) --> ["Das System schaltet sich nicht ein, richtig?"].
prolog:message(system_turns_on(de)) --> ["Schaltet sich das System ein?"].
prolog:message(system_do_not_work_after_reinserting_power_cable_connector(en)) --> ["Das System schaltet sich nicht ein, nachdem der Stromanschluss vom Netzteil zum Motherboard herausgenommen und wieder eingesteckt wurde, richtig?"].
prolog:message(system_works_after_reinserting_power_cable_connector(en)) --> ["Das System schaltet sich ein, nachdem der Stromstecker vom Netzteil zum Motherboard herausgenommen und wieder eingesteckt wurde, richtig?"].
prolog:message(system_do_not_work_with_another_power_cord(en)) --> ["Das System schaltet sich nicht mit einem anderen Netzkabel ein, richtig?"].
prolog:message(system_has_a_power_supply_unit(de)) --> ["Verfügt das System über ein Netzteil? Laptops haben keins, aber Desktops schon."].
prolog:message(system_works_after_reinserting_the_ram_modules(de)) --> ["Nachdem Sie das/die RAM-Modul(e) entfernt" ,
    " entfernt und in den Steckplatz eingesetzt haben, funktioniert das System?"].
prolog:message(system_works_on_another_outlet(de)) --> ["Funktioniert das System an einer anderen Steckdose?"].
prolog:message(system_works_with_a_new_motherboard(de)) --> ["Funktioniert der Computer mit einem neuen Motherboard?"].
prolog:message(system_works_with_a_new_power_supply_unit(de)) --> ["Funktioniert der Computer mit einem neuen Netzteil?"].
prolog:message(system_works_with_another_power_cord(de)) --> ["Funktioniert der Computer mit einem anderen Netzkabel?"].
prolog:message(system_works_with_old_modules(de)) --> ["Funktioniert der Computer mit dem/den alten RAM-Modul(en)?"].

prolog:message(no_electricity_on_the_power_outlet(de)) --> ["Kein Strom an der Steckdose"].
prolog:message(power_cord_is_broken(de)) --> ["Das Netzkabel ist kaputt"].
prolog:message(motherboard_power_cable_is_disconnected(de)) --> ["Das Kabel, das die Motherboard mit Strom versorgt, wurde abgezogen"].
prolog:message(motherboard_is_burnt(de)) --> ["Die Motherboard ist verbrannt"].
prolog:message(power_supply_unit_is_burnt(de)) --> ["Das Netzteil ist verbrannt"].
prolog:message(incompatible_ram_modules(de)) --> ["Das RAM-Modul ist nicht mit der Hauptplatine kompatibel"].
prolog:message(disconnected_ram_modules(de)) --> ["Das RAM-Modul saß falsch auf dem Memory Slot"].

prolog:message(questions_answered_yes(en)) --> ["=== Mit ja beantwortete Fragen === ", nl].
prolog:message(questions_answered_no(en)) --> ["=== Mit nein beantwortete Fragen === ", nl].
prolog:message(questions_answered_report_end(en)) --> ["=== Ende des Berichts ===", nl].

prolog:message(answer_keys_list(de)) --> [" (yes/y/no/n) "]. % TODO Translate the keys
prolog:message(diagnose_is(en)) --> ["Die Diagnose ist: "].
prolog:message(unable_to_diagnose_broken_component(en)) --> ["Ich kann nicht herausfinden, welche Komponente kaputt ist. ",
    "Bitte wenden Sie sich an einen Techniker.", nl].