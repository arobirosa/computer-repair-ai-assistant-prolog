%!
% This file contain all the translations in Spanish.

:- multifile
        prolog:message//1.

prolog:message(battery_light_turns_on(es)) --> ["¿Se enciende la luz de la batería después de presionar el botón de encendido?"].
prolog:message(hard_disk_spins_on_a_second_motherboard(es)) --> ["¿Gira el disco duro cuando se conecta a una segunda placa madre?"].
prolog:message(known_working_hard_disk_do_not_spin_on_motherboard(es)) --> ["¿Un disco duro que se sabe que funciona no gira con esa placa madre?"].
prolog:message(ram_modules_not_compatible_with_motherboard(es)) --> ["¿La velocidad y la forma de los módulos RAM no están documentadas en el manual de la placa base?"].
prolog:message(ram_modules_were_updated_lately(es)) --> ["¿Se ha cambiado el módulo RAM recientemente?"].
prolog:message(screen_is_not_displaying_anything(es)) --> ["¿La pantalla está en blanco? No muestra nada"].
prolog:message(system_turns_on(es)) --> ["¿Se enciende el sistema?"].
prolog:message(system_works_after_reinserting_power_cable_connector(es)) --> ["El sistema se enciende después de sacar y volver a insertar el conector de alimentación de la PSU a la placa base, ¿verdad?"].
prolog:message(system_has_a_power_supply_unit(es)) --> ["¿El sistema tiene una fuente de alimentación (PSU)? ",
    "Los portátiles no la tienen, mientras que los ordenadores de sobremesa sí"].
prolog:message(system_works_after_reinserting_the_ram_modules(es)) --> ["Después de retirar el módulo o módulos de RAM" ,
    " e insertarlos en la ranura, ¿funciona el sistema?"].
prolog:message(system_works_on_another_outlet(es)) --> ["¿Funciona el sistema en otra toma de corriente?"].
prolog:message(system_works_with_a_new_motherboard(es)) --> ["¿Funciona el ordenador con una nueva placa base?"].
prolog:message(system_works_with_a_new_power_supply_unit(es)) --> ["¿Funciona el ordenador con una nueva fuente de alimentación?"].
prolog:message(system_works_with_another_power_cord(es)) --> ["¿Funciona el ordenador con otro cable de alimentación?"].
prolog:message(system_works_with_old_modules(es)) --> ["¿Funciona el ordenador con los módulos RAM antiguos?"].

prolog:message(no_electricity_on_the_power_outlet(es)) --> ["No hay electricidad en el tomacorriente"].
prolog:message(power_cord_is_broken(es)) --> ["El cable de alimentación está roto"].
prolog:message(motherboard_power_cable_is_disconnected(es)) --> ["El cable que da electricidad a la placa base se ha desconectado"].
prolog:message(motherboard_is_burnt(es)) --> ["La placa base está quemada"].
prolog:message(power_supply_unit_is_burnt(es)) --> ["La fuente de alimentación está quemada"].
prolog:message(incompatible_ram_modules(es)) --> ["El módulo RAM es incompatible con la placa base"].
prolog:message(disconnected_ram_modules(es)) --> ["El módulo RAM se ha desconectado de la placa base"].

prolog:message(questions_answered_yes(es)) --> ["=== Preguntas contestadas con sí === ", nl].
prolog:message(questions_answered_no(es)) --> ["=== Preguntas contestadas con no === ", nl].
prolog:message(questions_answered_report_end(es)) --> ["=== Fin del informe ===", nl].

prolog:message(answer_keys_list(es)) --> [" (yes/y/no/n) "]. % TODO Translate the keys
prolog:message(diagnose_is(es)) --> ["La causa del problema es: "].
prolog:message(unable_to_diagnose_broken_component(es)) --> ["No puedo diagnosticar el problema. ", "Póngase en contacto con un técnico.", nl].
