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
prolog:message(sistema_no_enciende(es)) --> ["El sistema no se enciende, ¿es cierto?"].
prolog:message(system_turns_on(es)) --> ["¿Se enciende el sistema?"].
prolog:message(system_do_not_work_after_reinserting_power_cable_connector(es)) --> ["El sistema no se enciende después de sacar y volver a insertar el conector de alimentación de la fuente de alimentación a la placa base, ¿verdad?"].
prolog:message(system_works_after_reinserting_power_cable_connector(es)) --> ["El sistema se enciende después de sacar y volver a insertar el conector de alimentación de la PSU a la placa base, ¿verdad?"].
prolog:message(system_do_not_work_with_another_power_cord(es)) --> ["El sistema no se enciende con otro cable de alimentación, ¿verdad?"].
prolog:message(system_has_a_power_supply_unit(es)) --> ["¿El sistema tiene una fuente de alimentación (PSU)? ",
    "Los portátiles no la tienen, mientras que los ordenadores de sobremesa sí"].
prolog:message(system_works_after_reinserting_the_ram_modules(es)) --> ["Después de retirar el módulo o módulos de RAM" ,
    " e insertarlos en la ranura, ¿funciona el sistema?"].
prolog:message(system_works_on_another_outlet(es)) --> ["¿Funciona el sistema en otra toma de corriente?"].
prolog:message(system_works_with_a_new_motherboard(es)) --> ["¿Funciona el ordenador con una nueva placa base?"].
prolog:message(system_works_with_a_new_power_supply_unit(es)) --> ["¿Funciona el ordenador con una nueva fuente de alimentación?"].
prolog:message(system_works_with_another_power_cord(es)) --> ["¿Funciona el ordenador con otro cable de alimentación?"].
prolog:message(system_works_with_old_modules(es)) --> ["¿Funciona el ordenador con los módulos RAM antiguos?"].

prolog:message(answer_keys_list(es)) --> ["' (yes/y/no/n) '"]. % TODO Translate the keys
prolog:message(broken_component_is(es)) --> ["El componente roto es probablemente "].
