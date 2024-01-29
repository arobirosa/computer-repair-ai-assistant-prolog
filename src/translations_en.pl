%!
% This file contain all the translations in American English. This language will be used when the current locale don't have any translations.
% All these messages must be translated to the locales supported by the assistant.

:- multifile
        prolog:message//1.


prolog:message(battery_light_turns_on(en)) --> ["Is the battery light turned on after the power button is turned on?"].
prolog:message(hard_disk_spins_on_a_second_motherboard(en)) --> ["Does the hard disk spin when connected to a second motherboard?"].
prolog:message(known_working_hard_disk_do_not_spin_on_motherboard(en)) --> ["Does a hard disk which is known to work don't spin on the motherboard?"].
prolog:message(ram_modules_not_compatible_with_motherboard(en)) --> ["The speed and form of the RAM modules are not documented on the manual of the motherboard?"].
prolog:message(ram_modules_were_updated_lately(en)) --> ["Was the RAM module changed lately?"].
prolog:message(screen_is_not_displaying_anything(en)) --> ["Is display blank? It doesn't display anything"].
prolog:message(system_do_not_turn_on(en)) --> ["The system don't turn on, right?"].
prolog:message(system_turns_on(en)) --> ["Does the system turn on?"].
prolog:message(system_do_not_work_after_reinserting_power_cable_connector(en)) --> ["The system don't turn on after taking out and reinserting the power connector from the PSU to the motherboard, right?"].
prolog:message(system_works_after_reinserting_power_cable_connector(en)) --> ["The system turns on after taking out and reinserting the power connector from the PSU to the motherboard, right?"].
prolog:message(system_do_not_work_with_another_power_cord(en)) --> ["The system don't turn on with another power cord, right?"].
prolog:message(system_has_a_power_supply_unit(en)) --> ["Does the system have a power supply unit? Laptops don't have one, while desktops do."].
prolog:message(system_works_after_reinserting_the_ram_modules(en)) --> ["After you remove the RAM module(s)" ,
    " and insert them in the slot, does the system work?"].
prolog:message(system_works_on_another_outlet(en)) --> ["Does the system work in another power outlet?"].
prolog:message(system_works_with_a_new_motherboard(en)) --> ["Does the computer work with a new motherboard?"].
prolog:message(system_works_with_a_new_power_supply_unit(en)) --> ["Does the computer work with a new power supply?"].
prolog:message(system_works_with_another_power_cord(en)) --> ["Does the computer work with a another power cord?"].
prolog:message(system_works_with_old_modules(en)) --> ["Does the computer work with the old RAM module(s)?"].

prolog:message(answer_keys_list(en)) --> [" (yes/y/no/n) "].
prolog:message(broken_component_is(en)) --> ["The broken component probably is "].
