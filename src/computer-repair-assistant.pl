%!
% It proposes the possible cause of the malfunction by asking you the observed symptoms.
%
% Predicates
%   start   Starts a new repair case. It removes all old facts entered by the user.
%
% TODO The following predicates must be implemented
%   report  Generates a report
%   change  Modifies the answers
%   save(repairCaseID)  Saves the case into the database
%   load(repairCaseID)  Loads the case from the database

%%%%%%% DEFINITION OF SYMPTOMS

symptom(fever) :-  verify(" have a fever?").
symptom(rash) :- verify(" have a rash?").
symptom(headache) :- verify(" have a headache?").
symptom(runny_nose) :-  verify(" have a runny_nose?").
symptom(conjunctivitis) :- verify(" have a conjunctivitis?").
symptom(cough) :- verify(" have a cough?").
symptom(body_ache) :- verify(" have a body_ache?").
symptom(chills) :- verify(" have a chills?").
symptom(sore_throat) :- verify(" have a sore_throat?").
symptom(sneezing) :- verify(" have a sneezing?").
symptom(swollen_glands) :- verify(" have a swollen_glands?").

%%%%%%% DEFINITION OF BROKEN COMPONENT

brokenComponent(german_measles) :-
    symptom(fever),
    symptom(headache),
    symptom(runny_nose),
    symptom(rash).

brokenComponent(common_cold) :-
    symptom(headache),
    symptom(sneezing),
    symptom(sore_throat),
    symptom(runny_nose),
    symptom(chills).

brokenComponent(measles) :-
    symptom(cough),
    symptom(sneezing),
    symptom(runny_nose).

brokenComponent(flu) :-
    symptom(fever),
    symptom(headache),
    symptom(body_ache),
    symptom(conjunctivitis),
    symptom(chills),
    symptom(sore_throat),
    symptom(runny_nose),
    symptom(cough).

brokenComponent(mumps) :-
    symptom(fever),
    symptom(swollen_glands).

brokenComponent(chicken_pox) :-
    symptom(fever),
    symptom(chills),
    symptom(body_ache),
    symptom(rash).
    
brokenComponent(power_issue) :-
    symptom(no_power),
    symptom(strange_noise),
    symptom(no_screen).
brokenComponent(hardware_failure) :-
    symptom(slow_performance),
    symptom(overheating),
    symptom(unexpected_shutdown).
brokenComponent(virus_infection) :-
    symptom(popups),
    symptom(slow_internet),
    symptom(files_missing).
brokenComponent(os_issue) :-
    symptom(blue_screen),
    symptom(software_crashes),
    symptom(slow_boot).


%%%%%% INTERNAL PREDICATES OF THE EXPERT SYSTEM. DO NOT TOUCH.

% Informs the engine that these predicates will change during execution. They are inputted by the user.
:- dynamic yes/1,no/1.

go :-
    brokenComponent(Disease),
    write_list(['The broken component probably is ',Disease,'.']),nl.

go :-
    write('I can not diagnose the broken component. Please contact a technician.'),nl.

verify(S) :-
   (yes(S) -> true ;
    (no(S) -> fail ;
     ask(S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.

start :- undo, go.

%%%%%%% Other predicates %%%%%%%%%

% Prints the elements of the list
write_list([]).
write_list([Term| Terms]) :- write(Term),
write_list(Terms).

% Predicate to read a yes/no answer
ask(Question) :-
	write_list([Question, ' (yes/y/no/n) ']),
	read_line_to_string(user_input, N),
	( (N == 'yes' ; N == 'y') -> assert(yes(Question)) ;
       assert(no(Question)), fail).
