%!
% It proposes the possible cause of the malfunction by asking you the observed symptoms.
%
% Predicates
%   start   Starts a new repair case. It removes all old facts entered by the user.
%   report  Generates a report
%   change  Modifies the answers

% Definition of symptoms

% Definition of repair causes

% Internal predicates of the expert system. Do not touch.


go :-
write('What is repair case ID? '),
read_line_to_string(user_input, RepairCase),
brokenComponent(Disease),
write_list([RepairCase,', probably has ',Disease,'.']),nl.

go :-
write('Sorry, I don''t seem to be able to'),nl,
write('diagnose the disease.'),nl.

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

:- dynamic yes/1,no/1.

verify(S) :-
   (yes(S) -> true ;
    (no(S) -> fail ;
     ask(S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.


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

write_list([]).
write_list([Term| Terms]) :-
write(Term),
write_list(Terms).

response(Reply) :-
get_single_char(Code),
put_code(Code), nl,
char_code(Reply, Code).

% Other predicates

% Predicate to read a yes/no answer
ask(Question) :-
	write_list([Question, " (yes/y/no/n) "]),
	read_line_to_string(user_input, N),
	( (N == "yes" ; N == "y") -> assert(yes(Question)) ;
       assert(no(Question)), fail).
