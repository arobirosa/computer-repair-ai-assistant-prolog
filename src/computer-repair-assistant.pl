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
read(RepairCase),get_single_char(Code),
brokenComponent(RepairCase,Disease),
write_list([RepairCase,', probably has ',Disease,'.']),nl.

go :-
write('Sorry, I don''t seem to be able to'),nl,
write('diagnose the disease.'),nl.

symptom(RepairCase,fever) :-  verify(RepairCase," have a fever (y/n) ?").
symptom(RepairCase,rash) :- verify(RepairCase," have a rash (y/n) ?").
symptom(RepairCase,headache) :- verify(RepairCase," have a headache (y/n) ?").
symptom(RepairCase,runny_nose) :-  verify(RepairCase," have a runny_nose (y/n) ?").
symptom(RepairCase,conjunctivitis) :- verify(RepairCase," have a conjunctivitis (y/n) ?").
symptom(RepairCase,cough) :- verify(RepairCase," have a cough (y/n) ?").
symptom(RepairCase,body_ache) :-
verify(RepairCase," have a body_ache (y/n) ?").
symptom(RepairCase,chills) :-
verify(RepairCase," have a chills (y/n) ?").
symptom(RepairCase,sore_throat) :-
verify(RepairCase," have a sore_throat (y/n) ?").
symptom(RepairCase,sneezing) :-
verify(RepairCase," have a sneezing (y/n) ?").
symptom(RepairCase,swollen_glands) :-
verify(RepairCase," have a swollen_glands (y/n) ?").

ask(RepairCase,Question) :-
	write(RepairCase),write(', do you'),write(Question),
	read(N),
	( (N == yes ; N == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail).

:- dynamic yes/1,no/1.

verify(P,S) :-
   (yes(S) -> true ;
    (no(S) -> fail ;
     ask(P,S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.


brokenComponent(RepairCase,german_measles) :-
symptom(RepairCase,fever),
symptom(RepairCase,headache),
symptom(RepairCase,runny_nose),
symptom(RepairCase,rash).

brokenComponent(RepairCase,common_cold) :-
symptom(RepairCase,headache),
symptom(RepairCase,sneezing),
symptom(RepairCase,sore_throat),
symptom(RepairCase,runny_nose),
symptom(RepairCase,chills).

brokenComponent(RepairCase,measles) :-
symptom(RepairCase,cough),
symptom(RepairCase,sneezing),
symptom(RepairCase,runny_nose).

brokenComponent(RepairCase,flu) :-
symptom(RepairCase,fever),
symptom(RepairCase,headache),
symptom(RepairCase,body_ache),
symptom(RepairCase,conjunctivitis),
symptom(RepairCase,chills),
symptom(RepairCase,sore_throat),
symptom(RepairCase,runny_nose),
symptom(RepairCase,cough).



brokenComponent(RepairCase,mumps) :-
symptom(RepairCase,fever),
symptom(RepairCase,swollen_glands).

brokenComponent(RepairCase,chicken_pox) :-
symptom(RepairCase,fever),
symptom(RepairCase,chills),
symptom(RepairCase,body_ache),
symptom(RepairCase,rash).

write_list([]).
write_list([Term| Terms]) :-
write(Term),
write_list(Terms).

response(Reply) :-
get_single_char(Code),
put_code(Code), nl,
char_code(Reply, Code).
