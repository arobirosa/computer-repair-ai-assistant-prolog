# Goal
Proof of concept to evaluate the advantages and disadvantages of [knowledge representation and reasoning systems (KRR)](https://docs.google.com/document/d/1J_ASpGjDwgqMDzNgB-hqQC3keA8sIMZIHHs8gmJXG04/edit). The knowledge is represented by way of logic statements, and reasoning is done by way of the inference methods of that logic. It uses Prolog as the main programming language.

This AI Assistant will help anyone to repair their computer.

KRRs **might** be superior to Machine Learning (ML) in this use case because:
* The expert system can **explain how it got to the conclusion**. ["Explainability is particularly important in law, health care, regulation, 
  taxation, and the areas where mistakes may result in loss of life and disasters", from the ErgoAI FAQ](https://docs.google.com/document/d/1J_ASpGjDwgqMDzNgB-hqQC3keA8sIMZIHHs8gmJXG04/edit)
* The knowledge base is incremental
* The reasoning has high-precision when no probabilities are used
* Does not depend on a large number of examples. This means that it is not affected by training bias

Under the condition that it is **easy to represent the domain knowledge** interviewing experts in the area or reading textbooks, regulations, 
contracts and other documents.

[Source: ErgoAI FAQ](https://docs.google.com/document/d/1J_ASpGjDwgqMDzNgB-hqQC3keA8sIMZIHHs8gmJXG04/edit)

# Conclusions reached after developing the assistant

TODO Complete this section

## Positive points
* **Easy to understand how the assistant reached a conclusion**
* **The knowledge base is incremental**. The customer can start with a few cases and add new ones as soon as they appear in the real world.
* The reasoning has high-precision when no probabilities are used
* The [graphical debugger](https://www.swi-prolog.org/pldoc/man?section=start-guitracer) is useful and pretty easy to use, once you have set the breakpoints

## Negative points
* **The expert in the field must be assisted by a programmer to enter new knowledge into the system**. It is difficult for a domain expert to add new diagnoses on his/her own.** On db/symptomsAndIssues.sqlite you will find a database with new repair issues and symptoms. Using a programme like [DB Browser for SQLite](https://sqlitebrowser.org/) for adding new knowledge is slow and error-prone, the same as writing Prolog code directly. I was hoping to find an easy way to add knowledge and this would have been an advantage of expert systems which don't use statistical methods
* How the prolog engine works is difficult to understand. Small changes like using single quotes for strings will create atoms which behave different from string and the following code will never be true:

~~~
% Predicate to read a yes/no answer and store the answer in the knowledge base
ask_and_store_answer(Question) :-
	write_all([Question, ' (yes/y/no/n) ']),
	read_line_to_string(user_input, N),
	( (N == 'yes' ; N == 'y') -> assert(symptom_present(Question)) ;
       assert(symptom_absent(Question)), fail).
~~~

* The stability of some free packs (plug-ins) like [prosqlite](http://stoics.org.uk/~nicos/sware/prosqlite) to access SQLite databases is poor. After one day, every time I tried to open a connection, there was a segmentation fault in the C code associated with the pack:
~~~
Running on :date(2024,1,31)

SWI-Prolog [thread 1 (main) at Wed Jan 31 09:57:28 2024]: received fatal signal 11 (segv)
C-stack trace labeled "crash":
  [0] PL_advance_hash_table_enum() at ??:? [0x7f928dda0a9f]
  [1] Sdprintf() at ??:? [0x7f928dda351d]
  [2] PL_scan_options() at ??:? [0x7f928dda4d5e]
  [3] __sigaction() at ??:? [0x7f928da3c460]
~~~
I can open the uniprot database without issues in Intellij IDEA and [DB Browser for SQLite](https://sqlitebrowser.org/)
* There aren't any prolog programmers in the market. Prolog is more difficult to understand as a language than object-oriented or imperative languages like Java or Python

## Neutral points
* The SWI Prolog implementation is mature for commercial projects. 
* The IDEs have poor support for Prolog. Intellij IDEA supports Prolog syntax highlighting using the plug-in Logtalk. The Prolog plugin is not compatible with version 2022.1.3 

# Requirements and features of the proof of concept

## Must have

:white_check_mark: Uses the prolog programming language \
:white_check_mark: The user can enter that a symptom is absent \
:white_check_mark: An user without training can use the assistant. He only needs to how to start the expert system in his language. \
:x: An user with simple steps can add new causes and symptoms :rage:  
:white_check_mark: The messages and inputs can be internationalised in different languages Prolog isn't expected to be used interacting directly with the user, so I implemented this minimally. \
- [ ] The assistant works on Windows or can be used with a browser Prolog isn't expected to be used interacting directly with the user
:white_check_mark: Automatic tests for the assistant can be written \

## Should Have
:white_check_mark: It is possible to log errors, warnings, informational and debug messages. On SWI Prolog this is done with print_message/1 as with the i18n \
- [ ] The entered symptoms can be saved. And an user can restored a previously saved session
- [ ] The assistant can be integrated in a website or delivered as a separated programme
- [ ] It is possible to add causes which only affect some computer models

## Could have - Nice to have
- [ ] The assistant can be called from a Java programme

## Won't have

* Diagnostic using probability because this can be better modelled with **machine learning** and this could make the diagnostic **difficult to explain**

## TODO Tasks

Try the pack [instant_prolog_docs](https://www.swi-prolog.org/pack/list?p=instant_prolog_docs) to see if it documents predicates better than AI code assistant which work for any programming language.

# Installation

On a linux box run

~~~
sudo apt-get install swi-prolog
~~~

# Usage

~~~
swipl src/computer-repair-assistant.pl
~~~

# How to contribute

* As an user, you can create issues with errors or ideas for improvemets
* As a developer, you can extend the knowledge base, correct errors or make improvements. I recommend you to use Intellij IDEA as IDE, open a terminal window, run **swipl src/computer-repair-assistant.pl** and make tests in the interactive mode. You can reload the source code by entering **make.**.

# Acknowledgment

The diagnostic and symptoms were inspired from the diagrams of this [expert system created by Gena Charnukha](https://roboticsandenergy.com/projects/expert-program-computer-project/). Because I structured my expert system in another way, no code from this project was used.

# Evaluation of Prolog implementations and development environments

The following ones were evaluated for use in this project:

* [SWI-Prolog](https://www.swi-prolog.org/) **Choosen**. It can link Prolog predicates to an SQL database, Java and Python. It can work on both Windows and Linux-based systems. It can be used to create Web Applications
* [Prolog Development Tool - PDT](Prolog Development Tool - PDT) **Choosen** It has a console to execute any predicate while working on a prolog script. This is better than the support provided by Intellij IDEA with the Logtalk plugin.

[Table with all implementations of Prolog](https://en.wikipedia.org/wiki/Comparison_of_Prolog_implementations)

### Discarded

* [ErgoAI](https://github.com/ErgoAI) Documentation is poor. Discarded. It can link Prolog predicates to an SQL database, Java and Python
* [Visual Prolog](https://www.visual-prolog.com/) Discarded. It only works on Windows and its syntax is propietary.
* [XSB: a deductive database](https://xsb.sourceforge.net/) It is used by ErgoAI internally.
* [GNU Prolog](https://www.gprolog.org/) It can't use databases to get predicates
* [ECLiPSe is an open-source software system for the cost-effective development and deployment of constraint programming applications, e.g. in the areas of planning, scheduling, resource allocation, timetabling, transport etc.](http://www.eclipseclp.org/) Better for constraint based problems
* [Jprolog](https://github.com/jiprolog/jiprolog/releases) The website is outdated and the last change was on 2015.

# Other projects using prolog to create (medical) expert systems:

* [Medical Diagnosis system using Prolog created by Shiddika Jahan Bushra](https://github.com/sjbushra/Medical-Diagnosis-system-using-Prolog/) This is a clean way to create an expert system. Please be aware that the license of the code is unclear so it is better not to reuse that code.
* [NephroDoctor created by Nicola Dileo and Tommaso Viterbo](https://github.com/nicoladileo/NephroDoctor) Shows how to make diagnostic using  probability, modify the answers and save the symptoms to a file
* [Medical Expert System for COVID-19 from Rojay White and others](https://github.com/R-White-0/Medical-Expert-System) It uses 3 or more symptoms 
  to diagnostic COVID-19. It shows how to use windows, messages boxes and checkboxes to ask for user imput with Prolog. Notice: Works locally but not on the online version of SWI Prolog.



--------

TODO The next sections must be organized


https://www.swi-prolog.org/pldoc/man?predicate=style_check/1
https://www.gyaanibuddy.com/assignments/assignment-detail/medical-diagnosis-in-prolog/
https://swish.swi-prolog.org/example/movies.pl
https://www.swi-prolog.org/download/stable
https://www.swi-prolog.org/pldoc/man?section=implhistory

# Prolog code fixers

TODO Check if they work

https://codepal.ai/code-fixer/prolog
https://codingfleet.com/code-assistant/prolog/

# Other projects
[Medical diagnosis in prolog](https://www.gyaanibuddy.com/assignments/assignment-detail/medical-diagnosis-in-prolog/)
[Expert program computer project of Robotics and Energy](https://roboticsandenergy.com/projects/expert-program-computer-project/)
[SHEPHERD: Deep learning for diagnosing patients with rare genetic diseases](https://github.com/mims-harvard/SHEPHERD)
[Proof-of-concept (POC) of a medical diagnosis system using a probabilistic expert system, which is based on Noisy-or Bayesian Network Model.](https://github.com/hidiryuzuguzel/medical-expert-system-poc/tree/master)

# Papers

https://arxiv.org/pdf/2110.04439.pdf
https://www.semanticscholar.org/paper/PROLOG-EXPERT-SYSTEM%3A-THE-DIAGNOSIS-OF-KIDNEY-Roventa-Ro%C8%99u/349a7cc2ccbe9ab0ab562467eaaf259d46ab7e3e
file:///home/arobirosa/Downloads/450-Article%20Text-1399-1-10-20150224.pdf
https://dl.acm.org/doi/abs/10.1145/3487664.3487728
https://www.jstage.jst.go.jp/article/pjsai/JSAI2021/0/JSAI2021_4N3IS1b01/_pdf
https://scholarworks.calstate.edu/downloads/hd76s295t
