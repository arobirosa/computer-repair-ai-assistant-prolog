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

## Positive points
* **It is easy to understand how the assistant reached a conclusion**
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
* The strenghts of prolog are knowledge, logic and integration with other programming languages. As long as you work in the logical programming paradigm the code is elegant, easy to read and can be easily tested. If you jump into the imperative paradigm by creating a website, the resulting code for generating the HTML page is ugly, long and error prone and the internals are difficult to follow.
* The SWI Prolog implementation is mature for commercial projects. 
* The IDEs have poor support for Prolog. Intellij IDEA supports Prolog syntax highlighting using the plug-in Logtalk. The Prolog plugin is not compatible with version 2022.1.3 

# Requirements and features of the proof of concept

## Must have

:white_check_mark: Uses the prolog programming language \
:white_check_mark: The user can enter that a symptom is absent \
:white_check_mark: An user without training can use the assistant. He only needs to how to start the expert system in his language. \
:x: An user with simple steps can add new causes and symptoms :rage:  
:white_check_mark: The messages and inputs can be internationalised in different languages Prolog isn't expected to be used interacting directly with the user, so I implemented this minimally. \
:white_check_mark: The assistant can be used with a browser or works on Windows. Prolog isn't expected to be used interacting directly with the user. Please see [Integration posibilities](docs/integrationPosibilities.md) \
:white_check_mark: Automatic tests for the assistant can be written \

## Should Have
:white_check_mark: It is possible to log errors, warnings, informational and debug messages. On SWI Prolog this is done with print_message/1 as with the i18n \
- [ ] The entered symptoms can be saved. And an user can restore a previously saved session. **Possible but not implemented** \

:white_check_mark: The assistant can be integrated in a website or delivered as a separated programme. Please see [Integration posibilities](docs/integrationPosibilities.md)

- [ ] It is possible to add causes which only affect some computer models. **Possible but not implemented**

## Could have - Nice to have
- [ ] The assistant can be called from a Java programme. **Possible but not implemented** Please see [Integration posibilities].(docs/integrationPosibilities.md)

## Won't have

* Diagnostic using probability because this can be better modelled with **machine learning** and this could make the diagnostic **difficult to explain**

# Installation

On a linux box run

~~~
sudo apt-get install swi-prolog
swipl --version
~~~
You should see something similar to **SWI-Prolog version 9.0.4 for x86_64-linux**.

Go into the git repository and install all the dependencies:
~~~
swipl src/packs.pl
~~~

# Usage

On the root directory of the repository run:
~~~
swipl start.pl
~~~
You can use the parameter --language to specify the language of the messages. For example: de, es or en. The default is en.

Then browse to http://localhost:8080

## How to build and run it inside a Docker container
On the root directory of the repository run:
~~~
sudo docker build -t computer_assistant:1.0 .
sudo docker run -p 8080:8080 computer_assistant:1.0
~~~
Then browse to http://localhost:8080

# How to contribute

* As an user, you can create issues with errors or ideas for improvemets
* As a developer, you can extend the knowledge base, correct errors or make improvements. I recommend you to use Intellij IDEA as IDE, open a terminal window, run **swipl src/computer-repair-assistant.pl** and make tests in the interactive mode. You can reload the source code by entering **make.**. Please take a look at the [definition of done for each pull request](docs/Definition-of-Done.md)

# Acknowledgment

The diagnostic and symptoms were inspired from the diagrams of this [expert system created by Gena Charnukha](https://roboticsandenergy.com/projects/expert-program-computer-project/). Because I structured my expert system in another way, no code from this project was used.

# Evaluation of Prolog implementations and development environments

Please read the [results of the Evaluation of Prolog implementations and development environments here](docs/prologImplementationsEvaluation.md)

# Other projects using prolog to create (medical) expert systems:

You will find [other projects and academic papers here](docs/projectsUsingPrologToCreateExpertSystems.md)

# Contact me for inquiries about expert systems and other AI technologies

If you have a problem or process which might be solved or improved using Expert System or other AI technology, I could help you find out what tools and technologies will be useful for it.

Just send me a message to my GitHub email address. 
