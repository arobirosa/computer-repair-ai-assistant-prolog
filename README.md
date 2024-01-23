# Goal
Proof of concept to evaluate the advantages and disadvantages of [knowledge representation and reasoning systems (KRR)](https://docs.google.com/document/d/1J_ASpGjDwgqMDzNgB-hqQC3keA8sIMZIHHs8gmJXG04/edit). The knowledge is represented by way of logic statements, and reasoning is done by way of the inference methods of that logic. It uses Prolog as the main programming language.

This AI Assistant will help anyone to repair their computer.

KRRs **might** be superior to Machine Learning (ML) in this use case because:
* The expert system can **explain how it got to the conclusion**. ["Explainability is particularly important in law, health care, regulation, 
  taxation, and the areas where mistakes may result in loss of life and disasters", from the ErgoAI FAQ](https://docs.google.com/document/d/1J_ASpGjDwgqMDzNgB-hqQC3keA8sIMZIHHs8gmJXG04/edit)
* The knowledge base is incremental
* The reasoning has high-precision when no probabilities are used
* Does not depend on a large number of examples. This means that it is not affected by training bias

Under the condition that it is **easy to represent the domain knowledge** interviewing experts in the aread or reading textbooks, regulations, 
contracts and other documents.

[Source: ErgoAI FAQ](https://docs.google.com/document/d/1J_ASpGjDwgqMDzNgB-hqQC3keA8sIMZIHHs8gmJXG04/edit)

# Conclusions reached after developing the assistant

TODO Complete this section

## Positive points
* Easy to understand how the assistant reached a conclusion
* The knowledge base is incremental
* The reasoning has high-precision when no probabilities are used

## Negative points
* There aren't any prolog programmers in the market. Prolog is more difficult to understand as a language than object-oriented or imperative languages like Java or Python
* Most prolog distributions don't seem to be actively used in commercial projects


## Neutral points

* It is difficult for an domain expert to add new diagnoses on his/her own. For a fast repair, the symptoms which are easy to check must be written first on the prolog programm. 
* Intellij IDEA supports Prolog syntax highlighting using the plug-in Logtalk. The Prolog plugin is not compatible with version 2022.1.3 

# Requirements

## Must have

* Uses the prolog programming language
- [ ] An user without training can use the assistant
- [ ] An user with simple steps can add new causes and symptoms
- [ ] The messages and inputs can be internationalised in different languages
- [ ] The assistant works on Windows or can be used with a browser
- [ ] Automatic tests for the assistant can be written

## Should Have
- [ ] The entered symptoms can be saved. And an user can restored a previously saved session
- [ ] The assistant can be integrated in a website or delivered as a separated programme
- [ ] It is possible to add causes which only affect some computer models

## Could have - Nice to have
- [ ] The assistant can be called from a Java programme

## Won't have

* Diagnostic using probability because this can be better modelled with **machine learning** and this could make the diagnostic **difficult to explain**

# Installation

On a linux box run

~~~
sudo apt-get install swi-prolog
~~~

# Usage

~~~
swipl src/computer-repair-assistant.pl
~~~

# Acknowledgment

This project is based on medical expert system developed by * [Shiddika Jahan Bushra](https://github.com/sjbushra/Medical-Diagnosis-system-using-Prolog/)

The diagnostic and symptoms were inspired from the diagrams of this [expert system created by Gena Charnukha](https://roboticsandenergy.com/projects/expert-program-computer-project/). Because I structured my expert system in another way, no code from this project was used.

# Evaluation of Prolog development environments

The following were evaluated for use in this project:

* [SWI-Prolog](https://www.swi-prolog.org/) **Choosen**. It can link Prolog predicates to an SQL database, Java and Python. It can work on both Windows and Linux-based systems.

[Table with all implementations of Prolog](https://en.wikipedia.org/wiki/Comparison_of_Prolog_implementations)

## Discarded

* [ErgoAI](https://github.com/ErgoAI) Documentation is poor. Discarded. It can link Prolog predicates to an SQL database, Java and Python
* [Visual Prolog](https://www.visual-prolog.com/) Discarded. It only works on Windows and its syntax is propietary.
* [XSB: a deductive database](https://xsb.sourceforge.net/) It is used by ErgoAI internally.
* [GNU Prolog](https://www.gprolog.org/) It can't use databases to get predicates
* [ECLiPSe is an open-source software system for the cost-effective development and deployment of constraint programming applications, e.g. in the 
  areas of planning, scheduling, resource allocation, timetabling, transport etc.](http://www.eclipseclp.org/) Better for constraint based problems
* [Jprolog](https://github.com/jiprolog/jiprolog/releases) The website is outdated and the last change was on 2015.

# Other projects using prolog to create (medical expert systems):

* [Medical Diagnosis system using Prolog created by Shiddika Jahan Bushra](https://github.com/sjbushra/Medical-Diagnosis-system-using-Prolog/)
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
