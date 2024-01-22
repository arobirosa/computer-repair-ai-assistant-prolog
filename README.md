# Goal
Proof of concept to evaluate the advantages and disadvantages of knowledge representation and reasoning (KRR). It uses Prolog as the main programming language.

This AI Assistant will help anyone learning to repair their computer.

# Conclusions reached after developing the assistant

## Positive points
* Easy to understand how the assistant reached a conclusion
* 

## Negative points
* There aren't any prolog programmers in the market. Prolog is more difficult to understand as a language than object-oriented or imperative languages like Java or Python
* ost prolog distributions don't seem to be actively used in commercial projects

## Neutral points

# Requirements

## Must have

- [ ] Uses the prolog programming language
- [ ] An user without training can use the assistant
- [ ] An user with simple steps can add new causes and symptoms
- [ ] The messages and inputs can be internationalised in different languages
- [ ] The assistant works on Windows
- [ ] Automatic tests for the assistant can be written

## Should Have
- [ ] The entered symptoms can be saved. And an user can restored a previously saved session
- [ ] The assistant can be integrated in a website or delivered as a separated programme
- [ ] It is possible to add causes which only affect some computer models

## Could have - Nice to have
- [ ] The assistant can be called from a Java programme

## Won't have

None

# Installation

On a linux box run

~~~
sudo apt-get install swi-prolog
~~~

# Prolog development environments

The following were evaluated for use in this project:

* [SWI-Prolog](https://www.swi-prolog.org/) **Choosen**. It can link Prolog predicates to an SQL database, Java and Python. It can work on both Windows and Linux-based systems.

## Discarded

* [ErgoAI](https://github.com/ErgoAI) Documentation is poor. Discarded. It can link Prolog predicates to an SQL database, Java and Python
* [Visual Prolog](https://www.visual-prolog.com/) Discarded. It only works on Windows and its syntax is propietary.
* [XSB: a deductive database](https://xsb.sourceforge.net/) It is used by ErgoAI internally.
* [GNU Prolog](https://www.gprolog.org/) It can't use databases to get predicates
* [ECLiPSe is an open-source software system for the cost-effective development and deployment of constraint programming applications, e.g. in the 
  areas of planning, scheduling, resource allocation, timetabling, transport etc.](http://www.eclipseclp.org/) Better for constraint based problems
* [Jprolog](https://github.com/jiprolog/jiprolog/releases) The website is outdated and the last change was on 2015.



--------





The next sections must be organized

# Why using prolog or other knowledge representation and reasoning (KRR) tool?

## What is knowledge representation and reasoning (KRR)?
Knowledge representation and reasoning is a branch of Artificial Intelligence (AI) in which decision-making is done by way of reasoning from knowledge about the domain of discourse and general background knowledge. The knowledge is represented by way of logic statements, and reasoning is done by way of the inference methods of that logic. Different KRR approaches mainly differ in their underlying logics and the reasoning methods they support. [from ErgoAI Faq](https://docs.google.com/document/d/1J_ASpGjDwgqMDzNgB-hqQC3keA8sIMZIHHs8gmJXG04/edit)

## What is rule-based KRR? 
Rule-based KRR uses only facts and rules of the form IF X THEN Y (and some more complex ones) to represent knowledge, trading some expressivity for scalability.  Despite the syntactic restrictions, this form of KRR is highly expressive, and its scalability makes it practical. [from ErgoAI Faq](https://docs.google.com/document/d/1J_ASpGjDwgqMDzNgB-hqQC3keA8sIMZIHHs8gmJXG04/edit)


## What is KRR good for?
KRR is best suited for decision support in cases where domain knowledge can be elicited directly from subject matter experts (SMEs) with reasonable effort or from knowledge represented in text, such as regulations, contracts, textbooks, and other documents.   This knowledge is then codified using logical statements. In these situations, KRR is far superior to Machine Learning (ML) because it is explainable, incremental, has high-precision, does not depend on a large number of examples, and is not subject to spoofing or training bias.  Explainability is particularly important in law, health care, regulation, taxation, and the areas where mistakes may result in loss of life and disasters. [from ErgoAI Faq](https://docs.google.com/document/d/1J_ASpGjDwgqMDzNgB-hqQC3keA8sIMZIHHs8gmJXG04/edit)




# How to test a prolog programme?

https://stackoverflow.com/questions/8294703/faster-way-of-testing-your-prolog-program

# Prolog code fixers

TODO Check if they work

https://codepal.ai/code-fixer/prolog
https://codingfleet.com/code-assistant/prolog/

# Other projects
[Medical Diagnosis system using Prolog](https://github.com/sjbushra/Medical-Diagnosis-system-using-Prolog/tree/master)
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
