Here I list the mature alternatives which I found to integrate the prolog program with another system. Expert system run in the backend and the user usually interacts with a web or GUI application.

# HTTP Services

You can expose your Prolog logic as an HTTP service using the built-in HTTP libraries in SWI-Prolog.

## Web application: SWI-Prolog already includes a Web framework
* [Webconsole: Pack to show a prolog console on a browser](https://www.swi-prolog.org/pack/list?p=webconsole) Priority to test: 1
* [Tutorial on how to use SWI-Prolog's web framework](https://github.com/Anniepoo/swiplwebtut/blob/master/web.adoc)
* [simple-template: Templating for web applications](https://github.com/rla/simple-template)
* [simple_web: Microframework for building websites](https://www.swi-prolog.org/pack/list?p=simple_web)
* [weblog: Another web application framework](https://www.swi-prolog.org/pack/list?p=weblog)

## Javascript or Prolog endpoints
* [Pengines: Web Logic Programming Made Easy](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/ext/pengines/pengines.pl) Priority to test: 2

## JSON endpoints
* [The pack openapi can generate prolog predicates from an OPENAPI definition](https://www.swi-prolog.org/pack/list?p=openapi) Priority to test: 3
* [HTTP JSON Plugin module](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/ext/http/http/http_json.pl)

## Endpoint with XML

I didn't find any pack (library) to create one.

# Messaging brokers
* [slack_prolog provides a Prolog interface to Slack](https://www.swi-prolog.org/pack/list?p=slack_prolog)
* [MQTT allows to integrate using this messaging protocol](https://www.swi-prolog.org/pack/list?p=mqtt)

# WebSockets

SWI-Prolog supports websockets but they are usually for real-time communication. This is particularly useful for web applications that require a persistent connection to the server for real-time updates.

# Inter-process Communication (IPC)

It is possible to use sockets, shared memory, or even files for communication between your Prolog program and other applications but I don't have experience with IPC.

# Prolog Embedded in Other Languages

Popular languages have modules which integrate Prolog code like Drools for Java. I have experience extending code using Drools and Drools is difficult to debug, error-prone, there is no logging. At the end, this type of integration leads to an black box module which Java developers cannot understand.

