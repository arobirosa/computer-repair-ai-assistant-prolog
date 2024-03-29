%!
%   Copyright 2024 Antonio Robirosa <expert.system@go.areko.consulting>
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%

%% PREDICATES RELATED TO THE CREATION OF THE HTML PAGES
:- module(areko_html_pages,
	  [
	    start_server/0
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(webconsole)).

% At startup we start the server
start_server :- http_server(http_dispatch, [port(8080)]).

% Location of the CSS files
:- multifile http:location/3.
:- dynamic   http:location/3.

:- html_resource(css('naturedesign.css'),
		 [ requires('https://cdnjs.cloudflare.com/ajax/libs/bootstrap/5.3.2/css/bootstrap.min.css')
		 ]).
:- html_resource(naturedesignresources,
		 [ virtual(true),
		   requires(css('naturedesign.css')),
		   requires(css('webconsole.css'))
		 ]).

% Serve the CSS file. I didn't find a working way to serve CSS from two different folders
:- http_handler('/css/naturedesign.css', http_reply_file('css/naturedesign.css', []), []).
:- http_handler('/js/webconsole.js', http_reply_file('js/webconsole_lazy_load.js', []), []).

:- http_handler(root(.),
                http_redirect(moved_temporary, location_by_id(wc_home)),
                []).

% Overwrite the home page

:- http_handler(webconsole('wc_home'),    wc_home_nature_design,    [priority(1)]).

wc_home_nature_design(_Request) :-
	% Don't use check the IP with wc_allowed(Request)
	% The title will be always the same
	reply_html_page([title('Computer Repair AI Assistant'),meta([name("viewport"),content("width=device-width, initial-scale=1")])],
			[ \html_requires(naturedesignresources),
            			  h1([class('text-center mb-4')], 'Computer Repair AI Assistant'),
            			  div([class('container')],[
            			    \wc_error_area,
                            \wc_output_area([]),
                            div([class('form-group')], \wc_form_area([]))
            			  ])
            			]).

