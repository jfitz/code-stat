:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).	 % new
:- use_module(library(uri)).			         % new

:- http_handler(root(.),      list_modules, []). % /
:- http_handler(root(module), list_module,  []). % /module?name=<module>

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

%%	list_modules(+Request)
%
%	Create a table of all available modules with their source-file

list_modules(_Request) :-
        findall(M, current_module(M), List),
        sort(List, Modules),
        reply_html_page(title('Loaded Prolog modules'),
                        [ h1('Loaded Prolog modules'),
                          table([ \header
                                | \modules(Modules)
                                ])
                        ]).

header -->
        html(tr([th('Module'), th('File')])).

modules([]) -->	[].
modules([H|T]) -->
        html(tr([td(\module_link(H)), td(\module_file(H))])),
        modules(T).

module_file(H) -->
        { module_property(H, file(Path)) }, !,
        html(Path).
module_file(_) -->
        html(-).