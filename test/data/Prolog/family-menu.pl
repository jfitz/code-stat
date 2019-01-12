run_opt(1) :- print_males.

run_opt(2) :- print_females.

run_opt(3) :- 
    write(    'Enter a query.'), nl,
    read(Query),
    print_query_true(Query),
    print_query_false(Query).

run_opt(4) :- write('Goodbye'), nl, halt. 

run_opt(_) :- write('Invalid option'), nl, halt.  
