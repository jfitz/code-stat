write_male(X) :- write(X), write(' is male.'), nl.
print_males :- forall(male(X), write_male(X)).

write_female(X) :- write(X), write(' is female.'), nl.
print_females :- forall(female(X), write_female(X)).
