Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -json -name values -action confidence -language BASIC -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\confidence\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple -action confidence -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\confidence\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot -action confidence -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\confidence\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum -action confidence -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\confidence\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life -action confidence -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\confidence\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income -action confidence -language BASIC -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\confidence\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2 -action confidence -language BASIC -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\confidence\BASIC\rockt2.txt"

test\bin\run_test.ps1 -json -name diamond -action confidence -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\confidence\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test -action confidence -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\confidence\C\prime_test.txt"

test\bin\run_test.ps1 -json -name prog1 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG1.COB" -expected ".\test\ref\confidence\COBOL\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG2.COB" -expected ".\test\ref\confidence\COBOL\PROG2.txt"
test\bin\run_test.ps1 -json -name prog3 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG3.COB" -expected ".\test\ref\confidence\COBOL\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG4.COB" -expected ".\test\ref\confidence\COBOL\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG5.COB" -expected ".\test\ref\confidence\COBOL\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG6.COB" -expected ".\test\ref\confidence\COBOL\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG12-2.COB" -expected ".\test\ref\confidence\COBOL\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG13-3.COB" -expected ".\test\ref\confidence\COBOL\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG14-2.COB" -expected ".\test\ref\confidence\COBOL\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG15-4.COB" -expected ".\test\ref\confidence\COBOL\PROG15-4.txt"
test\bin\run_test.ps1 -json -name freeformat1 -action confidence -language Free-Format-COBOL -tabsize 4 -inputfile ".\test\data\COBOL\Free Format 1.COB" -expected ".\test\ref\confidence\COBOL\Free Format 1.txt"
test\bin\run_test.ps1 -json -name freeformat2 -action confidence -language Free-Format-COBOL -tabsize 4 -inputfile ".\test\data\COBOL\Free Format 2.COB" -expected ".\test\ref\confidence\COBOL\Free Format 2.txt"
test\bin\run_test.ps1 -json -name freeformat3 -action confidence -language Free-Format-COBOL -tabsize 4 -inputfile ".\test\data\COBOL\Free Format 3.COB" -expected ".\test\ref\confidence\COBOL\Free Format 3.txt"
test\bin\run_test.ps1 -json -name freeformat4 -action confidence -language Free-Format-COBOL -tabsize 4 -inputfile ".\test\data\COBOL\Free Format 4.COB" -expected ".\test\ref\confidence\COBOL\Free Format 4.txt"
test\bin\run_test.ps1 -json -name exec1 -action confidence -language Fixed-Format-COBOL -tabsize 4 -inputfile ".\test\data\COBOL\UNLDDBCU2.COB" -expected ".\test\ref\confidence\COBOL\UNLDDBCU2.txt"

test\bin\run_test.ps1 -json -name checkers -action confidence -language Cpp -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\confidence\Cpp\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb -action confidence -language Cpp -inputfile ".\test\data\Cpp\hrdb.cpp" -expected ".\test\ref\confidence\Cpp\hrdb.txt"
test\bin\run_test.ps1 -json -name date_h -action confidence -language Cpp -inputfile ".\test\data\Cpp\date.h" -expected ".\test\ref\confidence\Cpp\date_h.txt"
test\bin\run_test.ps1 -json -name date_cpp -action confidence -language Cpp -inputfile ".\test\data\Cpp\date.cpp" -expected ".\test\ref\confidence\Cpp\date_cpp.txt"

test\bin\run_test.ps1 -json -name firework -action confidence -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\confidence\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello -action confidence -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\confidence\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose -action confidence -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\confidence\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider -action confidence -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\confidence\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16 -action confidence -language Pascal -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\confidence\Pascal\TPC16.txt"

test\bin\run_test.ps1 -json -name polyglot-c -action confidence -language C -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence\C\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-cobol -action confidence -language COBOL -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence\COBOL\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action confidence -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence\Pascal\polyglot.txt"

test\bin\run_test.ps1 -json -name drone-3d -action confidence -language Python -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\confidence\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor -action confidence -language Python -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\confidence\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory -action confidence -language Python -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\confidence\Python\TrajectoryGenerator.txt"

Write-Output "Number of failures: $failures"
