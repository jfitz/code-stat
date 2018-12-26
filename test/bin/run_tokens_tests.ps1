Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -json -name simple -action tokens -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\tokens\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot -action tokens -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\tokens\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum -action tokens -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\tokens\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life -action tokens -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\tokens\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income -action tokens -language BASIC -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\tokens\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2 -action tokens -language BASIC -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\tokens\BASIC\rockt2.txt"

test\bin\run_test.ps1 -json -name diamond -action tokens -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\tokens\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test -action tokens -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\tokens\C\prime_test.txt"

test\bin\run_test.ps1 -json -name prog1 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG1.COB" -expected ".\test\ref\tokens\COBOL\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG2.COB" -expected ".\test\ref\tokens\COBOL\PROG2.txt"
test\bin\run_test.ps1 -json -name prog3 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG3.COB" -expected ".\test\ref\tokens\COBOL\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG4.COB" -expected ".\test\ref\tokens\COBOL\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG5.COB" -expected ".\test\ref\tokens\COBOL\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG6.COB" -expected ".\test\ref\tokens\COBOL\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG12-2.COB" -expected ".\test\ref\tokens\COBOL\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG13-3.COB" -expected ".\test\ref\tokens\COBOL\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG14-2.COB" -expected ".\test\ref\tokens\COBOL\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG15-4.COB" -expected ".\test\ref\tokens\COBOL\PROG15-4.txt"
test\bin\run_test.ps1 -json -name freeformat1 -action tokens -language Free-Format-COBOL -inputfile ".\test\data\COBOL\Free Format 1.COB" -tabsize 4 -expected ".\test\ref\tokens\COBOL\Free Format 1.txt"
test\bin\run_test.ps1 -json -name freeformat2 -action tokens -language Free-Format-COBOL -inputfile ".\test\data\COBOL\Free Format 2.COB" -tabsize 4 -expected ".\test\ref\tokens\COBOL\Free Format 2.txt"
test\bin\run_test.ps1 -json -name freeformat3 -action tokens -language Free-Format-COBOL -inputfile ".\test\data\COBOL\Free Format 3.COB" -tabsize 4 -expected ".\test\ref\tokens\COBOL\Free Format 3.txt"
test\bin\run_test.ps1 -json -name freeformat4 -action tokens -language Free-Format-COBOL -inputfile ".\test\data\COBOL\Free Format 4.COB" -tabsize 4 -expected ".\test\ref\tokens\COBOL\Free Format 4.txt"
test\bin\run_test.ps1 -json -name exec1 -action tokens -language Fixed-Format-COBOL -inputfile ".\test\data\COBOL\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\tokens\COBOL\UNLDDBCU2.txt"

test\bin\run_test.ps1 -json -name checkers -action tokens -language Cpp -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\tokens\Cpp\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb -action tokens -language Cpp -inputfile ".\test\data\Cpp\hrdb.cpp" -expected ".\test\ref\tokens\Cpp\hrdb.txt"
test\bin\run_test.ps1 -json -name date_h -action tokens -language Cpp -inputfile ".\test\data\Cpp\date.h" -expected ".\test\ref\tokens\Cpp\date_h.txt"
test\bin\run_test.ps1 -json -name date_cpp -action tokens -language Cpp -inputfile ".\test\data\Cpp\date.cpp" -expected ".\test\ref\tokens\Cpp\date_cpp.txt"

test\bin\run_test.ps1 -json -name firework -action tokens -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\tokens\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello -action tokens -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\tokens\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose -action tokens -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\tokens\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider -action tokens -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\tokens\Pascal\SPIDER.txt"

test\bin\run_test.ps1 -json -name polyglot-c -action tokens -language C -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\tokens\C\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-cobol -action tokens -language COBOL -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\tokens\COBOL\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action tokens -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\tokens\Pascal\polyglot.txt"

test\bin\run_test.ps1 -json -name drone-3d -action tokens -language Python -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\tokens\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor -action tokens -language Python -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\tokens\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory -action tokens -language Python -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\tokens\Python\TrajectoryGenerator.txt"

Write-Output "Number of failures: $failures"
