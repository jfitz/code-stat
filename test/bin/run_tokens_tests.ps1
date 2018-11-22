Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -name simple -action tokens -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\tokens\BASIC\simple.txt"
test\bin\run_test.ps1 -name 3dplot -action tokens -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\tokens\BASIC\3dplot.txt"
test\bin\run_test.ps1 -name batnum -action tokens -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\tokens\BASIC\batnum.txt"
test\bin\run_test.ps1 -name life -action tokens -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\tokens\BASIC\life.txt"
test\bin\run_test.ps1 -name diamond -action tokens -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\tokens\C\diamond.txt"
test\bin\run_test.ps1 -name prime_test -action tokens -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\tokens\C\prime_test.txt"
test\bin\run_test.ps1 -name prog1 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG1.COB" -expected ".\test\ref\tokens\COBOL\PROG1.txt"
test\bin\run_test.ps1 -name prog2 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG2.COB" -expected ".\test\ref\tokens\COBOL\PROG2.txt"
test\bin\run_test.ps1 -name prog3 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG3.COB" -expected ".\test\ref\tokens\COBOL\PROG3.txt"
test\bin\run_test.ps1 -name prog4 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG4.COB" -expected ".\test\ref\tokens\COBOL\PROG4.txt"
test\bin\run_test.ps1 -name prog5 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG5.COB" -expected ".\test\ref\tokens\COBOL\PROG5.txt"
test\bin\run_test.ps1 -name prog6 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG6.COB" -expected ".\test\ref\tokens\COBOL\PROG6.txt"
test\bin\run_test.ps1 -name prog12-2 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG12-2.COB" -expected ".\test\ref\tokens\COBOL\PROG12-2.txt"
test\bin\run_test.ps1 -name prog13-3 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG13-3.COB" -expected ".\test\ref\tokens\COBOL\PROG13-3.txt"
test\bin\run_test.ps1 -name prog14-2 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG14-2.COB" -expected ".\test\ref\tokens\COBOL\PROG14-2.txt"
test\bin\run_test.ps1 -name prog15-4 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG15-4.COB" -expected ".\test\ref\tokens\COBOL\PROG15-4.txt"
test\bin\run_test.ps1 -name checkers -action tokens -language Cpp -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\tokens\Cpp\checkers.txt"
test\bin\run_test.ps1 -name firework -action tokens -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\tokens\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -name hello -action tokens -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\tokens\Pascal\HELLO.txt"
test\bin\run_test.ps1 -name rose -action tokens -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\tokens\Pascal\ROSE.txt"
test\bin\run_test.ps1 -name spider -action tokens -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\tokens\Pascal\SPIDER.txt"

Write-Output "Number of failures: $failures"
