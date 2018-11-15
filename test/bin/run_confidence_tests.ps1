Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -name simple -action confidence -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\confidence\BASIC\simple.txt"
test\bin\run_test.ps1 -name diamond -action confidence -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\confidence\C\diamond.txt"
test\bin\run_test.ps1 -name prime_test -action confidence -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\confidence\C\prime_test.txt"
test\bin\run_test.ps1 -name prog1 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG1.COB" -expected ".\test\ref\confidence\COBOL\PROG1.txt"
test\bin\run_test.ps1 -name prog2 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG2.COB" -expected ".\test\ref\confidence\COBOL\PROG2.txt"
test\bin\run_test.ps1 -name prog3 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG3.COB" -expected ".\test\ref\confidence\COBOL\PROG3.txt"
test\bin\run_test.ps1 -name prog4 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG4.COB" -expected ".\test\ref\confidence\COBOL\PROG4.txt"
test\bin\run_test.ps1 -name prog5 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG5.COB" -expected ".\test\ref\confidence\COBOL\PROG5.txt"
test\bin\run_test.ps1 -name prog6 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG6.COB" -expected ".\test\ref\confidence\COBOL\PROG6.txt"
test\bin\run_test.ps1 -name prog12-2 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG12-2.COB" -expected ".\test\ref\confidence\COBOL\PROG12-2.txt"
test\bin\run_test.ps1 -name prog13-3 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG13-3.COB" -expected ".\test\ref\confidence\COBOL\PROG13-3.txt"
test\bin\run_test.ps1 -name prog14-2 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG14-2.COB" -expected ".\test\ref\confidence\COBOL\PROG14-2.txt"
test\bin\run_test.ps1 -name prog15-4 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG15-4.COB" -expected ".\test\ref\confidence\COBOL\PROG15-4.txt"
test\bin\run_test.ps1 -name checkers -action confidence -language Cpp -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\confidence\Cpp\checkers.txt"
test\bin\run_test.ps1 -name firework -action confidence -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\confidence\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -name hello -action confidence -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\confidence\Pascal\HELLO.txt"
test\bin\run_test.ps1 -name rose -action confidence -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\confidence\Pascal\ROSE.txt"
test\bin\run_test.ps1 -name spider -action confidence -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\confidence\Pascal\SPIDER.txt"

Write-Output "Number of failures: $failures"
