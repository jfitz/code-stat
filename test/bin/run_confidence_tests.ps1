Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -name simple -action confidence -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\confidence\BASIC\simple.txt"
test\bin\run_test.ps1 -name diamond -action confidence -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\confidence\C\diamond.txt"
test\bin\run_test.ps1 -name prime_test -action confidence -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\confidence\C\prime_test.txt"
test\bin\run_test.ps1 -name prog1 -action confidence -language COBOL -inputfile ".\test\data\COBOL\PROG1.COB" -expected ".\test\ref\confidence\COBOL\PROG1.txt"
test\bin\run_test.ps1 -name checkers -action confidence -language Cpp -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\confidence\Cpp\checkers.txt"
test\bin\run_test.ps1 -name firework -action confidence -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\confidence\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -name hello -action confidence -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\tokens\Pascal\HELLO.txt"
test\bin\run_test.ps1 -name rose -action confidence -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\tokens\Pascal\ROSE.txt"
test\bin\run_test.ps1 -name spider -action confidence -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\tokens\Pascal\SPIDER.txt"

Write-Output "Number of failures: $failures"
