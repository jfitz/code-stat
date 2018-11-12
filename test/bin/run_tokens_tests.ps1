Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -name simple -action tokens -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\tokens\BASIC\simple.txt"
test\bin\run_test.ps1 -name diamond -action tokens -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\tokens\C\diamond.txt"
test\bin\run_test.ps1 -name prime_test -action tokens -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\tokens\C\prime_test.txt"
test\bin\run_test.ps1 -name prog1 -action tokens -language COBOL -inputfile ".\test\data\COBOL\PROG1.COB" -expected ".\test\ref\tokens\COBOL\PROG1.txt"
test\bin\run_test.ps1 -name checkers -action tokens -language Cpp -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\tokens\Cpp\checkers.txt"
test\bin\run_test.ps1 -name firework -action tokens -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\tokens\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -name hello -action tokens -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\tokens\Pascal\HELLO.txt"
test\bin\run_test.ps1 -name rose -action tokens -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\tokens\Pascal\ROSE.txt"
test\bin\run_test.ps1 -name spider -action tokens -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\tokens\Pascal\SPIDER.txt"

Write-Output "Number of failures: $failures"
