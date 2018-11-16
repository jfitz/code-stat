Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -name simple -action detect -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\detect\BASIC\simple.txt"
test\bin\run_test.ps1 -name diamond -action detect -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\detect\C\diamond.txt"
test\bin\run_test.ps1 -name prime_test -action detect -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\detect\C\prime_test.txt"
test\bin\run_test.ps1 -name prog1 -action detect -inputfile ".\test\data\COBOL\PROG1.COB" -expected ".\test\ref\detect\COBOL\PROG1.txt"
test\bin\run_test.ps1 -name prog2 -action detect -inputfile ".\test\data\COBOL\PROG2.COB" -expected ".\test\ref\detect\COBOL\PROG2.txt"
test\bin\run_test.ps1 -name prog3 -action detect -inputfile ".\test\data\COBOL\PROG3.COB" -expected ".\test\ref\detect\COBOL\PROG3.txt"
test\bin\run_test.ps1 -name prog4 -action detect -inputfile ".\test\data\COBOL\PROG4.COB" -expected ".\test\ref\detect\COBOL\PROG4.txt"
test\bin\run_test.ps1 -name prog5 -action detect -inputfile ".\test\data\COBOL\PROG5.COB" -expected ".\test\ref\detect\COBOL\PROG5.txt"
test\bin\run_test.ps1 -name prog6 -action detect -inputfile ".\test\data\COBOL\PROG6.COB" -expected ".\test\ref\detect\COBOL\PROG6.txt"
test\bin\run_test.ps1 -name prog12-2 -action detect -inputfile ".\test\data\COBOL\PROG12-2.COB" -expected ".\test\ref\detect\COBOL\PROG12-2.txt"
test\bin\run_test.ps1 -name prog13-3 -action detect -inputfile ".\test\data\COBOL\PROG13-3.COB" -expected ".\test\ref\detect\COBOL\PROG13-3.txt"
test\bin\run_test.ps1 -name prog14-2 -action detect -inputfile ".\test\data\COBOL\PROG14-2.COB" -expected ".\test\ref\detect\COBOL\PROG14-2.txt"
test\bin\run_test.ps1 -name prog15-4 -action detect -inputfile ".\test\data\COBOL\PROG15-4.COB" -expected ".\test\ref\detect\COBOL\PROG15-4.txt"
test\bin\run_test.ps1 -name checkers -action detect -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\detect\Cpp\checkers.txt"
test\bin\run_test.ps1 -name firework -action detect -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -name hello -action detect -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\detect\Pascal\HELLO.txt"
test\bin\run_test.ps1 -name rose -action detect -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\detect\Pascal\ROSE.txt"
test\bin\run_test.ps1 -name spider -action detect -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\detect\Pascal\SPIDER.txt"

Write-Output "Number of failures: $failures"
