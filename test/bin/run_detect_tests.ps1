Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -json -name simple -action detect -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\detect\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot -action detect -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\detect\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum -action detect -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\detect\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life -action detect -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\detect\BASIC\life.txt"

test\bin\run_test.ps1 -json -name diamond -action detect -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\detect\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test -action detect -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\detect\C\prime_test.txt"

test\bin\run_test.ps1 -json -name prog1 -action detect -inputfile ".\test\data\COBOL\PROG1.COB" -expected ".\test\ref\detect\COBOL\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2 -action detect -inputfile ".\test\data\COBOL\PROG2.COB" -expected ".\test\ref\detect\COBOL\PROG2.txt"
test\bin\run_test.ps1 -json -name prog3 -action detect -inputfile ".\test\data\COBOL\PROG3.COB" -expected ".\test\ref\detect\COBOL\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4 -action detect -inputfile ".\test\data\COBOL\PROG4.COB" -expected ".\test\ref\detect\COBOL\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5 -action detect -inputfile ".\test\data\COBOL\PROG5.COB" -expected ".\test\ref\detect\COBOL\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6 -action detect -inputfile ".\test\data\COBOL\PROG6.COB" -expected ".\test\ref\detect\COBOL\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2 -action detect -inputfile ".\test\data\COBOL\PROG12-2.COB" -expected ".\test\ref\detect\COBOL\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3 -action detect -inputfile ".\test\data\COBOL\PROG13-3.COB" -expected ".\test\ref\detect\COBOL\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2 -action detect -inputfile ".\test\data\COBOL\PROG14-2.COB" -expected ".\test\ref\detect\COBOL\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4 -action detect -inputfile ".\test\data\COBOL\PROG15-4.COB" -expected ".\test\ref\detect\COBOL\PROG15-4.txt"
test\bin\run_test.ps1 -json -name freeformat1 -action detect -inputfile ".\test\data\COBOL\Free Format 1.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\Free Format 1.txt"
test\bin\run_test.ps1 -json -name freeformat2 -action detect -inputfile ".\test\data\COBOL\Free Format 2.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\Free Format 2.txt"
test\bin\run_test.ps1 -json -name freeformat3 -action detect -inputfile ".\test\data\COBOL\Free Format 3.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\Free Format 3.txt"
test\bin\run_test.ps1 -json -name freeformat4 -action detect -inputfile ".\test\data\COBOL\Free Format 4.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\Free Format 4.txt"
test\bin\run_test.ps1 -json -name exec1 -action detect -inputfile ".\test\data\COBOL\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\UNLDDBCU2.txt"

test\bin\run_test.ps1 -json -name checkers -action detect -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\detect\Cpp\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb -action detect -inputfile ".\test\data\Cpp\hrdb.cpp" -expected ".\test\ref\detect\Cpp\hrdb.txt"
test\bin\run_test.ps1 -json -name date_h -action detect -inputfile ".\test\data\Cpp\date.h" -expected ".\test\ref\detect\Cpp\date_h.txt"
test\bin\run_test.ps1 -json -name date_cpp -action detect -inputfile ".\test\data\Cpp\date.cpp" -expected ".\test\ref\detect\Cpp\date_cpp.txt"

test\bin\run_test.ps1 -json -name firework -action detect -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello -action detect -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\detect\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose -action detect -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\detect\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider -action detect -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\detect\Pascal\SPIDER.txt"

test\bin\run_test.ps1 -json -name polyglot -action detect -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect\multiple\polyglot.txt"

Write-Output "Number of failures: $failures"
