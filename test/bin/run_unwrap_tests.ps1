Set-StrictMode -Version Latest

test\bin\run_test.ps1 -name unwrap-c -action unwrap -language C -inputfile ".\test\data\C\c-decl.c" -expected ".\test\ref\unwrap\c-decl.txt"
test\bin\run_test.ps1 -name unwrap-cbasic -action unwrap -language CBasic -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\unwrap\DEMOGRAF.txt"
test\bin\run_test.ps1 -name unwrap-cobol -action unwrap -language COBOL -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\unwrap\P010.txt"
# C++
test\bin\run_test.ps1 -name unwrap-fortran -action unwrap -language FORTRAN -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\unwrap\HERON.txt"
# Objective-C
test\bin\run_test.ps1 -name unwrap-pl1 -action unwrap -language PL1-Fixed -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\unwrap\bpgpli.txt"
test\bin\run_test.ps1 -name unwrap-python -action unwrap -language Python -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\unwrap\Examiner.txt"
