Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -name unwrap-cobol -action unwrap -language COBOL -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\unwrap\P010.txt"
test\bin\run_test.ps1 -name unwrap-fortran -action unwrap -language FORTRAN -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\unwrap\HERON.txt"

Write-Output "Number of failures: $failures"
