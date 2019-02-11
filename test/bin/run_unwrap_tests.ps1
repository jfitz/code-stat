Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -name unwrap-cobol -action unwrap -language COBOL -inputfile ".\test\data\COBOL-68\P010.cob" -expected ".\test\ref\unwrap-cobol\P010.txt"

Write-Output "Number of failures: $failures"
