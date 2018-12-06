Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -name freeformat1 -action detab -inputfile ".\test\data\COBOL\Free Format 1.COB" -expected ".\test\ref\detab\COBOL\Free Format 1.txt"
test\bin\run_test.ps1 -name freeformat2 -action detab -inputfile ".\test\data\COBOL\Free Format 2.COB" -expected ".\test\ref\detab\COBOL\Free Format 2.txt"
test\bin\run_test.ps1 -name freeformat3 -action detab -inputfile ".\test\data\COBOL\Free Format 3.COB" -expected ".\test\ref\detab\COBOL\Free Format 3.txt"
test\bin\run_test.ps1 -name freeformat4 -action detab -inputfile ".\test\data\COBOL\Free Format 4.COB" -expected ".\test\ref\detab\COBOL\Free Format 4.txt"

Write-Output "Number of failures: $failures"
