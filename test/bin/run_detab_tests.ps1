Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -name "Cobol Program 1" -action detab -inputfile ".\test\data\COBOL\Cobol Program 1.COB" -expected ".\test\ref\detab\COBOL\Cobol Program 1.txt"
test\bin\run_test.ps1 -name "Cobol Program 2" -action detab -inputfile ".\test\data\COBOL\Cobol Program 2.COB" -expected ".\test\ref\detab\COBOL\Cobol Program 2.txt"
test\bin\run_test.ps1 -name "Cobol Program 3" -action detab -inputfile ".\test\data\COBOL\Cobol Program 3.COB" -expected ".\test\ref\detab\COBOL\Cobol Program 3.txt"
test\bin\run_test.ps1 -name "Cobol Program 4" -action detab -inputfile ".\test\data\COBOL\Cobol Program 4.COB" -expected ".\test\ref\detab\COBOL\Cobol Program 4.txt"

Write-Output "Number of failures: $failures"
