Set-StrictMode -Version Latest

test\bin\run_test.ps1 -name freeformat1 -action detab -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\detab\COBOL\Person.txt"
test\bin\run_test.ps1 -name freeformat2 -action detab -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\detab\COBOL\Report.txt"
test\bin\run_test.ps1 -name freeformat3 -action detab -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\detab\COBOL\ReportCard.txt"
test\bin\run_test.ps1 -name freeformat4 -action detab -inputfile ".\test\data\COBOL-2002\SalesReport.cob" -expected ".\test\ref\detab\COBOL\SalesReport.txt"
