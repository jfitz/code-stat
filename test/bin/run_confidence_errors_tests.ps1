Set-StrictMode -Version Latest

[int]$failures = 0

# generic
test\bin\run_test.ps1 -json -name adabkend-adb -action confidence -errors -language generic -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\confidence-errors\generic\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action confidence -errors -language generic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\confidence-errors\generic\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action confidence -errors -language generic -inputfile ".\test\data\C\parser.c" -expected ".\test\ref\confidence-errors\generic\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action confidence -errors -language generic -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\confidence-errors\generic\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action confidence -errors -language generic -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\confidence-errors\generic\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action confidence -errors -language generic -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\confidence-errors\generic\heron.txt"
test\bin\run_test.ps1 -json -name bing -action confidence -errors -language generic -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\confidence-errors\generic\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action confidence -errors -language generic -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\confidence-errors\generic\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action confidence -errors -language generic -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\confidence-errors\generic\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action confidence -errors -language generic -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\confidence-errors\generic\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action confidence -errors -language generic -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\confidence-errors\generic\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action confidence -errors -language generic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\confidence-errors\generic\spider.txt"

# generic with comments
test\bin\run_test.ps1 -json -name adabkend-adb -action confidence -errors -language generic -comment ada -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\confidence-errors\generic-comments\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action confidence -errors -language generic -comment basic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\confidence-errors\generic-comments\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action confidence -errors -language generic -comment c -inputfile ".\test\data\C\parser.c" -expected ".\test\ref\confidence-errors\generic-comments\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action confidence -errors -language generic -comment cobol -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\confidence-errors\generic-comments\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action confidence -errors -language generic -comment cpp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\confidence-errors\generic-comments\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action confidence -errors -language generic -comment fortran -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\confidence-errors\generic-comments\heron.txt"
test\bin\run_test.ps1 -json -name bing -action confidence -errors -language generic -comment hash -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\confidence-errors\generic-comments\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action confidence -errors -language generic -comment percent -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\confidence-errors\generic-comments\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action confidence -errors -language generic -comment hash -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\confidence-errors\generic-comments\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action confidence -errors -language generic -comment hash -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\confidence-errors\generic-comments\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action confidence -errors -language generic -comment ada -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\confidence-errors\generic-comments\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action confidence -errors -language generic -comment basic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\confidence-errors\generic-comments\spider.txt"

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action confidence -errors -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-83 -action confidence -errors -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.ads" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-83 -action confidence -errors -language Ada-83 -inputfile ".\test\data\Ada-83\signup.adb" -expected ".\test\ref\confidence-errors\Ada-83\signup-adb.txt"

# Ada-95
test\bin\run_test.ps1 -json -name adabkend-adb-95 -action confidence -errors -language Ada-95 -inputfile ".\test\data\Ada-95\adabkend.adb" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-95 -action confidence -errors -language Ada-95 -inputfile ".\test\data\Ada-95\adabkend.ads" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-95 -action confidence -errors -language Ada-95 -inputfile ".\test\data\Ada-95\signup.adb" -expected ".\test\ref\confidence-errors\Ada-83\signup-adb.txt"

# Ada-2005
test\bin\run_test.ps1 -json -name adabkend-adb-2005 -action confidence -errors -language Ada-2005 -inputfile ".\test\data\Ada-2005\adabkend.adb" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-2005 -action confidence -errors -language Ada-2005 -inputfile ".\test\data\Ada-2005\adabkend.ads" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-2005 -action confidence -errors -language Ada-2005 -inputfile ".\test\data\Ada-2005\signup.adb" -expected ".\test\ref\confidence-errors\Ada-83\signup-adb.txt"

# Ada-2012
test\bin\run_test.ps1 -json -name adabkend-adb-2012 -action confidence -errors -language Ada-2012 -inputfile ".\test\data\Ada-2012\adabkend.adb" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-2012 -action confidence -errors -language Ada-2012 -inputfile ".\test\data\Ada-2012\adabkend.ads" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-2012 -action confidence -errors -language Ada-2012 -inputfile ".\test\data\Ada-2012\signup.adb" -expected ".\test\ref\confidence-errors\Ada-83\signup-adb.txt"

# Awk
test\bin\run_test.ps1 -json -name funstack-awk -action confidence -errors -language Awk -inputfile ".\test\data\Awk\funstack.awk" -expected ".\test\ref\confidence-errors\Awk\funstack.txt"
test\bin\run_test.ps1 -json -name awkaster-awk -action confidence -errors -language Awk -inputfile ".\test\data\Awk\awkaster.awk" -expected ".\test\ref\confidence-errors\Awk\awkaster.txt"

# BASIC
test\bin\run_test.ps1 -json -name values-bas -action confidence -errors -language BASIC -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\confidence-errors\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple-bas -action confidence -errors -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\confidence-errors\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot-bas -action confidence -errors -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\confidence-errors\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action confidence -errors -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\confidence-errors\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action confidence -errors -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\confidence-errors\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income-bas -action confidence -errors -language BASIC -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\confidence-errors\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2-bas -action confidence -errors -language BASIC -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\confidence-errors\BASIC\rockt2.txt"
test\bin\run_test.ps1 -json -name sea-creature-bas -action confidence -errors -language BASIC -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\confidence-errors\BASIC\sea_creature.txt"

# BASICA
test\bin\run_test.ps1 -json -name sea-creature-basica -action confidence -errors -language BASICA -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\confidence-errors\BASICA\sea_creature.txt"

# C
test\bin\run_test.ps1 -json -name diamond-c -action confidence -errors -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\confidence-errors\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test-c -action confidence -errors -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\confidence-errors\C\prime_test.txt"
test\bin\run_test.ps1 -json -name decl-c -action confidence -errors -language C -inputfile ".\test\data\C\c-decl.c" -expected ".\test\ref\confidence-errors\C\c-decl-c.txt"
test\bin\run_test.ps1 -json -name parser-h -action confidence -errors -language C -inputfile ".\test\data\C\parser.h" -expected ".\test\ref\confidence-errors\C\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c -action confidence -errors -language C -inputfile ".\test\data\C\parser.c" -expected ".\test\ref\confidence-errors\C\parser-c.txt"
test\bin\run_test.ps1 -json -name values-c -action confidence -errors -language C -inputfile ".\test\data\C\values.c" -expected ".\test\ref\confidence-errors\C\values.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action confidence -errors -language CBASIC -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\confidence-errors\CBASIC\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action confidence -errors -language CBASIC -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\confidence-errors\CBASIC\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action confidence -errors -language CBASIC -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\confidence-errors\CBASIC\GRAPHR.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken1-68 -action confidence -errors -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\confidence-errors\COBOL-68\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action confidence -errors -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\confidence-errors\COBOL-68\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action confidence -errors -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\confidence-errors\COBOL-68\mccracken3.txt"

# COBOL-74
test\bin\run_test.ps1 -json -name prog1-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG1.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG2.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG2A.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG3.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG4.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG5.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG6.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG12-2.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG13-3.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG14-2.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG15-4.COB" -expected ".\test\ref\confidence-errors\COBOL-74\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-74 -action confidence -errors -language COBOL-74 -tabsize 4 -inputfile ".\test\data\COBOL-74\UNLDDBCU2.COB" -expected ".\test\ref\confidence-errors\COBOL-74\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-wide-74 -action confidence -errors -language COBOL-74 -inputfile ".\test\data\COBOL-74\P010-wide.COB" -wide -expected ".\test\ref\confidence-errors\COBOL-74\P010-wide.txt"

# COBOL-85
test\bin\run_test.ps1 -json -name prog1-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG1.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG2.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG2A.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG3.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG4.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG5.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG6.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG12-2.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG13-3.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG14-2.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\confidence-errors\COBOL-85\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-85 -action confidence -errors -language COBOL-85 -tabsize 4 -inputfile ".\test\data\COBOL-85\UNLDDBCU2.COB" -expected ".\test\ref\confidence-errors\COBOL-85\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\confidence-errors\COBOL-85\P010.txt"

# COBOL-2002
test\bin\run_test.ps1 -json -name report-card-cob2002 -action confidence -errors -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\confidence-errors\COBOL-2002\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2002 -action confidence -errors -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\confidence-errors\COBOL-2002\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2002 -action confidence -errors -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\SalesReport.cob" -expected ".\test\ref\confidence-errors\COBOL-2002\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2002 -action confidence -errors -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\confidence-errors\COBOL-2002\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action confidence -errors -language COBOL-2002 -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\confidence-errors\COBOL-2002\AdventOfCode.txt"

# COBOL-2014
test\bin\run_test.ps1 -json -name report-card-cob2014 -action confidence -errors -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\ReportCard.cob" -expected ".\test\ref\confidence-errors\COBOL-2014\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014 -action confidence -errors -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\Person.cob" -expected ".\test\ref\confidence-errors\COBOL-2014\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014 -action confidence -errors -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\SalesReport.cob" -expected ".\test\ref\confidence-errors\COBOL-2014\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014 -action confidence -errors -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\Report.cob" -expected ".\test\ref\confidence-errors\COBOL-2014\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014 -action confidence -errors -language COBOL-2014 -inputfile ".\test\data\COBOL-2014\AdventOfCode.cob" -expected ".\test\ref\confidence-errors\COBOL-2014\AdventOfCode.txt"

# COBOL-2014 with AcuCobol extensions
test\bin\run_test.ps1 -json -name report-card-cob2014-ACU -action confidence -errors -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\ReportCard.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-ACU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-ACU -action confidence -errors -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\Person.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-ACU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-ACU -action confidence -errors -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\SalesReport.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-ACU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-ACU -action confidence -errors -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\Report.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-ACU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-ACU -action confidence -errors -language COBOL-2014-ACU -inputfile ".\test\data\COBOL-2014-ACU\AdventOfCode.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-ACU\AdventOfCode.txt"

# COBOL-2014 with IBM extensions
test\bin\run_test.ps1 -json -name report-card-cob2014-IBM -action confidence -errors -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\ReportCard.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-IBM\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-IBM -action confidence -errors -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\Person.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-IBM\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-IBM -action confidence -errors -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\SalesReport.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-IBM\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-IBM -action confidence -errors -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\Report.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-IBM\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-IBM -action confidence -errors -language COBOL-2014-IBM -inputfile ".\test\data\COBOL-2014-IBM\AdventOfCode.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-IBM\AdventOfCode.txt"

# COBOL-2014 with GNU extensions
test\bin\run_test.ps1 -json -name report-card-cob2014-GNU -action confidence -errors -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\ReportCard.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-GNU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-GNU -action confidence -errors -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\Person.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-GNU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action confidence -errors -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-GNU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-GNU -action confidence -errors -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\Report.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-GNU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-GNU -action confidence -errors -language COBOL-2014-GNU -inputfile ".\test\data\COBOL-2014-GNU\AdventOfCode.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-GNU\AdventOfCode.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\confidence-errors\Cplusplus\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\confidence-errors\Cplusplus\hrdb.txt"
test\bin\run_test.ps1 -json -name date-h-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\confidence-errors\Cplusplus\date_h.txt"
test\bin\run_test.ps1 -json -name date-cplusplus-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\confidence-errors\Cplusplus\date_cpp.txt"
test\bin\run_test.ps1 -json -name inherit-cplusplus-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\confidence-errors\Cplusplus\inherit.txt"
test\bin\run_test.ps1 -json -name values-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\values.cpp" -expected ".\test\ref\confidence-errors\Cplusplus\values.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action confidence -errors -language Csharp -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\confidence-errors\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2-cs -action confidence -errors -language Csharp -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\confidence-errors\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3-cs -action confidence -errors -language Csharp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\confidence-errors\Csharp\calculator3.txt"

# D
test\bin\run_test.ps1 -json -name regex-d -action confidence -errors -language d -inputfile ".\test\data\d\regex.d" -expected ".\test\ref\confidence-errors\d\regex.txt"
test\bin\run_test.ps1 -json -name halffloat-d -action confidence -errors -language d -inputfile ".\test\data\d\halffloat.d" -expected ".\test\ref\confidence-errors\d\halffloat.txt"

# dbase II
test\bin\run_test.ps1 -json -name sample-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\sample.prg" -expected ".\test\ref\confidence-errors\dbase-II\sample.txt"

# FORTRAN-66
test\bin\run_test.ps1 -json -name hello-ftn66 -action confidence -errors -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\confidence-errors\FORTRAN-66\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action confidence -errors -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\confidence-errors\FORTRAN-66\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action confidence -errors -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\confidence-errors\FORTRAN-66\HERON2.txt"
test\bin\run_test.ps1 -json -name heron-wide-ftn66 -action confidence -errors -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON-wide.FOR" -wide -expected ".\test\ref\confidence-errors\FORTRAN-66\HERON-wide.txt"

# FORTRAN-77
test\bin\run_test.ps1 -json -name hello-ftn77 -action confidence -errors -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\confidence-errors\FORTRAN-77\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action confidence -errors -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\confidence-errors\FORTRAN-77\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action confidence -errors -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\confidence-errors\FORTRAN-77\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action confidence -errors -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\confidence-errors\FORTRAN-77\HERON.txt"

# Fortran-90
test\bin\run_test.ps1 -json -name average-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\confidence-errors\Fortran-90\average.txt"
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\confidence-errors\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\confidence-errors\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\confidence-errors\Fortran-90\hello.txt"

# Fortran-95
test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action confidence -errors -language Fortran-95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\confidence-errors\Fortran-95\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action confidence -errors -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\confidence-errors\Fortran-95\Wikibooks-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn95 -action confidence -errors -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex2.f95" -expected ".\test\ref\confidence-errors\Fortran-95\Wikibooks-ex2.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn95 -action confidence -errors -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex3.f95" -expected ".\test\ref\confidence-errors\Fortran-95\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn95 -action confidence -errors -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex4.f95" -expected ".\test\ref\confidence-errors\Fortran-95\Wikibooks-ex4.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action confidence -errors -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\confidence-errors\Fortran-2003\geo4060.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn2003 -action confidence -errors -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\temp_sub.for" -expected ".\test\ref\confidence-errors\Fortran-2003\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn2003 -action confidence -errors -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\temp_func.for" -expected ".\test\ref\confidence-errors\Fortran-2003\temp_func.txt"

# Fortran-2008
test\bin\run_test.ps1 -json -name geo4060-ftn2008 -action confidence -errors -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\geo4060.for" -expected ".\test\ref\confidence-errors\Fortran-2008\geo4060.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn2008 -action confidence -errors -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\temp_sub.for" -expected ".\test\ref\confidence-errors\Fortran-2008\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn2008 -action confidence -errors -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\temp_func.for" -expected ".\test\ref\confidence-errors\Fortran-2008\temp_func.txt"

# F#
test\bin\run_test.ps1 -json -name samples-fs -action confidence -errors -language Fsharp -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\confidence-errors\Fsharp\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action confidence -errors -language Go -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\confidence-errors\Go\find-cli.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action confidence -errors -language HTML -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\confidence-errors\HTML\knuth.txt"
test\bin\run_test.ps1 -json -name developer-html -action confidence -errors -language HTML -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\confidence-errors\HTML\developer-css.txt"
test\bin\run_test.ps1 -json -name codestat-html -action confidence -errors -language HTML -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\confidence-errors\HTML\codestat-css-javascript.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action confidence -errors -language Java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\confidence-errors\Java\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action confidence -errors -language Java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\confidence-errors\Java\palindrome.txt"
test\bin\run_test.ps1 -json -name binary-search-java -action confidence -errors -language Java -inputfile ".\test\data\Java\binary_search.java" -expected ".\test\ref\confidence-errors\Java\binary_search.txt"
test\bin\run_test.ps1 -json -name ObjectServer-java -action confidence -errors -language Java -inputfile ".\test\data\Java\ObjectServer.java" -expected ".\test\ref\confidence-errors\Java\ObjectServer.txt"

# JavaScript
test\bin\run_test.ps1 -json -name values-js -action confidence -errors -language JavaScript -inputfile ".\test\data\JavaScript\values.js" -expected ".\test\ref\confidence-errors\JavaScript\values.txt"
test\bin\run_test.ps1 -json -name codestat-js -action confidence -errors -language JavaScript -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\confidence-errors\JavaScript\codestat.txt"
test\bin\run_test.ps1 -json -name bing-js -action confidence -errors -language JavaScript -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\confidence-errors\JavaScript\bing.txt"
test\bin\run_test.ps1 -json -name calc_prime-js -action confidence -errors -language JavaScript -inputfile ".\test\data\JavaScript\calc_prime.js" -expected ".\test\ref\confidence-errors\JavaScript\calc_prime.txt"
test\bin\run_test.ps1 -json -name backtick-js -action confidence -errors -language JavaScript -inputfile ".\test\data\JavaScript\backtick.js" -expected ".\test\ref\confidence-errors\JavaScript\backtick.txt"

# Kotlin
test\bin\run_test.ps1 -json -name qksms-kt -action confidence -errors -language Kotlin -inputfile ".\test\data\Kotlin\qksms.kt" -expected ".\test\ref\confidence-errors\Kotlin\qksms.txt"
test\bin\run_test.ps1 -json -name render-kt -action confidence -errors -language Kotlin -inputfile ".\test\data\Kotlin\render.kt" -expected ".\test\ref\confidence-errors\Kotlin\render.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action confidence -errors -language Objective-C -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\confidence-errors\Objective-C\HelloWorld.txt"
test\bin\run_test.ps1 -json -name qrmath-objc -action confidence -errors -language Objective-C -inputfile ".\test\data\Objective-C\QRMath.h" -expected ".\test\ref\confidence-errors\Objective-C\QRMath.txt"
test\bin\run_test.ps1 -json -name qrencoder-objc -action confidence -errors -language Objective-C -inputfile ".\test\data\Objective-C\QREncoder.m" -expected ".\test\ref\confidence-errors\Objective-C\QREncoder.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\confidence-errors\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\confidence-errors\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\confidence-errors\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\confidence-errors\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\confidence-errors\Pascal\TPC16.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action confidence -errors -language PL1-Fixed -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\confidence-errors\PL1\BPGPLI.txt"
test\bin\run_test.ps1 -json -name checkdt-pl1 -action confidence -errors -language PL1-Free -inputfile ".\test\data\PL1\CHECKDT.pl1" -expected ".\test\ref\confidence-errors\PL1\CHECKDT.txt"
test\bin\run_test.ps1 -json -name mainfact-pl1 -action confidence -errors -language PL1 -inputfile ".\test\data\PL1\MAINFACT.pl1" -expected ".\test\ref\confidence-errors\PL1\MAINFACT.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main-prolog -action confidence -errors -language Prolog -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\confidence-errors\Prolog\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb-prolog -action confidence -errors -language Prolog -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\confidence-errors\Prolog\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu-prolog -action confidence -errors -language Prolog -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\confidence-errors\Prolog\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries-prolog -action confidence -errors -language Prolog -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\confidence-errors\Prolog\family-queries.txt"
test\bin\run_test.ps1 -json -name web-server-hello-prolog -action confidence -errors -language Prolog -inputfile ".\test\data\Prolog\web-server-hello.pl" -expected ".\test\ref\confidence-errors\Prolog\web-server-hello.txt"
test\bin\run_test.ps1 -json -name web-server-params-prolog -action confidence -errors -language Prolog -inputfile ".\test\data\Prolog\web-server-params.pl" -expected ".\test\ref\confidence-errors\Prolog\web-server-params.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d-python -action confidence -errors -language Python -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\confidence-errors\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor-python -action confidence -errors -language Python -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\confidence-errors\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory-python -action confidence -errors -language Python -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\confidence-errors\Python\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-python -action confidence -errors -language Python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\confidence-errors\Python\values.txt"
test\bin\run_test.ps1 -json -name examiner-python -action confidence -errors -language Python -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\confidence-errors\Python\Examiner.txt"
test\bin\run_test.ps1 -json -name authorized-view-python -action confidence -errors -language Python -inputfile ".\test\data\Python\authorized_view.py" -expected ".\test\ref\confidence-errors\Python\authorized_view.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01-r -action confidence -errors -language R -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\confidence-errors\R\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02-r -action confidence -errors -language R -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\confidence-errors\R\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03-r -action confidence -errors -language R -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\confidence-errors\R\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04-r -action confidence -errors -language R -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\confidence-errors\R\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05-r -action confidence -errors -language R -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\confidence-errors\R\ETM-540-05.txt"
test\bin\run_test.ps1 -json -name basketball-r -action confidence -errors -language R -inputfile ".\test\data\R\basketball.R" -expected ".\test\ref\confidence-errors\R\basketball.txt"
test\bin\run_test.ps1 -json -name render-r -action confidence -errors -language R -inputfile ".\test\data\R\render.R" -expected ".\test\ref\confidence-errors\R\render.txt"

# Ruby
test\bin\run_test.ps1 -json -name basic-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\confidence-errors\Ruby\basic.txt"
test\bin\run_test.ps1 -json -name constants-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\constants.rb" -expected ".\test\ref\confidence-errors\Ruby\constants.txt"
test\bin\run_test.ps1 -json -name exceptions-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\exceptions.rb" -expected ".\test\ref\confidence-errors\Ruby\exceptions.txt"
test\bin\run_test.ps1 -json -name expressions-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\expressions.rb" -expected ".\test\ref\confidence-errors\Ruby\expressions.txt"
test\bin\run_test.ps1 -json -name functions-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\functions.rb" -expected ".\test\ref\confidence-errors\Ruby\functions.txt"
test\bin\run_test.ps1 -json -name io-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\io.rb" -expected ".\test\ref\confidence-errors\Ruby\io.txt"
test\bin\run_test.ps1 -json -name modifiers-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\modifiers.rb" -expected ".\test\ref\confidence-errors\Ruby\modifiers.txt"
test\bin\run_test.ps1 -json -name operators-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\operators.rb" -expected ".\test\ref\confidence-errors\Ruby\operators.txt"
test\bin\run_test.ps1 -json -name statements-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\statements.rb" -expected ".\test\ref\confidence-errors\Ruby\statements.txt"
test\bin\run_test.ps1 -json -name tokenbuilders-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\tokenbuilders.rb" -expected ".\test\ref\confidence-errors\Ruby\tokenbuilders.txt"
test\bin\run_test.ps1 -json -name tokenizers-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\tokenizers.rb" -expected ".\test\ref\confidence-errors\Ruby\tokenizers.txt"
test\bin\run_test.ps1 -json -name tokens-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\tokens.rb" -expected ".\test\ref\confidence-errors\Ruby\tokens.txt"
test\bin\run_test.ps1 -json -name webhook-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\confidence-errors\Ruby\webhook.txt"

# Rust
test\bin\run_test.ps1 -json -name literals-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\literals.rs" -expected ".\test\ref\confidence-errors\Rust\literals.txt"
test\bin\run_test.ps1 -json -name dom-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\dom.rs" -expected ".\test\ref\confidence-errors\Rust\dom.txt"
test\bin\run_test.ps1 -json -name html-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\html.rs" -expected ".\test\ref\confidence-errors\Rust\html.txt"
test\bin\run_test.ps1 -json -name geometry-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\geometry.rs" -expected ".\test\ref\confidence-errors\Rust\geometry.txt"
test\bin\run_test.ps1 -json -name scene-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\scene.rs" -expected ".\test\ref\confidence-errors\Rust\scene.txt"
test\bin\run_test.ps1 -json -name comments-1-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\nested_comments_1.rs" -expected ".\test\ref\confidence-errors\Rust\nested_comments_1.txt"
test\bin\run_test.ps1 -json -name comments-2-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\nested_comments_2.rs" -expected ".\test\ref\confidence-errors\Rust\nested_comments_2.txt"
test\bin\run_test.ps1 -json -name comments-3-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\nested_comments_3.rs" -expected ".\test\ref\confidence-errors\Rust\nested_comments_3.txt"
test\bin\run_test.ps1 -json -name raw-string-1-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\raw_string_1.rs" -expected ".\test\ref\confidence-errors\Rust\raw_string_1.txt"
test\bin\run_test.ps1 -json -name raw-string-2-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\raw_string_2.rs" -expected ".\test\ref\confidence-errors\Rust\raw_string_2.txt"
test\bin\run_test.ps1 -json -name attributes-1-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\attributes_1.rs" -expected ".\test\ref\confidence-errors\Rust\attributes_1.txt"
test\bin\run_test.ps1 -json -name chip8-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\chip8.rs" -expected ".\test\ref\confidence-errors\Rust\chip8.txt"
test\bin\run_test.ps1 -json -name chip8-display-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\chip8-display.rs" -expected ".\test\ref\confidence-errors\Rust\chip8-display.txt"
test\bin\run_test.ps1 -json -name chip8-instructions-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\chip8-instructions.rs" -expected ".\test\ref\confidence-errors\Rust\chip8-instructions.txt"
test\bin\run_test.ps1 -json -name chip8-main-rust -action confidence -errors -language Rust -inputfile ".\test\data\Rust\chip8-main.rs" -expected ".\test\ref\confidence-errors\Rust\chip8-main.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action confidence -errors -language SQL-92 -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\confidence-errors\SQL-92\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action confidence -errors -language SQL-92 -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\confidence-errors\SQL-92\table.txt"

# SQL-99
test\bin\run_test.ps1 -json -name microsoft-sql-99 -action confidence -errors -language SQL-99 -inputfile ".\test\data\SQL-99\microsoft.sql" -expected ".\test\ref\confidence-errors\SQL-99\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-99 -action confidence -errors -language SQL-99 -inputfile ".\test\data\SQL-99\table.sql" -expected ".\test\ref\confidence-errors\SQL-99\table.txt"

# SQL-2003
test\bin\run_test.ps1 -json -name microsoft-sql-2003 -action confidence -errors -language SQL-2003 -inputfile ".\test\data\SQL-2003\microsoft.sql" -expected ".\test\ref\confidence-errors\SQL-2003\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2003 -action confidence -errors -language SQL-2003 -inputfile ".\test\data\SQL-2003\table.sql" -expected ".\test\ref\confidence-errors\SQL-2003\table.txt"

# SQL-2008
test\bin\run_test.ps1 -json -name microsoft-sql-2008 -action confidence -errors -language SQL-2008 -inputfile ".\test\data\SQL-2008\microsoft.sql" -expected ".\test\ref\confidence-errors\SQL-2008\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2008 -action confidence -errors -language SQL-2008 -inputfile ".\test\data\SQL-2008\table.sql" -expected ".\test\ref\confidence-errors\SQL-2008\table.txt"

# SQL-2011
test\bin\run_test.ps1 -json -name microsoft-sql-2011 -action confidence -errors -language SQL-2011 -inputfile ".\test\data\SQL-2011\microsoft.sql" -expected ".\test\ref\confidence-errors\SQL-2011\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2011 -action confidence -errors -language SQL-2011 -inputfile ".\test\data\SQL-2011\table.sql" -expected ".\test\ref\confidence-errors\SQL-2011\table.txt"

# SQL-2016
test\bin\run_test.ps1 -json -name microsoft-sql-2016 -action confidence -errors -language SQL-2016 -inputfile ".\test\data\SQL-2016\microsoft.sql" -expected ".\test\ref\confidence-errors\SQL-2016\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2016 -action confidence -errors -language SQL-2016 -inputfile ".\test\data\SQL-2016\table.sql" -expected ".\test\ref\confidence-errors\SQL-2016\table.txt"

# Swift
test\bin\run_test.ps1 -json -name AppDelegate -action confidence -errors -language Swift -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\confidence-errors\Swift\AppDelegate.txt"
test\bin\run_test.ps1 -json -name Meal -action confidence -errors -language Swift -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\confidence-errors\Swift\Meal.txt"
test\bin\run_test.ps1 -json -name MealTableViewCell -action confidence -errors -language Swift -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\confidence-errors\Swift\MealTableViewCell.txt"
test\bin\run_test.ps1 -json -name MealTableViewController -action confidence -errors -language Swift -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\confidence-errors\Swift\MealTableViewController.txt"
test\bin\run_test.ps1 -json -name MealViewController -action confidence -errors -language Swift -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\confidence-errors\Swift\MealViewController.txt"
test\bin\run_test.ps1 -json -name RatingControl -action confidence -errors -language Swift -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\confidence-errors\Swift\RatingControl.txt"
test\bin\run_test.ps1 -json -name URLExtensions -action confidence -errors -language Swift -inputfile ".\test\data\Swift\URLExtensions.swift" -expected ".\test\ref\confidence-errors\Swift\URLExtensions.txt"

# TypeScript
test\bin\run_test.ps1 -json -name TimeReporter-ts -action confidence -errors -language TypeScript -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\confidence-errors\TypeScript\TimeReporter.txt"
test\bin\run_test.ps1 -json -name ImageBoard-ts -action confidence -errors -language TypeScript -inputfile ".\test\data\TypeScript\ImageBoard.ts" -expected ".\test\ref\confidence-errors\TypeScript\ImageBoard.txt"

# Visual Basic 6
test\bin\run_test.ps1 -json -name spider-vb6 -action confidence -errors -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\confidence-errors\VisualBasic-6\spider.txt"
test\bin\run_test.ps1 -json -name azure_metadata-vb6 -action confidence -errors -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\azure_metadata.bas" -expected ".\test\ref\confidence-errors\VisualBasic-6\azure_metadata.txt"
test\bin\run_test.ps1 -json -name diffie_hellman-vb6 -action confidence -errors -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\diffie_hellman.bas" -expected ".\test\ref\confidence-errors\VisualBasic-6\diffie_hellman.txt"

# Visual Basic .NET
test\bin\run_test.ps1 -json -name word-processor-vbnet -action confidence -errors -language VisualBasic-NET -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\confidence-errors\VisualBasic-NET\WordProcessor.txt"

# Polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c -action confidence -errors -language C -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence-errors\C\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence-errors\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action confidence -errors -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence-errors\Pascal\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-python -action confidence -errors -language Python -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\confidence-errors\Python\polyglot-py-rb.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\confidence-errors\Ruby\polyglot-py-rb.txt"

Write-Output "Number of failures: $failures"
