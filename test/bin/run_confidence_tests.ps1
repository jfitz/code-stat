Set-StrictMode -Version Latest

[int]$failures = 0

# generic
test\bin\run_test.ps1 -json -name adabkend-adb -action confidence -language generic -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\confidence\generic\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action confidence -language generic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\confidence\generic\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action confidence -language generic -inputfile ".\test\data\C\parser.c" -expected ".\test\ref\confidence\generic\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action confidence -language generic -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\confidence\generic\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action confidence -language generic -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\confidence\generic\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action confidence -language generic -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\confidence\generic\heron.txt"
test\bin\run_test.ps1 -json -name bing -action confidence -language generic -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\confidence\generic\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action confidence -language generic -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\confidence\generic\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action confidence -language generic -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\confidence\generic\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action confidence -language generic -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\confidence\generic\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action confidence -language generic -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\confidence\generic\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action confidence -language generic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\confidence\generic\spider.txt"

# generic with comments
test\bin\run_test.ps1 -json -name adabkend-adb -action confidence -language generic -comment ada -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\confidence\generic-comments\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action confidence -language generic -comment basic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\confidence\generic-comments\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action confidence -language generic -comment c -inputfile ".\test\data\C\parser.c" -expected ".\test\ref\confidence\generic-comments\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action confidence -language generic -comment cobol -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\confidence\generic-comments\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action confidence -language generic -comment cpp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\confidence\generic-comments\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action confidence -language generic -comment fortran -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\confidence\generic-comments\heron.txt"
test\bin\run_test.ps1 -json -name bing -action confidence -language generic -comment hash -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\confidence\generic-comments\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action confidence -language generic -comment percent -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\confidence\generic-comments\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action confidence -language generic -comment hash -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\confidence\generic-comments\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action confidence -language generic -comment hash -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\confidence\generic-comments\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action confidence -language generic -comment ada -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\confidence\generic-comments\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action confidence -language generic -comment basic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\confidence\generic-comments\spider.txt"

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action confidence -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\confidence\Ada-83\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-83 -action confidence -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.ads" -expected ".\test\ref\confidence\Ada-83\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-83 -action confidence -language Ada-83 -inputfile ".\test\data\Ada-83\signup.adb" -expected ".\test\ref\confidence\Ada-83\signup-adb.txt"

# Ada-95
test\bin\run_test.ps1 -json -name adabkend-adb-95 -action confidence -language Ada-95 -inputfile ".\test\data\Ada-95\adabkend.adb" -expected ".\test\ref\confidence\Ada-95\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-95 -action confidence -language Ada-95 -inputfile ".\test\data\Ada-95\adabkend.ads" -expected ".\test\ref\confidence\Ada-95\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-95 -action confidence -language Ada-95 -inputfile ".\test\data\Ada-95\signup.adb" -expected ".\test\ref\confidence\Ada-95\signup-adb.txt"

# Ada-2005
test\bin\run_test.ps1 -json -name adabkend-adb-2005 -action confidence -language Ada-2005 -inputfile ".\test\data\Ada-2005\adabkend.adb" -expected ".\test\ref\confidence\Ada-2005\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-2005 -action confidence -language Ada-2005 -inputfile ".\test\data\Ada-2005\adabkend.ads" -expected ".\test\ref\confidence\Ada-2005\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-2005 -action confidence -language Ada-2005 -inputfile ".\test\data\Ada-2005\signup.adb" -expected ".\test\ref\confidence\Ada-2005\signup-adb.txt"

# Ada-2012
test\bin\run_test.ps1 -json -name adabkend-adb-2012 -action confidence -language Ada-2012 -inputfile ".\test\data\Ada-2012\adabkend.adb" -expected ".\test\ref\confidence\Ada-2012\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-2012 -action confidence -language Ada-2012 -inputfile ".\test\data\Ada-2012\adabkend.ads" -expected ".\test\ref\confidence\Ada-2012\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-2012 -action confidence -language Ada-2012 -inputfile ".\test\data\Ada-2012\signup.adb" -expected ".\test\ref\confidence\Ada-2012\signup-adb.txt"

# Awk
test\bin\run_test.ps1 -json -name funstack-awk -action confidence -language Awk -inputfile ".\test\data\Awk\funstack.awk" -expected ".\test\ref\confidence\Awk\funstack.txt"
test\bin\run_test.ps1 -json -name awkaster-awk -action confidence -language Awk -inputfile ".\test\data\Awk\awkaster.awk" -expected ".\test\ref\confidence\Awk\awkaster.txt"

# BASIC
test\bin\run_test.ps1 -json -name values-bas -action confidence -language BASIC -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\confidence\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple-bas -action confidence -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\confidence\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot-bas -action confidence -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\confidence\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action confidence -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\confidence\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action confidence -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\confidence\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income-bas -action confidence -language BASIC -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\confidence\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2-bas -action confidence -language BASIC -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\confidence\BASIC\rockt2.txt"
test\bin\run_test.ps1 -json -name sea-creature-bas -action confidence -language BASIC -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\confidence\BASIC\sea_creature.txt"

# BASICA
test\bin\run_test.ps1 -json -name sea-creature-basica -action confidence -language BASICA -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\confidence\BASICA\sea_creature.txt"

# C
test\bin\run_test.ps1 -json -name diamond-c -action confidence -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\confidence\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test-c -action confidence -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\confidence\C\prime_test.txt"
test\bin\run_test.ps1 -json -name decl-c -action confidence -language C -inputfile ".\test\data\C\c-decl.c" -expected ".\test\ref\confidence\C\c-decl-c.txt"
test\bin\run_test.ps1 -json -name parser-h -action confidence -language C -inputfile ".\test\data\C\parser.h" -expected ".\test\ref\confidence\C\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c -action confidence -language C -inputfile ".\test\data\C\parser.c" -expected ".\test\ref\confidence\C\parser-c.txt"
test\bin\run_test.ps1 -json -name values-c -action confidence -language C -inputfile ".\test\data\C\values.c" -expected ".\test\ref\confidence\C\values.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action confidence -language CBASIC -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\confidence\CBASIC\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action confidence -language CBASIC -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\confidence\CBASIC\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action confidence -language CBASIC -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\confidence\CBASIC\GRAPHR.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken1-68 -action confidence -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\confidence\COBOL-68\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action confidence -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\confidence\COBOL-68\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action confidence -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\confidence\COBOL-68\mccracken3.txt"

# COBOL-74
test\bin\run_test.ps1 -json -name prog1-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG1.COB" -expected ".\test\ref\confidence\COBOL-74\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG2.COB" -expected ".\test\ref\confidence\COBOL-74\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG2A.COB" -expected ".\test\ref\confidence\COBOL-74\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG3.COB" -expected ".\test\ref\confidence\COBOL-74\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG4.COB" -expected ".\test\ref\confidence\COBOL-74\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG5.COB" -expected ".\test\ref\confidence\COBOL-74\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG6.COB" -expected ".\test\ref\confidence\COBOL-74\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG12-2.COB" -expected ".\test\ref\confidence\COBOL-74\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG13-3.COB" -expected ".\test\ref\confidence\COBOL-74\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG14-2.COB" -expected ".\test\ref\confidence\COBOL-74\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG15-4.COB" -expected ".\test\ref\confidence\COBOL-74\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-74 -action confidence -language COBOL-74 -tabsize 4 -inputfile ".\test\data\COBOL-74\UNLDDBCU2.COB" -expected ".\test\ref\confidence\COBOL-74\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-wide-74 -action confidence -language COBOL-74 -inputfile ".\test\data\COBOL-74\P010-wide.COB" -wide -expected ".\test\ref\confidence\COBOL-74\P010-wide.txt"

# COBOL-85
test\bin\run_test.ps1 -json -name prog1-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG1.COB" -expected ".\test\ref\confidence\COBOL-85\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG2.COB" -expected ".\test\ref\confidence\COBOL-85\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG2A.COB" -expected ".\test\ref\confidence\COBOL-85\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG3.COB" -expected ".\test\ref\confidence\COBOL-85\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG4.COB" -expected ".\test\ref\confidence\COBOL-85\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG5.COB" -expected ".\test\ref\confidence\COBOL-85\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG6.COB" -expected ".\test\ref\confidence\COBOL-85\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG12-2.COB" -expected ".\test\ref\confidence\COBOL-85\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG13-3.COB" -expected ".\test\ref\confidence\COBOL-85\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG14-2.COB" -expected ".\test\ref\confidence\COBOL-85\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\confidence\COBOL-85\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-85 -action confidence -language COBOL-85 -tabsize 4 -inputfile ".\test\data\COBOL-85\UNLDDBCU2.COB" -expected ".\test\ref\confidence\COBOL-85\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-85 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\confidence\COBOL-85\P010.txt"

# COBOL-2002
test\bin\run_test.ps1 -json -name report-card-cob2002 -action confidence -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\confidence\COBOL-2002\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2002 -action confidence -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\confidence\COBOL-2002\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2002 -action confidence -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\SalesReport.cob" -expected ".\test\ref\confidence\COBOL-2002\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2002 -action confidence -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\confidence\COBOL-2002\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action confidence -language COBOL-2002 -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\confidence\COBOL-2002\AdventOfCode.txt"

# COBOL-2014
test\bin\run_test.ps1 -json -name report-card-cob2014 -action confidence -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\ReportCard.cob" -expected ".\test\ref\confidence\COBOL-2014\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014 -action confidence -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\Person.cob" -expected ".\test\ref\confidence\COBOL-2014\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014 -action confidence -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\SalesReport.cob" -expected ".\test\ref\confidence\COBOL-2014\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014 -action confidence -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\Report.cob" -expected ".\test\ref\confidence\COBOL-2014\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014 -action confidence -language COBOL-2014 -inputfile ".\test\data\COBOL-2014\AdventOfCode.cob" -expected ".\test\ref\confidence\COBOL-2014\AdventOfCode.txt"

# COBOL 2014 with AcuCobol extensions
test\bin\run_test.ps1 -json -name report-card-cob2014-ACU -action confidence -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\ReportCard.cob" -expected ".\test\ref\confidence\COBOL-2014-ACU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-ACU -action confidence -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\Person.cob" -expected ".\test\ref\confidence\COBOL-2014-ACU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-ACU -action confidence -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\SalesReport.cob" -expected ".\test\ref\confidence\COBOL-2014-ACU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-ACU -action confidence -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\Report.cob" -expected ".\test\ref\confidence\COBOL-2014-ACU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-ACU -action confidence -language COBOL-2014-ACU -inputfile ".\test\data\COBOL-2014-ACU\AdventOfCode.cob" -expected ".\test\ref\confidence\COBOL-2014-ACU\AdventOfCode.txt"

# COBOL 2014 with IBM extensions
test\bin\run_test.ps1 -json -name report-card-cob2014-IBM -action confidence -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\ReportCard.cob" -expected ".\test\ref\confidence\COBOL-2014-IBM\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-IBM -action confidence -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\Person.cob" -expected ".\test\ref\confidence\COBOL-2014-IBM\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-IBM -action confidence -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\SalesReport.cob" -expected ".\test\ref\confidence\COBOL-2014-IBM\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-IBM -action confidence -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\Report.cob" -expected ".\test\ref\confidence\COBOL-2014-IBM\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-IBM -action confidence -language COBOL-2014-IBM -inputfile ".\test\data\COBOL-2014-IBM\AdventOfCode.cob" -expected ".\test\ref\confidence\COBOL-2014-IBM\AdventOfCode.txt"

# COBOL 2014 with GNU extensions
test\bin\run_test.ps1 -json -name report-card-cob2014-GNU -action confidence -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\ReportCard.cob" -expected ".\test\ref\confidence\COBOL-2014-GNU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-GNU -action confidence -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\Person.cob" -expected ".\test\ref\confidence\COBOL-2014-GNU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action confidence -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\confidence\COBOL-2014-GNU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-GNU -action confidence -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\Report.cob" -expected ".\test\ref\confidence\COBOL-2014-GNU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-GNU -action confidence -language COBOL-2014-GNU -inputfile ".\test\data\COBOL-2014-GNU\AdventOfCode.cob" -expected ".\test\ref\confidence\COBOL-2014-GNU\AdventOfCode.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action confidence -language Cplusplus -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\confidence\Cplusplus\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb-cpp -action confidence -language Cplusplus -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\confidence\Cplusplus\hrdb.txt"
test\bin\run_test.ps1 -json -name date-h-cpp -action confidence -language Cplusplus -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\confidence\Cplusplus\date_h.txt"
test\bin\run_test.ps1 -json -name date-cplusplus-cpp -action confidence -language Cplusplus -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\confidence\Cplusplus\date_cpp.txt"
test\bin\run_test.ps1 -json -name inherit-cplusplus-cpp -action confidence -language Cplusplus -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\confidence\Cplusplus\inherit.txt"
test\bin\run_test.ps1 -json -name values-cpp -action confidence -language Cplusplus -inputfile ".\test\data\Cplusplus\values.cpp" -expected ".\test\ref\confidence\Cplusplus\values.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action confidence -language Csharp -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\confidence\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2-cs -action confidence -language Csharp -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\confidence\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3-cs -action confidence -language Csharp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\confidence\Csharp\calculator3.txt"

# FORTRAN-66
test\bin\run_test.ps1 -json -name hello-ftn66 -action confidence -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\confidence\FORTRAN-66\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action confidence -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\confidence\FORTRAN-66\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action confidence -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\confidence\FORTRAN-66\HERON2.txt"
test\bin\run_test.ps1 -json -name heron-wide-ftn66 -action confidence -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON-wide.FOR" -wide -expected ".\test\ref\confidence\FORTRAN-66\HERON-wide.txt"

# FORTRAN-77
test\bin\run_test.ps1 -json -name hello-ftn77 -action confidence -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\confidence\FORTRAN-77\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action confidence -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\confidence\FORTRAN-77\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action confidence -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\confidence\FORTRAN-77\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action confidence -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\confidence\FORTRAN-77\HERON.txt"

# Fortran-90
test\bin\run_test.ps1 -json -name average-ftn90 -action confidence -language Fortran-90 -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\confidence\Fortran-90\average.txt"
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action confidence -language Fortran-90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\confidence\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action confidence -language Fortran-90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\confidence\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action confidence -language Fortran-90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\confidence\Fortran-90\hello.txt"

# Fortran-95
test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action confidence -language Fortran-95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\confidence\Fortran-95\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action confidence -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\confidence\Fortran-95\Wikibooks-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn95 -action confidence -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex2.f95" -expected ".\test\ref\confidence\Fortran-95\Wikibooks-ex2.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn95 -action confidence -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex3.f95" -expected ".\test\ref\confidence\Fortran-95\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn95 -action confidence -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex4.f95" -expected ".\test\ref\confidence\Fortran-95\Wikibooks-ex4.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action confidence -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\confidence\Fortran-2003\geo4060.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn2003 -action confidence -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\temp_sub.for" -expected ".\test\ref\confidence\Fortran-2003\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn2003 -action confidence -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\temp_func.for" -expected ".\test\ref\confidence\Fortran-2003\temp_func.txt"

# Fortran-2008
test\bin\run_test.ps1 -json -name geo4060-ftn2008 -action confidence -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\geo4060.for" -expected ".\test\ref\confidence\Fortran-2008\geo4060.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn2008 -action confidence -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\temp_sub.for" -expected ".\test\ref\confidence\Fortran-2008\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn2008 -action confidence -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\temp_func.for" -expected ".\test\ref\confidence\Fortran-2008\temp_func.txt"

# F#
test\bin\run_test.ps1 -json -name samples-fs -action confidence -language Fsharp -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\confidence\Fsharp\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action confidence -language Go -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\confidence\Go\find-cli.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action confidence -language HTML -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\confidence\HTML\knuth.txt"
test\bin\run_test.ps1 -json -name developer-html -action confidence -language HTML -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\confidence\HTML\developer-css.txt"
test\bin\run_test.ps1 -json -name codestat-html -action confidence -language HTML -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\confidence\HTML\codestat-css-javascript.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action confidence -language Java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\confidence\Java\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action confidence -language Java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\confidence\Java\palindrome.txt"
test\bin\run_test.ps1 -json -name binary-search-java -action confidence -language Java -inputfile ".\test\data\Java\binary_search.java" -expected ".\test\ref\confidence\Java\binary_search.txt"
test\bin\run_test.ps1 -json -name ObjectServer-java -action confidence -language Java -inputfile ".\test\data\Java\ObjectServer.java" -expected ".\test\ref\confidence\Java\ObjectServer.txt"
test\bin\run_test.ps1 -json -name backtick-js -action confidence -language JavaScript -inputfile ".\test\data\JavaScript\backtick.js" -expected ".\test\ref\confidence\JavaScript\backtick.txt"

# JavaScript
test\bin\run_test.ps1 -json -name values-js -action confidence -language JavaScript -inputfile ".\test\data\JavaScript\values.js" -expected ".\test\ref\confidence\JavaScript\values.txt"
test\bin\run_test.ps1 -json -name codestat-js -action confidence -language JavaScript -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\confidence\JavaScript\codestat.txt"
test\bin\run_test.ps1 -json -name bing-js -action confidence -language JavaScript -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\confidence\JavaScript\bing.txt"
test\bin\run_test.ps1 -json -name calc_prime-js -action confidence -language JavaScript -inputfile ".\test\data\JavaScript\calc_prime.js" -expected ".\test\ref\confidence\JavaScript\calc_prime.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action confidence -language Objective-C -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\confidence\Objective-C\HelloWorld.txt"
test\bin\run_test.ps1 -json -name qrmath-objc -action confidence -language Objective-C -inputfile ".\test\data\Objective-C\QRMath.h" -expected ".\test\ref\confidence\Objective-C\QRMath.txt"
test\bin\run_test.ps1 -json -name qrencoder-objc -action confidence -language Objective-C -inputfile ".\test\data\Objective-C\QREncoder.m" -expected ".\test\ref\confidence\Objective-C\QREncoder.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework-pas -action confidence -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\confidence\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello-pas -action confidence -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\confidence\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose-pas -action confidence -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\confidence\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider-pas -action confidence -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\confidence\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16-pas -action confidence -language Pascal -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\confidence\Pascal\TPC16.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action confidence -language PL1-Fixed -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\confidence\PL1\BPGPLI.txt"
test\bin\run_test.ps1 -json -name checkdt-pl1 -action confidence -language PL1-Free -inputfile ".\test\data\PL1\CHECKDT.pl1" -expected ".\test\ref\confidence\PL1\CHECKDT.txt"
test\bin\run_test.ps1 -json -name mainfact-pl1 -action confidence -language PL1 -inputfile ".\test\data\PL1\MAINFACT.pl1" -expected ".\test\ref\confidence\PL1\MAINFACT.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main-prolog -action confidence -language Prolog -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\confidence\Prolog\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb-prolog -action confidence -language Prolog -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\confidence\Prolog\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu-prolog -action confidence -language Prolog -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\confidence\Prolog\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries-prolog -action confidence -language Prolog -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\confidence\Prolog\family-queries.txt"
test\bin\run_test.ps1 -json -name web-server-hello-prolog -action confidence -language Prolog -inputfile ".\test\data\Prolog\web-server-hello.pl" -expected ".\test\ref\confidence\Prolog\web-server-hello.txt"
test\bin\run_test.ps1 -json -name web-server-params-prolog -action confidence -language Prolog -inputfile ".\test\data\Prolog\web-server-params.pl" -expected ".\test\ref\confidence\Prolog\web-server-params.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d-python -action confidence -language Python -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\confidence\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor-python -action confidence -language Python -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\confidence\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory-python -action confidence -language Python -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\confidence\Python\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-python -action confidence -language Python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\confidence\Python\values.txt"
test\bin\run_test.ps1 -json -name examiner-python -action confidence -language Python -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\confidence\Python\Examiner.txt"
test\bin\run_test.ps1 -json -name authorized-view-python -action confidence -language Python -inputfile ".\test\data\Python\authorized_view.py" -expected ".\test\ref\confidence\Python\authorized_view.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01-r -action confidence -language R -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\confidence\R\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02-r -action confidence -language R -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\confidence\R\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03-r -action confidence -language R -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\confidence\R\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04-r -action confidence -language R -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\confidence\R\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05-r -action confidence -language R -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\confidence\R\ETM-540-05.txt"

# Ruby
test\bin\run_test.ps1 -json -name basic-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\confidence\Ruby\basic.txt"
test\bin\run_test.ps1 -json -name constants-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\constants.rb" -expected ".\test\ref\confidence\Ruby\constants.txt"
test\bin\run_test.ps1 -json -name exceptions-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\exceptions.rb" -expected ".\test\ref\confidence\Ruby\exceptions.txt"
test\bin\run_test.ps1 -json -name expressions-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\expressions.rb" -expected ".\test\ref\confidence\Ruby\expressions.txt"
test\bin\run_test.ps1 -json -name functions-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\functions.rb" -expected ".\test\ref\confidence\Ruby\functions.txt"
test\bin\run_test.ps1 -json -name io-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\io.rb" -expected ".\test\ref\confidence\Ruby\io.txt"
test\bin\run_test.ps1 -json -name modifiers-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\modifiers.rb" -expected ".\test\ref\confidence\Ruby\modifiers.txt"
test\bin\run_test.ps1 -json -name operators-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\operators.rb" -expected ".\test\ref\confidence\Ruby\operators.txt"
test\bin\run_test.ps1 -json -name statements-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\statements.rb" -expected ".\test\ref\confidence\Ruby\statements.txt"
test\bin\run_test.ps1 -json -name tokenbuilders-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\tokenbuilders.rb" -expected ".\test\ref\confidence\Ruby\tokenbuilders.txt"
test\bin\run_test.ps1 -json -name tokenizers-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\tokenizers.rb" -expected ".\test\ref\confidence\Ruby\tokenizers.txt"
test\bin\run_test.ps1 -json -name tokens-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\tokens.rb" -expected ".\test\ref\confidence\Ruby\tokens.txt"
test\bin\run_test.ps1 -json -name webhook-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\confidence\Ruby\webhook.txt"

# Rust
test\bin\run_test.ps1 -json -name dom-rust -action confidence -language Rust -inputfile ".\test\data\Rust\dom.rs" -expected ".\test\ref\confidence\Rust\dom.txt"
test\bin\run_test.ps1 -json -name html-rust -action confidence -language Rust -inputfile ".\test\data\Rust\html.rs" -expected ".\test\ref\confidence\Rust\html.txt"
test\bin\run_test.ps1 -json -name geometry-rust -action confidence -language Rust -inputfile ".\test\data\Rust\geometry.rs" -expected ".\test\ref\confidence\Rust\geometry.txt"
test\bin\run_test.ps1 -json -name scene-rust -action confidence -language Rust -inputfile ".\test\data\Rust\scene.rs" -expected ".\test\ref\confidence\Rust\scene.txt"
test\bin\run_test.ps1 -json -name comments-1-rust -action confidence -language Rust -inputfile ".\test\data\Rust\nested_comments_1.rs" -expected ".\test\ref\confidence\Rust\nested_comments_1.txt"
test\bin\run_test.ps1 -json -name comments-2-rust -action confidence -language Rust -inputfile ".\test\data\Rust\nested_comments_2.rs" -expected ".\test\ref\confidence\Rust\nested_comments_2.txt"
test\bin\run_test.ps1 -json -name comments-3-rust -action confidence -language Rust -inputfile ".\test\data\Rust\nested_comments_3.rs" -expected ".\test\ref\confidence\Rust\nested_comments_3.txt"
test\bin\run_test.ps1 -json -name raw-string-1-rust -action confidence -language Rust -inputfile ".\test\data\Rust\raw_string_1.rs" -expected ".\test\ref\confidence\Rust\raw_string_1.txt"
test\bin\run_test.ps1 -json -name raw-string-2-rust -action confidence -language Rust -inputfile ".\test\data\Rust\raw_string_2.rs" -expected ".\test\ref\confidence\Rust\raw_string_2.txt"
test\bin\run_test.ps1 -json -name attributes-1-rust -action confidence -language Rust -inputfile ".\test\data\Rust\attributes_1.rs" -expected ".\test\ref\confidence\Rust\attributes_1.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action confidence -language SQL-92 -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\confidence\SQL-92\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action confidence -language SQL-92 -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\confidence\SQL-92\table.txt"

# SQL-99
test\bin\run_test.ps1 -json -name microsoft-sql-99 -action confidence -language SQL-99 -inputfile ".\test\data\SQL-99\microsoft.sql" -expected ".\test\ref\confidence\SQL-99\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-99 -action confidence -language SQL-99 -inputfile ".\test\data\SQL-99\table.sql" -expected ".\test\ref\confidence\SQL-99\table.txt"

# SQL-2003
test\bin\run_test.ps1 -json -name microsoft-sql-2003 -action confidence -language SQL-2003 -inputfile ".\test\data\SQL-2003\microsoft.sql" -expected ".\test\ref\confidence\SQL-2003\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2003 -action confidence -language SQL-2003 -inputfile ".\test\data\SQL-2003\table.sql" -expected ".\test\ref\confidence\SQL-2003\table.txt"

# SQL-2008
test\bin\run_test.ps1 -json -name microsoft-sql-2008 -action confidence -language SQL-2008 -inputfile ".\test\data\SQL-2008\microsoft.sql" -expected ".\test\ref\confidence\SQL-2008\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2008 -action confidence -language SQL-2008 -inputfile ".\test\data\SQL-2008\table.sql" -expected ".\test\ref\confidence\SQL-2008\table.txt"

# SQL-2011
test\bin\run_test.ps1 -json -name microsoft-sql-2011 -action confidence -language SQL-2011 -inputfile ".\test\data\SQL-2011\microsoft.sql" -expected ".\test\ref\confidence\SQL-2011\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2011 -action confidence -language SQL-2011 -inputfile ".\test\data\SQL-2011\table.sql" -expected ".\test\ref\confidence\SQL-2011\table.txt"

# SQL-2016
test\bin\run_test.ps1 -json -name microsoft-sql-2016 -action confidence -language SQL-2016 -inputfile ".\test\data\SQL-2016\microsoft.sql" -expected ".\test\ref\confidence\SQL-2016\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2016 -action confidence -language SQL-2016 -inputfile ".\test\data\SQL-2016\table.sql" -expected ".\test\ref\confidence\SQL-2016\table.txt"

# Swift
test\bin\run_test.ps1 -json -name AppDelegate -action confidence -language Swift -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\confidence\Swift\AppDelegate.txt"
test\bin\run_test.ps1 -json -name Meal -action confidence -language Swift -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\confidence\Swift\Meal.txt"
test\bin\run_test.ps1 -json -name MealTableViewCell -action confidence -language Swift -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\confidence\Swift\MealTableViewCell.txt"
test\bin\run_test.ps1 -json -name MealTableViewController -action confidence -language Swift -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\confidence\Swift\MealTableViewController.txt"
test\bin\run_test.ps1 -json -name MealViewController -action confidence -language Swift -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\confidence\Swift\MealViewController.txt"
test\bin\run_test.ps1 -json -name RatingControl -action confidence -language Swift -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\confidence\Swift\RatingControl.txt"
test\bin\run_test.ps1 -json -name URLExtensions -action confidence -language Swift -inputfile ".\test\data\Swift\URLExtensions.swift" -expected ".\test\ref\confidence\Swift\URLExtensions.txt"

# TypeScript
test\bin\run_test.ps1 -json -name TimeReporter-ts -action confidence -language TypeScript -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\confidence\TypeScript\TimeReporter.txt"
test\bin\run_test.ps1 -json -name ImageBoard-ts -action confidence -language TypeScript -inputfile ".\test\data\TypeScript\ImageBoard.ts" -expected ".\test\ref\confidence\TypeScript\ImageBoard.txt"

# Visual Basic 6
test\bin\run_test.ps1 -json -name spider-vb6 -action confidence -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\confidence\VisualBasic-6\spider.txt"
test\bin\run_test.ps1 -json -name azure_metadata-vb6 -action confidence -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\azure_metadata.bas" -expected ".\test\ref\confidence\VisualBasic-6\azure_metadata.txt"
test\bin\run_test.ps1 -json -name diffie_hellman-vb6 -action confidence -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\diffie_hellman.bas" -expected ".\test\ref\confidence\VisualBasic-6\diffie_hellman.txt"

# Visual Basic .NET
test\bin\run_test.ps1 -json -name word-processor-vbnet -action confidence -language VisualBasic-NET -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\confidence\VisualBasic-NET\WordProcessor.txt"

# polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c -action confidence -language C -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence\C\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action confidence -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action confidence -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence\Pascal\polyglot.txt"

Write-Output "Number of failures: $failures"
