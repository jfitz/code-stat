Set-StrictMode -Version Latest

[int]$failures = 0

# BASIC
test\bin\run_test.ps1 -json -name values -action statistics -language BASIC -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\statistics\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple -action statistics -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\statistics\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot -action statistics -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\statistics\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum -action statistics -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\statistics\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life -action statistics -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\statistics\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income -action statistics -language BASIC -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\statistics\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2 -action statistics -language BASIC -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\statistics\BASIC\rockt2.txt"

# C
test\bin\run_test.ps1 -json -name diamond -action statistics -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\statistics\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test -action statistics -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\statistics\C\prime_test.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action statistics -language CBASIC -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\statistics\CBASIC\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action statistics -language CBASIC -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\statistics\CBASIC\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action statistics -language CBASIC -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\statistics\CBASIC\GRAPHR.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken1-68 -action statistics -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\statistics\COBOL-68\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action statistics -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\statistics\COBOL-68\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action statistics -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\statistics\COBOL-68\mccracken3.txt"

# COBOL-74
test\bin\run_test.ps1 -json -name prog1-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG1.COB" -expected ".\test\ref\statistics\COBOL-74\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG2.COB" -expected ".\test\ref\statistics\COBOL-74\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG2A.COB" -expected ".\test\ref\statistics\COBOL-74\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG3.COB" -expected ".\test\ref\statistics\COBOL-74\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG4.COB" -expected ".\test\ref\statistics\COBOL-74\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG5.COB" -expected ".\test\ref\statistics\COBOL-74\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG6.COB" -expected ".\test\ref\statistics\COBOL-74\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG12-2.COB" -expected ".\test\ref\statistics\COBOL-74\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG13-3.COB" -expected ".\test\ref\statistics\COBOL-74\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG14-2.COB" -expected ".\test\ref\statistics\COBOL-74\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG15-4.COB" -expected ".\test\ref\statistics\COBOL-74\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-74 -action statistics -language COBOL-74 -tabsize 4 -inputfile ".\test\data\COBOL-74\UNLDDBCU2.COB" -expected ".\test\ref\statistics\COBOL-74\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-wide-74 -action statistics -language COBOL-74 -inputfile ".\test\data\COBOL-74\P010-wide.COB" -wide -expected ".\test\ref\statistics\COBOL-74\P010-wide.txt"

# COBOL-85
test\bin\run_test.ps1 -json -name prog1-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG1.COB" -expected ".\test\ref\statistics\COBOL-85\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG2.COB" -expected ".\test\ref\statistics\COBOL-85\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG2A.COB" -expected ".\test\ref\statistics\COBOL-85\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG3.COB" -expected ".\test\ref\statistics\COBOL-85\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG4.COB" -expected ".\test\ref\statistics\COBOL-85\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG5.COB" -expected ".\test\ref\statistics\COBOL-85\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG6.COB" -expected ".\test\ref\statistics\COBOL-85\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG12-2.COB" -expected ".\test\ref\statistics\COBOL-85\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG13-3.COB" -expected ".\test\ref\statistics\COBOL-85\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG14-2.COB" -expected ".\test\ref\statistics\COBOL-85\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\statistics\COBOL-85\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-85 -action statistics -language COBOL-85 -tabsize 4 -inputfile ".\test\data\COBOL-85\UNLDDBCU2.COB" -expected ".\test\ref\statistics\COBOL-85\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-85 -action statistics -language COBOL-85 -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\statistics\COBOL-85\P010.txt"

# COBOL-2002
test\bin\run_test.ps1 -json -name report-card-cob2002 -action statistics -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\statistics\COBOL-2002\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2002 -action statistics -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\statistics\COBOL-2002\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2002 -action statistics -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\SalesReport.cob" -expected ".\test\ref\statistics\COBOL-2002\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2002 -action statistics -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\statistics\COBOL-2002\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action statistics -language COBOL-2002 -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\statistics\COBOL-2002\AdventOfCode.txt"

# COBOL-2014
test\bin\run_test.ps1 -json -name report-card-cob2014 -action statistics -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\ReportCard.cob" -expected ".\test\ref\statistics\COBOL-2014\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014 -action statistics -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\Person.cob" -expected ".\test\ref\statistics\COBOL-2014\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014 -action statistics -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\SalesReport.cob" -expected ".\test\ref\statistics\COBOL-2014\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014 -action statistics -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\Report.cob" -expected ".\test\ref\statistics\COBOL-2014\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014 -action statistics -language COBOL-2014 -inputfile ".\test\data\COBOL-2014\AdventOfCode.cob" -expected ".\test\ref\statistics\COBOL-2014\AdventOfCode.txt"

# COBOL-2014 with AcuCobol extensions
test\bin\run_test.ps1 -json -name report-card-cob2014-ACU -action statistics -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\ReportCard.cob" -expected ".\test\ref\statistics\COBOL-2014-ACU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-ACU -action statistics -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\Person.cob" -expected ".\test\ref\statistics\COBOL-2014-ACU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-ACU -action statistics -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\SalesReport.cob" -expected ".\test\ref\statistics\COBOL-2014-ACU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-ACU -action statistics -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\Report.cob" -expected ".\test\ref\statistics\COBOL-2014-ACU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-ACU -action statistics -language COBOL-2014-ACU -inputfile ".\test\data\COBOL-2014-ACU\AdventOfCode.cob" -expected ".\test\ref\statistics\COBOL-2014-ACU\AdventOfCode.txt"

# COBOL-2014 with IBM extensions
test\bin\run_test.ps1 -json -name report-card-cob2014-IBM -action statistics -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\ReportCard.cob" -expected ".\test\ref\statistics\COBOL-2014-IBM\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-IBM -action statistics -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\Person.cob" -expected ".\test\ref\statistics\COBOL-2014-IBM\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-IBM -action statistics -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\SalesReport.cob" -expected ".\test\ref\statistics\COBOL-2014-IBM\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-IBM -action statistics -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\Report.cob" -expected ".\test\ref\statistics\COBOL-2014-IBM\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-IBM -action statistics -language COBOL-2014-IBM -inputfile ".\test\data\COBOL-2014-IBM\AdventOfCode.cob" -expected ".\test\ref\statistics\COBOL-2014-IBM\AdventOfCode.txt"

# COBOL-2014 with GNU extensions
test\bin\run_test.ps1 -json -name report-card-cob2014-GNU -action statistics -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\ReportCard.cob" -expected ".\test\ref\statistics\COBOL-2014-GNU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-GNU -action statistics -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\Person.cob" -expected ".\test\ref\statistics\COBOL-2014-GNU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action statistics -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\statistics\COBOL-2014-GNU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-GNU -action statistics -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\Report.cob" -expected ".\test\ref\statistics\COBOL-2014-GNU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-GNU -action statistics -language COBOL-2014-GNU -inputfile ".\test\data\COBOL-2014-GNU\AdventOfCode.cob" -expected ".\test\ref\statistics\COBOL-2014-GNU\AdventOfCode.txt"

# C++
test\bin\run_test.ps1 -json -name checkers -action statistics -language Cpluplus -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\statistics\Cplusplus\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\statistics\Cplusplus\hrdb.txt"
test\bin\run_test.ps1 -json -name date_h -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\statistics\Cplusplus\date_h.txt"
test\bin\run_test.ps1 -json -name date_cplusplus -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\statistics\Cplusplus\date_cpp.txt"
test\bin\run_test.ps1 -json -name inherit_cplusplus -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\statistics\Cplusplus\inherit.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1 -action statistics -language Csharp -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\statistics\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2 -action statistics -language Csharp -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\statistics\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3 -action statistics -language Csharp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\statistics\Csharp\calculator3.txt"

# FORTRAN-66
test\bin\run_test.ps1 -json -name hello-ftn66 -action statistics -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\statistics\FORTRAN-66\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action statistics -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\statistics\FORTRAN-66\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action statistics -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\statistics\FORTRAN-66\HERON2.txt"
test\bin\run_test.ps1 -json -name heron-wide-ftn66 -action statistics -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON-wide.FOR" -wide -expected ".\test\ref\statistics\FORTRAN-66\HERON-wide.txt"

# FORTRAN-77
test\bin\run_test.ps1 -json -name hello-ftn77 -action statistics -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\statistics\FORTRAN-77\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action statistics -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\statistics\FORTRAN-77\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action statistics -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\statistics\FORTRAN-77\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action statistics -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\statistics\FORTRAN-77\HERON.txt"

# Fortran-90
test\bin\run_test.ps1 -json -name average-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\statistics\Fortran-90\average.txt"
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\statistics\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\statistics\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\statistics\Fortran-90\hello.txt"

# Fortran-95
test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action statistics -language Fortran-95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\statistics\Fortran-95\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action statistics -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\statistics\Fortran-95\Wikibooks-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn95 -action statistics -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex2.f95" -expected ".\test\ref\statistics\Fortran-95\Wikibooks-ex2.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn95 -action statistics -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex3.f95" -expected ".\test\ref\statistics\Fortran-95\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn95 -action statistics -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex4.f95" -expected ".\test\ref\statistics\Fortran-95\Wikibooks-ex4.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action statistics -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\statistics\Fortran-2003\geo4060.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn2003 -action statistics -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\temp_sub.for" -expected ".\test\ref\statistics\Fortran-2003\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn2003 -action statistics -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\temp_func.for" -expected ".\test\ref\statistics\Fortran-2003\temp_func.txt"

# Fortran-2008
test\bin\run_test.ps1 -json -name geo4060-ftn2008 -action statistics -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\geo4060.for" -expected ".\test\ref\statistics\Fortran-2008\geo4060.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn2008 -action statistics -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\temp_sub.for" -expected ".\test\ref\statistics\Fortran-2008\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn2008 -action statistics -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\temp_func.for" -expected ".\test\ref\statistics\Fortran-2008\temp_func.txt"

# F#
test\bin\run_test.ps1 -json -name samples-fs -action statistics -language Fsharp -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\statistics\Fsharp\samples.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action statistics -language HTML -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\statistics\HTML\knuth.txt"
test\bin\run_test.ps1 -json -name developer-html -action statistics -language HTML -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\statistics\HTML\developer-css.txt"
test\bin\run_test.ps1 -json -name codestat-html -action statistics -language HTML -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\statistics\HTML\codestat-css-javascript.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action statistics -language Java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\statistics\Java\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action statistics -language Java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\statistics\Java\palindrome.txt"

# JavaScript
test\bin\run_test.ps1 -json -name values-js -action statistics -language JavaScript -inputfile ".\test\data\JavaScript\values.js" -expected ".\test\ref\statistics\JavaScript\values.txt"
test\bin\run_test.ps1 -json -name codestat-js -action statistics -language JavaScript -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\statistics\JavaScript\codestat.txt"
test\bin\run_test.ps1 -json -name bing-js -action statistics -language JavaScript -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\statistics\JavaScript\bing.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action statistics -language Objective-C -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\statistics\Objective-C\HelloWorld.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework -action statistics -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\statistics\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello -action statistics -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\statistics\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose -action statistics -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\statistics\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider -action statistics -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\statistics\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16 -action statistics -language Pascal -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\statistics\Pascal\TPC16.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main -action statistics -language Prolog -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\statistics\Prolog\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb -action statistics -language Prolog -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\statistics\Prolog\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu -action statistics -language Prolog -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\statistics\Prolog\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries -action statistics -language Prolog -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\statistics\Prolog\family-queries.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d -action statistics -language Python -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\statistics\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor -action statistics -language Python -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\statistics\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory -action statistics -language Python -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\statistics\Python\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-python -action statistics -language Python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\statistics\Python\values.txt"
test\bin\run_test.ps1 -json -name examiner -action statistics -language Python -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\statistics\Python\Examiner.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01 -action statistics -language R -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\statistics\R\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02 -action statistics -language R -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\statistics\R\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03 -action statistics -language R -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\statistics\R\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04 -action statistics -language R -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\statistics\R\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action statistics -language R -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\statistics\R\ETM-540-05.txt"

# Ruby
test\bin\run_test.ps1 -json -name basic-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\statistics\Ruby\basic.txt"
test\bin\run_test.ps1 -json -name constants-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\constants.rb" -expected ".\test\ref\statistics\Ruby\constants.txt"
test\bin\run_test.ps1 -json -name exceptions-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\exceptions.rb" -expected ".\test\ref\statistics\Ruby\exceptions.txt"
test\bin\run_test.ps1 -json -name expressions-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\expressions.rb" -expected ".\test\ref\statistics\Ruby\expressions.txt"
test\bin\run_test.ps1 -json -name functions-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\functions.rb" -expected ".\test\ref\statistics\Ruby\functions.txt"
test\bin\run_test.ps1 -json -name io-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\io.rb" -expected ".\test\ref\statistics\Ruby\io.txt"
test\bin\run_test.ps1 -json -name modifiers-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\modifiers.rb" -expected ".\test\ref\statistics\Ruby\modifiers.txt"
test\bin\run_test.ps1 -json -name operators-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\operators.rb" -expected ".\test\ref\statistics\Ruby\operators.txt"
test\bin\run_test.ps1 -json -name statements-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\statements.rb" -expected ".\test\ref\statistics\Ruby\statements.txt"
test\bin\run_test.ps1 -json -name tokenbuilders-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\tokenbuilders.rb" -expected ".\test\ref\statistics\Ruby\tokenbuilders.txt"
test\bin\run_test.ps1 -json -name tokenizers-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\tokenizers.rb" -expected ".\test\ref\statistics\Ruby\tokenizers.txt"
test\bin\run_test.ps1 -json -name tokens-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\tokens.rb" -expected ".\test\ref\statistics\Ruby\tokens.txt"
test\bin\run_test.ps1 -json -name webhook-ruby -action statistics -language Ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\statistics\Ruby\webhook.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action statistics -language SQL-92 -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\statistics\SQL-92\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action statistics -language SQL-92 -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\statistics\SQL-92\table.txt"

# SQL-99
test\bin\run_test.ps1 -json -name microsoft-sql-99 -action statistics -language SQL-99 -inputfile ".\test\data\SQL-99\microsoft.sql" -expected ".\test\ref\statistics\SQL-99\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-99 -action statistics -language SQL-99 -inputfile ".\test\data\SQL-99\table.sql" -expected ".\test\ref\statistics\SQL-99\table.txt"

# SQL-2003
test\bin\run_test.ps1 -json -name microsoft-sql-2003 -action statistics -language SQL-2003 -inputfile ".\test\data\SQL-2003\microsoft.sql" -expected ".\test\ref\statistics\SQL-2003\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2003 -action statistics -language SQL-2003 -inputfile ".\test\data\SQL-2003\table.sql" -expected ".\test\ref\statistics\SQL-2003\table.txt"

# SQL-2008
test\bin\run_test.ps1 -json -name microsoft-sql-2008 -action statistics -language SQL-2008 -inputfile ".\test\data\SQL-2008\microsoft.sql" -expected ".\test\ref\statistics\SQL-2008\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2008 -action statistics -language SQL-2008 -inputfile ".\test\data\SQL-2008\table.sql" -expected ".\test\ref\statistics\SQL-2008\table.txt"

# SQL-2011
test\bin\run_test.ps1 -json -name microsoft-sql-2011 -action statistics -language SQL-2011 -inputfile ".\test\data\SQL-2011\microsoft.sql" -expected ".\test\ref\statistics\SQL-2011\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2011 -action statistics -language SQL-2011 -inputfile ".\test\data\SQL-2011\table.sql" -expected ".\test\ref\statistics\SQL-2011\table.txt"

# SQL-2016
test\bin\run_test.ps1 -json -name microsoft-sql-2016 -action statistics -language SQL-2016 -inputfile ".\test\data\SQL-2016\microsoft.sql" -expected ".\test\ref\statistics\SQL-2016\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2016 -action statistics -language SQL-2016 -inputfile ".\test\data\SQL-2016\table.sql" -expected ".\test\ref\statistics\SQL-2016\table.txt"

# Swift
test\bin\run_test.ps1 -json -name AppDelegate -action statistics -language Swift -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\statistics\Swift\AppDelegate.txt"
test\bin\run_test.ps1 -json -name Meal -action statistics -language Swift -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\statistics\Swift\Meal.txt"
test\bin\run_test.ps1 -json -name MealTableViewCell -action statistics -language Swift -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\statistics\Swift\MealTableViewCell.txt"
test\bin\run_test.ps1 -json -name MealTableViewController -action statistics -language Swift -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\statistics\Swift\MealTableViewController.txt"
test\bin\run_test.ps1 -json -name MealViewController -action statistics -language Swift -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\statistics\Swift\MealViewController.txt"
test\bin\run_test.ps1 -json -name RatingControl -action statistics -language Swift -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\statistics\Swift\RatingControl.txt"
test\bin\run_test.ps1 -json -name URLExtensions -action statistics -language Swift -inputfile ".\test\data\Swift\URLExtensions.swift" -expected ".\test\ref\statistics\Swift\URLExtensions.txt"

# TypeScript
test\bin\run_test.ps1 -json -name TimeReporter-ts -action statistics -language TypeScript -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\statistics\TypeScript\TimeReporter.txt"
test\bin\run_test.ps1 -json -name ImageBoard-ts -action statistics -language TypeScript -inputfile ".\test\data\TypeScript\ImageBoard.ts" -expected ".\test\ref\statistics\TypeScript\ImageBoard.txt"

# Visual Basic 6
test\bin\run_test.ps1 -json -name spider-vb6 -action statistics -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\statistics\VisualBasic-6\spider.txt"
test\bin\run_test.ps1 -json -name azure_metadata-vb6 -action statistics -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\azure_metadata.bas" -expected ".\test\ref\statistics\VisualBasic-6\azure_metadata.txt"
test\bin\run_test.ps1 -json -name diffie_hellman-vb6 -action statistics -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\diffie_hellman.bas" -expected ".\test\ref\statistics\VisualBasic-6\diffie_hellman.txt"

# Visual Basic .NET
test\bin\run_test.ps1 -json -name word-processor-vbnet -action statistics -language VisualBasic-NET -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\statistics\VisualBasic-NET\WordProcessor.txt"

# polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c -action statistics -language C -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\statistics\C\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action statistics -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\statistics\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action statistics -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\statistics\Pascal\polyglot.txt"

Write-Output "Number of failures: $failures"
