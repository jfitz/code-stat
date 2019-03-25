Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -json -name values -action tokens -language BASIC -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\tokens\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple -action tokens -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\tokens\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot -action tokens -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\tokens\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum -action tokens -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\tokens\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life -action tokens -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\tokens\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income -action tokens -language BASIC -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\tokens\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2 -action tokens -language BASIC -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\tokens\BASIC\rockt2.txt"

test\bin\run_test.ps1 -json -name diamond -action tokens -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\tokens\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test -action tokens -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\tokens\C\prime_test.txt"

test\bin\run_test.ps1 -json -name fibo -action tokens -language CBASIC -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\tokens\CBASIC\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action tokens -language CBASIC -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\tokens\CBASIC\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action tokens -language CBASIC -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\tokens\CBASIC\GRAPHR.txt"

test\bin\run_test.ps1 -json -name mccracken1-68 -action tokens -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\tokens\COBOL-68\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action tokens -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\tokens\COBOL-68\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action tokens -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\tokens\COBOL-68\mccracken3.txt"

test\bin\run_test.ps1 -json -name prog1-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG1.COB" -expected ".\test\ref\tokens\COBOL-74\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG2.COB" -expected ".\test\ref\tokens\COBOL-74\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG2A.COB" -expected ".\test\ref\tokens\COBOL-74\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG3.COB" -expected ".\test\ref\tokens\COBOL-74\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG4.COB" -expected ".\test\ref\tokens\COBOL-74\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG5.COB" -expected ".\test\ref\tokens\COBOL-74\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG6.COB" -expected ".\test\ref\tokens\COBOL-74\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG12-2.COB" -expected ".\test\ref\tokens\COBOL-74\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG13-3.COB" -expected ".\test\ref\tokens\COBOL-74\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG14-2.COB" -expected ".\test\ref\tokens\COBOL-74\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\PROG15-4.COB" -expected ".\test\ref\tokens\COBOL-74\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\tokens\COBOL-74\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-wide-74 -action tokens -language COBOL-74 -inputfile ".\test\data\COBOL-74\P010-wide.COB" -wide -expected ".\test\ref\tokens\COBOL-74\P010-wide.txt"

test\bin\run_test.ps1 -json -name prog1-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG1.COB" -expected ".\test\ref\tokens\COBOL-85\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG2.COB" -expected ".\test\ref\tokens\COBOL-85\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG2A.COB" -expected ".\test\ref\tokens\COBOL-85\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG3.COB" -expected ".\test\ref\tokens\COBOL-85\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG4.COB" -expected ".\test\ref\tokens\COBOL-85\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG5.COB" -expected ".\test\ref\tokens\COBOL-85\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG6.COB" -expected ".\test\ref\tokens\COBOL-85\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG12-2.COB" -expected ".\test\ref\tokens\COBOL-85\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG13-3.COB" -expected ".\test\ref\tokens\COBOL-85\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG14-2.COB" -expected ".\test\ref\tokens\COBOL-85\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\tokens\COBOL-85\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\tokens\COBOL-85\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-85 -action tokens -language COBOL-85 -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\tokens\COBOL-85\P010.txt"

test\bin\run_test.ps1 -json -name report-card-cob2002 -action tokens -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\tokens\COBOL-2002\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2002 -action tokens -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\tokens\COBOL-2002\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2002 -action tokens -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\SalesReport.cob" -expected ".\test\ref\tokens\COBOL-2002\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2002 -action tokens -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\tokens\COBOL-2002\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action tokens -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\tokens\COBOL-2002\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name report-card-cob2014 -action tokens -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\ReportCard.cob" -expected ".\test\ref\tokens\COBOL-2014\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014 -action tokens -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\Person.cob" -expected ".\test\ref\tokens\COBOL-2014\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014 -action tokens -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\SalesReport.cob" -expected ".\test\ref\tokens\COBOL-2014\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014 -action tokens -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\Report.cob" -expected ".\test\ref\tokens\COBOL-2014\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014 -action tokens -language COBOL-2014 -tabsize 4 -inputfile ".\test\data\COBOL-2014\AdventOfCode.cob" -expected ".\test\ref\tokens\COBOL-2014\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name report-card-cob2014-ACU -action tokens -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\ReportCard.cob" -expected ".\test\ref\tokens\COBOL-2014-ACU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-ACU -action tokens -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\Person.cob" -expected ".\test\ref\tokens\COBOL-2014-ACU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-ACU -action tokens -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\SalesReport.cob" -expected ".\test\ref\tokens\COBOL-2014-ACU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-ACU -action tokens -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\Report.cob" -expected ".\test\ref\tokens\COBOL-2014-ACU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-ACU -action tokens -language COBOL-2014-ACU -tabsize 4 -inputfile ".\test\data\COBOL-2014-ACU\AdventOfCode.cob" -expected ".\test\ref\tokens\COBOL-2014-ACU\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name report-card-cob2014-IBM -action tokens -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\ReportCard.cob" -expected ".\test\ref\tokens\COBOL-2014-IBM\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-IBM -action tokens -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\Person.cob" -expected ".\test\ref\tokens\COBOL-2014-IBM\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-IBM -action tokens -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\SalesReport.cob" -expected ".\test\ref\tokens\COBOL-2014-IBM\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-IBM -action tokens -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\Report.cob" -expected ".\test\ref\tokens\COBOL-2014-IBM\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-IBM -action tokens -language COBOL-2014-IBM -tabsize 4 -inputfile ".\test\data\COBOL-2014-IBM\AdventOfCode.cob" -expected ".\test\ref\tokens\COBOL-2014-IBM\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name report-card-cob2014-GNU -action tokens -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\ReportCard.cob" -expected ".\test\ref\tokens\COBOL-2014-GNU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-GNU -action tokens -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\Person.cob" -expected ".\test\ref\tokens\COBOL-2014-GNU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action tokens -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\tokens\COBOL-2014-GNU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-GNU -action tokens -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\Report.cob" -expected ".\test\ref\tokens\COBOL-2014-GNU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-GNU -action tokens -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\AdventOfCode.cob" -expected ".\test\ref\tokens\COBOL-2014-GNU\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name checkers -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\tokens\Cplusplus\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\tokens\Cplusplus\hrdb.txt"
test\bin\run_test.ps1 -json -name date_h -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\tokens\Cplusplus\date_h.txt"
test\bin\run_test.ps1 -json -name date_cplusplus -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\tokens\Cplusplus\date_cpp.txt"
test\bin\run_test.ps1 -json -name inherit_cplusplus -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\tokens\Cplusplus\inherit.txt"

test\bin\run_test.ps1 -json -name calculator1 -action tokens -language Csharp -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\tokens\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2 -action tokens -language Csharp -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\tokens\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3 -action tokens -language Csharp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\tokens\Csharp\calculator3.txt"

test\bin\run_test.ps1 -json -name hello-ftn66 -action tokens -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\tokens\FORTRAN-66\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action tokens -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\tokens\FORTRAN-66\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action tokens -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\tokens\FORTRAN-66\HERON2.txt"
test\bin\run_test.ps1 -json -name heron-wide-ftn66 -action tokens -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON-wide.FOR" -wide -expected ".\test\ref\tokens\FORTRAN-66\HERON-wide.txt"

test\bin\run_test.ps1 -json -name hello-ftn77 -action tokens -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\tokens\FORTRAN-77\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action tokens -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\tokens\FORTRAN-77\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action tokens -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\tokens\FORTRAN-77\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action tokens -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\tokens\FORTRAN-77\HERON.txt"

test\bin\run_test.ps1 -json -name average-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\tokens\Fortran-90\average.txt"
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\tokens\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\tokens\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\tokens\Fortran-90\hello.txt"

test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action tokens -language Fortran-95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\tokens\Fortran-95\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action tokens -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\tokens\Fortran-95\Wikibooks-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn95 -action tokens -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex2.f95" -expected ".\test\ref\tokens\Fortran-95\Wikibooks-ex2.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn95 -action tokens -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex3.f95" -expected ".\test\ref\tokens\Fortran-95\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn95 -action tokens -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex4.f95" -expected ".\test\ref\tokens\Fortran-95\Wikibooks-ex4.txt"

test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action tokens -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\tokens\Fortran-2003\geo4060.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn2003 -action tokens -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\temp_sub.for" -expected ".\test\ref\tokens\Fortran-2003\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn2003 -action tokens -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\temp_func.for" -expected ".\test\ref\tokens\Fortran-2003\temp_func.txt"

test\bin\run_test.ps1 -json -name geo4060-ftn2008 -action tokens -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\geo4060.for" -expected ".\test\ref\tokens\Fortran-2008\geo4060.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn2008 -action tokens -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\temp_sub.for" -expected ".\test\ref\tokens\Fortran-2008\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn2008 -action tokens -language Fortran-2008 -inputfile ".\test\data\Fortran-2008\temp_func.for" -expected ".\test\ref\tokens\Fortran-2008\temp_func.txt"

test\bin\run_test.ps1 -json -name knuth-html -action tokens -language HTML -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\tokens\HTML\knuth.txt"
test\bin\run_test.ps1 -json -name developer-html -action tokens -language HTML -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\tokens\HTML\developer-css.txt"
test\bin\run_test.ps1 -json -name codestat-html -action tokens -language HTML -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\tokens\HTML\codestat-css-javascript.txt"

test\bin\run_test.ps1 -json -name prime_test-java -action tokens -language Java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\tokens\Java\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action tokens -language Java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\tokens\Java\palindrome.txt"

test\bin\run_test.ps1 -json -name codestat-js -action tokens -language JavaScript -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\tokens\JavaScript\codestat.txt"
test\bin\run_test.ps1 -json -name bing-js -action tokens -language JavaScript -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\tokens\JavaScript\bing.txt"

test\bin\run_test.ps1 -json -name hello-objc -action tokens -language Objective-C -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\tokens\Objective-C\HelloWorld.txt"

test\bin\run_test.ps1 -json -name firework -action tokens -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\tokens\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello -action tokens -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\tokens\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose -action tokens -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\tokens\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider -action tokens -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\tokens\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16 -action tokens -language Pascal -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\tokens\Pascal\TPC16.txt"

test\bin\run_test.ps1 -json -name family-main -action tokens -language Prolog -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\tokens\Prolog\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb -action tokens -language Prolog -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\tokens\Prolog\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu -action tokens -language Prolog -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\tokens\Prolog\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries -action tokens -language Prolog -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\tokens\Prolog\family-queries.txt"

test\bin\run_test.ps1 -json -name drone-3d -action tokens -language Python -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\tokens\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor -action tokens -language Python -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\tokens\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory -action tokens -language Python -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\tokens\Python\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-python -action tokens -language Python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\tokens\Python\values.txt"

test\bin\run_test.ps1 -json -name ETM-540-01 -action tokens -language R -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\tokens\R\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02 -action tokens -language R -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\tokens\R\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03 -action tokens -language R -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\tokens\R\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04 -action tokens -language R -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\tokens\R\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action tokens -language R -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\tokens\R\ETM-540-05.txt"

test\bin\run_test.ps1 -json -name basic-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\tokens\Ruby\basic.txt"
test\bin\run_test.ps1 -json -name constants-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\constants.rb" -expected ".\test\ref\tokens\Ruby\constants.txt"
test\bin\run_test.ps1 -json -name exceptions-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\exceptions.rb" -expected ".\test\ref\tokens\Ruby\exceptions.txt"
test\bin\run_test.ps1 -json -name expressions-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\expressions.rb" -expected ".\test\ref\tokens\Ruby\expressions.txt"
test\bin\run_test.ps1 -json -name functions-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\functions.rb" -expected ".\test\ref\tokens\Ruby\functions.txt"
test\bin\run_test.ps1 -json -name io-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\io.rb" -expected ".\test\ref\tokens\Ruby\io.txt"
test\bin\run_test.ps1 -json -name modifiers-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\modifiers.rb" -expected ".\test\ref\tokens\Ruby\modifiers.txt"
test\bin\run_test.ps1 -json -name operators-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\operators.rb" -expected ".\test\ref\tokens\Ruby\operators.txt"
test\bin\run_test.ps1 -json -name statements-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\statements.rb" -expected ".\test\ref\tokens\Ruby\statements.txt"
test\bin\run_test.ps1 -json -name tokenbuilders-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\tokenbuilders.rb" -expected ".\test\ref\tokens\Ruby\tokenbuilders.txt"
test\bin\run_test.ps1 -json -name tokenizers-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\tokenizers.rb" -expected ".\test\ref\tokens\Ruby\tokenizers.txt"
test\bin\run_test.ps1 -json -name tokens-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\tokens.rb" -expected ".\test\ref\tokens\Ruby\tokens.txt"
test\bin\run_test.ps1 -json -name webhook-ruby -action tokens -language Ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\tokens\Ruby\webhook.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-92 -action tokens -language SQL-92 -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\tokens\SQL-92\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action tokens -language SQL-92 -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\tokens\SQL-92\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-99 -action tokens -language SQL-99 -inputfile ".\test\data\SQL-99\microsoft.sql" -expected ".\test\ref\tokens\SQL-99\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-99 -action tokens -language SQL-99 -inputfile ".\test\data\SQL-99\table.sql" -expected ".\test\ref\tokens\SQL-99\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-2003 -action tokens -language SQL-2003 -inputfile ".\test\data\SQL-2003\microsoft.sql" -expected ".\test\ref\tokens\SQL-2003\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2003 -action tokens -language SQL-2003 -inputfile ".\test\data\SQL-2003\table.sql" -expected ".\test\ref\tokens\SQL-2003\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-2008 -action tokens -language SQL-2008 -inputfile ".\test\data\SQL-2008\microsoft.sql" -expected ".\test\ref\tokens\SQL-2008\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2008 -action tokens -language SQL-2008 -inputfile ".\test\data\SQL-2008\table.sql" -expected ".\test\ref\tokens\SQL-2008\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-2011 -action tokens -language SQL-2011 -inputfile ".\test\data\SQL-2011\microsoft.sql" -expected ".\test\ref\tokens\SQL-2011\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2011 -action tokens -language SQL-2011 -inputfile ".\test\data\SQL-2011\table.sql" -expected ".\test\ref\tokens\SQL-2011\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-2016 -action tokens -language SQL-2016 -inputfile ".\test\data\SQL-2016\microsoft.sql" -expected ".\test\ref\tokens\SQL-2016\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2016 -action tokens -language SQL-2016 -inputfile ".\test\data\SQL-2016\table.sql" -expected ".\test\ref\tokens\SQL-2016\table.txt"

test\bin\run_test.ps1 -json -name AppDelegate -action tokens -language Swift -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\tokens\Swift\AppDelegate.txt"
test\bin\run_test.ps1 -json -name Meal -action tokens -language Swift -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\tokens\Swift\Meal.txt"
test\bin\run_test.ps1 -json -name MealTableViewCell -action tokens -language Swift -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\tokens\Swift\MealTableViewCell.txt"
test\bin\run_test.ps1 -json -name MealTableViewController -action tokens -language Swift -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\tokens\Swift\MealTableViewController.txt"
test\bin\run_test.ps1 -json -name MealViewController -action tokens -language Swift -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\tokens\Swift\MealViewController.txt"
test\bin\run_test.ps1 -json -name RatingControl -action tokens -language Swift -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\tokens\Swift\RatingControl.txt"

test\bin\run_test.ps1 -json -name TimeReporter-ts -action tokens -language TypeScript -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\tokens\TypeScript\TimeReporter.txt"
test\bin\run_test.ps1 -json -name ImageBoard-ts -action tokens -language TypeScript -inputfile ".\test\data\TypeScript\ImageBoard.ts" -expected ".\test\ref\tokens\TypeScript\ImageBoard.txt"

test\bin\run_test.ps1 -json -name spider-vb6 -action tokens -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\tokens\VisualBasic-6\spider.txt"
test\bin\run_test.ps1 -json -name azure_metadata-vb6 -action tokens -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\azure_metadata.bas" -expected ".\test\ref\tokens\VisualBasic-6\azure_metadata.txt"
test\bin\run_test.ps1 -json -name diffie_hellman-vb6 -action tokens -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\diffie_hellman.bas" -expected ".\test\ref\tokens\VisualBasic-6\diffie_hellman.txt"

test\bin\run_test.ps1 -json -name word-processor-vbnet -action tokens -language VisualBasic-NET -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\tokens\VisualBasic-NET\WordProcessor.txt"

test\bin\run_test.ps1 -json -name polyglot-c -action tokens -language C -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\tokens\C\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action tokens -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\tokens\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action tokens -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\tokens\Pascal\polyglot.txt"

Write-Output "Number of failures: $failures"
