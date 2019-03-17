Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -json -name values -action detect -tiebreak -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\detect\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple -action detect -tiebreak -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\detect\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot -action detect -tiebreak -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\detect\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum -action detect -tiebreak -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\detect\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life -action detect -tiebreak -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\detect\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income -action detect -tiebreak -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\detect\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2 -action detect -tiebreak -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\detect\BASIC\rockt2.txt"

test\bin\run_test.ps1 -json -name diamond -action detect -tiebreak -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\detect\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test -action detect -tiebreak -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\detect\C\prime_test.txt"

test\bin\run_test.ps1 -json -name fibo -action detect -tiebreak -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\detect\CBASIC\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action detect -tiebreak -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\detect\CBASIC\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action detect -tiebreak -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\detect\CBASIC\GRAPHR.txt"

test\bin\run_test.ps1 -json -name mccracken1-68 -action detect -tiebreak -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\detect\COBOL-68\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action detect -tiebreak -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\detect\COBOL-68\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action detect -tiebreak -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\detect\COBOL-68\mccracken3.txt"

test\bin\run_test.ps1 -json -name prog1-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG1.COB" -expected ".\test\ref\detect\COBOL-74\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG2.COB" -expected ".\test\ref\detect\COBOL-74\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG2A.COB" -expected ".\test\ref\detect\COBOL-74\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG3.COB" -expected ".\test\ref\detect\COBOL-74\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG4.COB" -expected ".\test\ref\detect\COBOL-74\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG5.COB" -expected ".\test\ref\detect\COBOL-74\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG6.COB" -expected ".\test\ref\detect\COBOL-74\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG12-2.COB" -expected ".\test\ref\detect\COBOL-74\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG13-3.COB" -expected ".\test\ref\detect\COBOL-74\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG14-2.COB" -expected ".\test\ref\detect\COBOL-74\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\PROG15-4.COB" -expected ".\test\ref\detect\COBOL-74\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL-74\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-wide-74 -action detect -tiebreak -inputfile ".\test\data\COBOL-74\P010-wide.COB" -wide -expected ".\test\ref\detect\COBOL-74\P010-wide.txt"

test\bin\run_test.ps1 -json -name prog1-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG1.COB" -expected ".\test\ref\detect\COBOL-85\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG2.COB" -expected ".\test\ref\detect\COBOL-85\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG2A.COB" -expected ".\test\ref\detect\COBOL-85\PROG2A.txt"
test\bin\run_test.ps1 -json -name prog3-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG3.COB" -expected ".\test\ref\detect\COBOL-85\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG4.COB" -expected ".\test\ref\detect\COBOL-85\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG5.COB" -expected ".\test\ref\detect\COBOL-85\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG6.COB" -expected ".\test\ref\detect\COBOL-85\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG12-2.COB" -expected ".\test\ref\detect\COBOL-85\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG13-3.COB" -expected ".\test\ref\detect\COBOL-85\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG14-2.COB" -expected ".\test\ref\detect\COBOL-85\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\detect\COBOL-85\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL-85\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-85 -action detect -tiebreak -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\detect\COBOL-85\P010.txt"

test\bin\run_test.ps1 -json -name report-card-cob2002 -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\detect\COBOL-2002\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2002 -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\detect\COBOL-2002\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2002 -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2002\SalesReport.cob" -expected ".\test\ref\detect\COBOL-2002\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2002 -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\detect\COBOL-2002\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action detect -tiebreak -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\detect\COBOL-2002\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name report-card-cob2014 -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014\ReportCard.cob" -expected ".\test\ref\detect\COBOL-2014\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014 -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014\Person.cob" -expected ".\test\ref\detect\COBOL-2014\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014 -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014\SalesReport.cob" -expected ".\test\ref\detect\COBOL-2014\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014 -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014\Report.cob" -expected ".\test\ref\detect\COBOL-2014\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014 -action detect -tiebreak -inputfile ".\test\data\COBOL-2014\AdventOfCode.cob" -expected ".\test\ref\detect\COBOL-2014\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name report-card-cob2014-ACU -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-ACU\ReportCard.cob" -expected ".\test\ref\detect\COBOL-2014-ACU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-ACU -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-ACU\Person.cob" -expected ".\test\ref\detect\COBOL-2014-ACU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-ACU -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-ACU\SalesReport.cob" -expected ".\test\ref\detect\COBOL-2014-ACU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-ACU -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-ACU\Report.cob" -expected ".\test\ref\detect\COBOL-2014-ACU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-ACU -action detect -tiebreak -inputfile ".\test\data\COBOL-2014-ACU\AdventOfCode.cob" -expected ".\test\ref\detect\COBOL-2014-ACU\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name report-card-cob2014-IBM -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-IBM\ReportCard.cob" -expected ".\test\ref\detect\COBOL-2014-IBM\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-IBM -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-IBM\Person.cob" -expected ".\test\ref\detect\COBOL-2014-IBM\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-IBM -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-IBM\SalesReport.cob" -expected ".\test\ref\detect\COBOL-2014-IBM\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-IBM -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-IBM\Report.cob" -expected ".\test\ref\detect\COBOL-2014-IBM\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-IBM -action detect -tiebreak -inputfile ".\test\data\COBOL-2014-IBM\AdventOfCode.cob" -expected ".\test\ref\detect\COBOL-2014-IBM\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name report-card-cob2014-GNU -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-GNU\ReportCard.cob" -expected ".\test\ref\detect\COBOL-2014-GNU\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2014-GNU -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-GNU\Person.cob" -expected ".\test\ref\detect\COBOL-2014-GNU\Person.txt"
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\detect\COBOL-2014-GNU\SalesReport.txt"
test\bin\run_test.ps1 -json -name report-cob2014-GNU -action detect -tabsize 4 -tiebreak -inputfile ".\test\data\COBOL-2014-GNU\Report.cob" -expected ".\test\ref\detect\COBOL-2014-GNU\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2014-GNU -action detect -tiebreak -inputfile ".\test\data\COBOL-2014-GNU\AdventOfCode.cob" -expected ".\test\ref\detect\COBOL-2014-GNU\AdventOfCode.txt"

test\bin\run_test.ps1 -json -name checkers -action detect -tiebreak -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\detect\Cplusplus\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb -action detect -tiebreak -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\detect\Cplusplus\hrdb.txt"
test\bin\run_test.ps1 -json -name date_h -action detect -tiebreak -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\detect\Cplusplus\date_h.txt"
test\bin\run_test.ps1 -json -name date_cplusplus -action detect -tiebreak -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\detect\Cplusplus\date_cpp.txt"

test\bin\run_test.ps1 -json -name calculator1 -action detect -tiebreak -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\detect\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2 -action detect -tiebreak -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\detect\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3 -action detect -tiebreak -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\detect\Csharp\calculator3.txt"

test\bin\run_test.ps1 -json -name hello-ftn66 -action detect -tiebreak -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\detect\FORTRAN-66\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action detect -tiebreak -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\detect\FORTRAN-66\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action detect -tiebreak -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\detect\FORTRAN-66\HERON2.txt"
test\bin\run_test.ps1 -json -name heron-wide-ftn66 -action detect -tiebreak -inputfile ".\test\data\FORTRAN-66\HERON-wide.FOR" -wide -expected ".\test\ref\detect\FORTRAN-66\HERON-wide.txt"

test\bin\run_test.ps1 -json -name hello-ftn77 -action detect -tiebreak -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\detect\FORTRAN-77\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action detect -tiebreak -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\detect\FORTRAN-77\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action detect -tiebreak -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\detect\FORTRAN-77\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action detect -tiebreak -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\detect\FORTRAN-77\HERON.txt"

test\bin\run_test.ps1 -json -name average-ftn90 -action detect -tiebreak -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\detect\Fortran-90\average.txt"
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action detect -tiebreak -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\detect\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action detect -tiebreak -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\detect\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action detect -tiebreak -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\detect\Fortran-90\hello.txt"

test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action detect -tiebreak -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\detect\Fortran-95\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action detect -tiebreak -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\detect\Fortran-95\Wikibooks-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn95 -action detect -tiebreak -inputfile ".\test\data\Fortran-95\Wikibooks-ex2.f95" -expected ".\test\ref\detect\Fortran-95\Wikibooks-ex2.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn95 -action detect -tiebreak -inputfile ".\test\data\Fortran-95\Wikibooks-ex3.f95" -expected ".\test\ref\detect\Fortran-95\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn95 -action detect -tiebreak -inputfile ".\test\data\Fortran-95\Wikibooks-ex4.f95" -expected ".\test\ref\detect\Fortran-95\Wikibooks-ex4.txt"

test\bin\run_test.ps1 -json -name knuth-html -action detect -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\detect\HTML\knuth.txt"
test\bin\run_test.ps1 -json -name developer-html -action detect -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\detect\HTML\developer-css.txt"
test\bin\run_test.ps1 -json -name codestat-html -action detect -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\detect\HTML\codestat-css-javascript.txt"

test\bin\run_test.ps1 -json -name prime_test-java -action detect -tiebreak -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\detect\Java\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action detect -tiebreak -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\detect\Java\palindrome.txt"

test\bin\run_test.ps1 -json -name codestat-js -action detect -tiebreak -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\detect\JavaScript\codestat.txt"
test\bin\run_test.ps1 -json -name bing-js -action detect -tiebreak -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\detect\JavaScript\bing.txt"

test\bin\run_test.ps1 -json -name hello-objc -action detect -tiebreak -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\detect\Objective-C\HelloWorld.txt"

test\bin\run_test.ps1 -json -name firework -action detect -tiebreak -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello -action detect -tiebreak -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\detect\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose -action detect -tiebreak -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\detect\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider -action detect -tiebreak -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\detect\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16 -action detect -tiebreak -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\detect\Pascal\TPC16.txt"

test\bin\run_test.ps1 -json -name family-main -action detect -tiebreak -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\detect\Prolog\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb -action detect -tiebreak -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\detect\Prolog\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu -action detect -tiebreak -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\detect\Prolog\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries -action detect -tiebreak -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\detect\Prolog\family-queries.txt"

test\bin\run_test.ps1 -json -name drone-3d -action detect -tiebreak -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\detect\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor -action detect -tiebreak -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\detect\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory -action detect -tiebreak -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\detect\Python\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-python -action detect -tiebreak -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\detect\Python\values.txt"

test\bin\run_test.ps1 -json -name ETM-540-01 -action detect -tiebreak -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\detect\R\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02 -action detect -tiebreak -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\detect\R\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03 -action detect -tiebreak -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\detect\R\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04 -action detect -tiebreak -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\detect\R\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action detect -tiebreak -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\detect\R\ETM-540-05.txt"

test\bin\run_test.ps1 -json -name basic-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\detect\Ruby\basic.txt"
test\bin\run_test.ps1 -json -name constants-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\constants.rb" -expected ".\test\ref\detect\Ruby\constants.txt"
test\bin\run_test.ps1 -json -name exceptions-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\exceptions.rb" -expected ".\test\ref\detect\Ruby\exceptions.txt"
test\bin\run_test.ps1 -json -name expressions-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\expressions.rb" -expected ".\test\ref\detect\Ruby\expressions.txt"
test\bin\run_test.ps1 -json -name functions-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\functions.rb" -expected ".\test\ref\detect\Ruby\functions.txt"
test\bin\run_test.ps1 -json -name io-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\io.rb" -expected ".\test\ref\detect\Ruby\io.txt"
test\bin\run_test.ps1 -json -name modifiers-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\modifiers.rb" -expected ".\test\ref\detect\Ruby\modifiers.txt"
test\bin\run_test.ps1 -json -name operators-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\operators.rb" -expected ".\test\ref\detect\Ruby\operators.txt"
test\bin\run_test.ps1 -json -name statements-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\statements.rb" -expected ".\test\ref\detect\Ruby\statements.txt"
test\bin\run_test.ps1 -json -name tokenbuilders-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\tokenbuilders.rb" -expected ".\test\ref\detect\Ruby\tokenbuilders.txt"
test\bin\run_test.ps1 -json -name tokenizers-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\tokenizers.rb" -expected ".\test\ref\detect\Ruby\tokenizers.txt"
test\bin\run_test.ps1 -json -name tokens-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\tokens.rb" -expected ".\test\ref\detect\Ruby\tokens.txt"
test\bin\run_test.ps1 -json -name webhook-ruby -action detect -tiebreak -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\detect\Ruby\webhook.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-92 -action detect -tiebreak -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\detect\SQL-92\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action detect -tiebreak -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\detect\SQL-92\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-99 -action detect -tiebreak -inputfile ".\test\data\SQL-99\microsoft.sql" -expected ".\test\ref\detect\SQL-99\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-99 -action detect -tiebreak -inputfile ".\test\data\SQL-99\table.sql" -expected ".\test\ref\detect\SQL-99\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-2003 -action detect -tiebreak -inputfile ".\test\data\SQL-2003\microsoft.sql" -expected ".\test\ref\detect\SQL-2003\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2003 -action detect -tiebreak -inputfile ".\test\data\SQL-2003\table.sql" -expected ".\test\ref\detect\SQL-2003\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-2008 -action detect -tiebreak -inputfile ".\test\data\SQL-2008\microsoft.sql" -expected ".\test\ref\detect\SQL-2008\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2008 -action detect -tiebreak -inputfile ".\test\data\SQL-2008\table.sql" -expected ".\test\ref\detect\SQL-2008\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-2011 -action detect -tiebreak -inputfile ".\test\data\SQL-2011\microsoft.sql" -expected ".\test\ref\detect\SQL-2011\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2011 -action detect -tiebreak -inputfile ".\test\data\SQL-2011\table.sql" -expected ".\test\ref\detect\SQL-2011\table.txt"

test\bin\run_test.ps1 -json -name microsoft-sql-2016 -action detect -tiebreak -inputfile ".\test\data\SQL-2016\microsoft.sql" -expected ".\test\ref\detect\SQL-2016\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-2016 -action detect -tiebreak -inputfile ".\test\data\SQL-2016\table.sql" -expected ".\test\ref\detect\SQL-2016\table.txt"

test\bin\run_test.ps1 -json -name AppDelegate -action detect -tiebreak -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\detect\Swift\AppDelegate.txt"
test\bin\run_test.ps1 -json -name Meal -action detect -tiebreak -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\detect\Swift\Meal.txt"
test\bin\run_test.ps1 -json -name MealTableViewCell -action detect -tiebreak -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\detect\Swift\MealTableViewCell.txt"
test\bin\run_test.ps1 -json -name MealTableViewController -action detect -tiebreak -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\detect\Swift\MealTableViewController.txt"
test\bin\run_test.ps1 -json -name MealViewController -action detect -tiebreak -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\detect\Swift\MealViewController.txt"
test\bin\run_test.ps1 -json -name RatingControl -action detect -tiebreak -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\detect\Swift\RatingControl.txt"

test\bin\run_test.ps1 -json -name polyglot -action detect -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect\multiple\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-languages -action detect -languages c,cplusplus,cobol68,cobol74,cobol85,objectivec,pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect\multiple\polyglot-languages.txt"

Write-Output "Number of failures: $failures"
