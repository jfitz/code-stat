Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -json -name values -action confidence -language BASIC -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\confidence\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple -action confidence -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\confidence\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot -action confidence -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\confidence\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum -action confidence -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\confidence\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life -action confidence -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\confidence\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income -action confidence -language BASIC -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\confidence\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2 -action confidence -language BASIC -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\confidence\BASIC\rockt2.txt"

test\bin\run_test.ps1 -json -name diamond -action confidence -language C -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\confidence\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test -action confidence -language C -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\confidence\C\prime_test.txt"

test\bin\run_test.ps1 -json -name prog1 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG1.COB" -expected ".\test\ref\confidence\COBOL-85\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG2.COB" -expected ".\test\ref\confidence\COBOL-85\PROG2.txt"
test\bin\run_test.ps1 -json -name prog3 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG3.COB" -expected ".\test\ref\confidence\COBOL-85\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG4.COB" -expected ".\test\ref\confidence\COBOL-85\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG5.COB" -expected ".\test\ref\confidence\COBOL-85\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG6.COB" -expected ".\test\ref\confidence\COBOL-85\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG12-2.COB" -expected ".\test\ref\confidence\COBOL-85\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG13-3.COB" -expected ".\test\ref\confidence\COBOL-85\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG14-2.COB" -expected ".\test\ref\confidence\COBOL-85\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4 -action confidence -language COBOL-85 -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\confidence\COBOL-85\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1 -action confidence -language COBOL-85 -tabsize 4 -inputfile ".\test\data\COBOL-85\UNLDDBCU2.COB" -expected ".\test\ref\confidence\COBOL-85\UNLDDBCU2.txt"

test\bin\run_test.ps1 -json -name report_card -action confidence -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.COB" -expected ".\test\ref\confidence\COBOL-2002\ReportCard.txt"
test\bin\run_test.ps1 -json -name person -action confidence -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Person.COB" -expected ".\test\ref\confidence\COBOL-2002\Person.txt"
test\bin\run_test.ps1 -json -name sales_report -action confidence -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\SalesReport.COB" -expected ".\test\ref\confidence\COBOL-2002\SalesReport.txt"
test\bin\run_test.ps1 -json -name report -action confidence -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.COB" -expected ".\test\ref\confidence\COBOL-2002\Report.txt"

test\bin\run_test.ps1 -json -name checkers -action confidence -language Cpp -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\confidence\Cpp\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb -action confidence -language Cpp -inputfile ".\test\data\Cpp\hrdb.cpp" -expected ".\test\ref\confidence\Cpp\hrdb.txt"
test\bin\run_test.ps1 -json -name date_h -action confidence -language Cpp -inputfile ".\test\data\Cpp\date.h" -expected ".\test\ref\confidence\Cpp\date_h.txt"
test\bin\run_test.ps1 -json -name date_cpp -action confidence -language Cpp -inputfile ".\test\data\Cpp\date.cpp" -expected ".\test\ref\confidence\Cpp\date_cpp.txt"

test\bin\run_test.ps1 -json -name firework -action confidence -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\confidence\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello -action confidence -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\confidence\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose -action confidence -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\confidence\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider -action confidence -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\confidence\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16 -action confidence -language Pascal -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\confidence\Pascal\TPC16.txt"

test\bin\run_test.ps1 -json -name polyglot-c -action confidence -language C -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence\C\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action confidence -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action confidence -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence\Pascal\polyglot.txt"

test\bin\run_test.ps1 -json -name drone-3d -action confidence -language Python -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\confidence\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor -action confidence -language Python -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\confidence\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory -action confidence -language Python -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\confidence\Python\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-python -action confidence -language Python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\confidence\Python\values.txt"

test\bin\run_test.ps1 -json -name hello-ftn66 -action confidence -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\confidence\FORTRAN-66\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action confidence -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\confidence\FORTRAN-66\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action confidence -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\confidence\FORTRAN-66\HERON2.txt"

test\bin\run_test.ps1 -json -name hello-ftn77 -action confidence -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\confidence\FORTRAN-77\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action confidence -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\confidence\FORTRAN-77\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action confidence -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\confidence\FORTRAN-77\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action confidence -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\confidence\FORTRAN-77\HERON.txt"

test\bin\run_test.ps1 -json -name average-ftn90 -action confidence -language Fortran-90 -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\confidence\Fortran-90\average.txt"
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action confidence -language Fortran-90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\confidence\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action confidence -language Fortran-90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\confidence\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action confidence -language Fortran-90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\confidence\Fortran-90\hello.txt"

test\bin\run_test.ps1 -json -name calculator1 -action confidence -language Csharp -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\confidence\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2 -action confidence -language Csharp -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\confidence\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3 -action confidence -language Csharp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\confidence\Csharp\calculator3.txt"

test\bin\run_test.ps1 -json -name prime_test-java -action confidence -language Java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\confidence\Java\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action confidence -language Java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\confidence\Java\palindrome.txt"

test\bin\run_test.ps1 -json -name webhook-ruby -action confidence -language Ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\confidence\Ruby\webhook.txt"

test\bin\run_test.ps1 -json -name family-main -action confidence -language Prolog -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\confidence\Prolog\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb -action confidence -language Prolog -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\confidence\Prolog\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu -action confidence -language Prolog -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\confidence\Prolog\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries -action confidence -language Prolog -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\confidence\Prolog\family-queries.txt"

test\bin\run_test.ps1 -json -name AppDelegate -action confidence -language Swift -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\confidence\Swift\AppDelegate.txt"
test\bin\run_test.ps1 -json -name Meal -action confidence -language Swift -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\confidence\Swift\Meal.txt"
test\bin\run_test.ps1 -json -name MealTableViewCell -action confidence -language Swift -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\confidence\Swift\MealTableViewCell.txt"
test\bin\run_test.ps1 -json -name MealTableViewController -action confidence -language Swift -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\confidence\Swift\MealTableViewController.txt"
test\bin\run_test.ps1 -json -name MealViewController -action confidence -language Swift -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\confidence\Swift\MealViewController.txt"
test\bin\run_test.ps1 -json -name RatingControl -action confidence -language Swift -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\confidence\Swift\RatingControl.txt"

test\bin\run_test.ps1 -json -name ETM-540-01 -action confidence -language R -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\confidence\R\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02 -action confidence -language R -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\confidence\R\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03 -action confidence -language R -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\confidence\R\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04 -action confidence -language R -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\confidence\R\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action confidence -language R -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\confidence\R\ETM-540-05.txt"

Write-Output "Number of failures: $failures"
