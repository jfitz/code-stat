Set-StrictMode -Version Latest

[int]$failures = 0

test\bin\run_test.ps1 -json -name values -action detect -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\detect\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple -action detect -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\detect\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot -action detect -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\detect\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum -action detect -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\detect\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life -action detect -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\detect\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income -action detect -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\detect\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2 -action detect -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\detect\BASIC\rockt2.txt"

test\bin\run_test.ps1 -json -name diamond -action detect -inputfile ".\test\data\C\diamond.c" -expected ".\test\ref\detect\C\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test -action detect -inputfile ".\test\data\C\prime_test.c" -expected ".\test\ref\detect\C\prime_test.txt"

test\bin\run_test.ps1 -json -name prog1 -action detect -inputfile ".\test\data\COBOL\PROG1.COB" -expected ".\test\ref\detect\COBOL\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2 -action detect -inputfile ".\test\data\COBOL\PROG2.COB" -expected ".\test\ref\detect\COBOL\PROG2.txt"
test\bin\run_test.ps1 -json -name prog3 -action detect -inputfile ".\test\data\COBOL\PROG3.COB" -expected ".\test\ref\detect\COBOL\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4 -action detect -inputfile ".\test\data\COBOL\PROG4.COB" -expected ".\test\ref\detect\COBOL\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5 -action detect -inputfile ".\test\data\COBOL\PROG5.COB" -expected ".\test\ref\detect\COBOL\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6 -action detect -inputfile ".\test\data\COBOL\PROG6.COB" -expected ".\test\ref\detect\COBOL\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2 -action detect -inputfile ".\test\data\COBOL\PROG12-2.COB" -expected ".\test\ref\detect\COBOL\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3 -action detect -inputfile ".\test\data\COBOL\PROG13-3.COB" -expected ".\test\ref\detect\COBOL\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2 -action detect -inputfile ".\test\data\COBOL\PROG14-2.COB" -expected ".\test\ref\detect\COBOL\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4 -action detect -inputfile ".\test\data\COBOL\PROG15-4.COB" -expected ".\test\ref\detect\COBOL\PROG15-4.txt"
test\bin\run_test.ps1 -json -name freeformat1 -action detect -inputfile ".\test\data\COBOL\Free Format 1.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\Free Format 1.txt"
test\bin\run_test.ps1 -json -name freeformat2 -action detect -inputfile ".\test\data\COBOL\Free Format 2.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\Free Format 2.txt"
test\bin\run_test.ps1 -json -name freeformat3 -action detect -inputfile ".\test\data\COBOL\Free Format 3.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\Free Format 3.txt"
test\bin\run_test.ps1 -json -name freeformat4 -action detect -inputfile ".\test\data\COBOL\Free Format 4.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\Free Format 4.txt"
test\bin\run_test.ps1 -json -name exec1 -action detect -inputfile ".\test\data\COBOL\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL\UNLDDBCU2.txt"

test\bin\run_test.ps1 -json -name checkers -action detect -inputfile ".\test\data\Cpp\checkers.cpp" -expected ".\test\ref\detect\Cpp\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb -action detect -inputfile ".\test\data\Cpp\hrdb.cpp" -expected ".\test\ref\detect\Cpp\hrdb.txt"
test\bin\run_test.ps1 -json -name date_h -action detect -inputfile ".\test\data\Cpp\date.h" -expected ".\test\ref\detect\Cpp\date_h.txt"
test\bin\run_test.ps1 -json -name date_cpp -action detect -inputfile ".\test\data\Cpp\date.cpp" -expected ".\test\ref\detect\Cpp\date_cpp.txt"

test\bin\run_test.ps1 -json -name firework -action detect -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello -action detect -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\detect\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose -action detect -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\detect\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider -action detect -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\detect\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16 -action detect -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\detect\Pascal\TPC16.txt"

test\bin\run_test.ps1 -json -name polyglot -action detect -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect\multiple\polyglot.txt"

test\bin\run_test.ps1 -json -name drone-3d -action detect -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\detect\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor -action detect -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\detect\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory -action detect -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\detect\Python\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-python -action detect -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\detect\Python\values.txt"

test\bin\run_test.ps1 -json -name hello-ftn66 -action detect -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\detect\FORTRAN-66\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action detect -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\detect\FORTRAN-66\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action detect -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\detect\FORTRAN-66\HERON2.txt"

test\bin\run_test.ps1 -json -name hello-ftn77 -action detect -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\detect\FORTRAN-77\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action detect -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\detect\FORTRAN-77\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action detect -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\detect\FORTRAN-77\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action detect -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\detect\FORTRAN-77\HERON.txt"

test\bin\run_test.ps1 -json -name average-ftn90 -action detect -language Fortran-90 -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\detect\Fortran-90\average.txt"
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action detect -language Fortran-90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\detect\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action detect -language Fortran-90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\detect\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action detect -language Fortran-90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\detect\Fortran-90\hello.txt"

test\bin\run_test.ps1 -json -name calculator1 -action detect -language Csharp -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\detect\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2 -action detect -language Csharp -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\detect\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3 -action detect -language Csharp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\detect\Csharp\calculator3.txt"

test\bin\run_test.ps1 -json -name prime_test-java -action detect -language Java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\detect\Java\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action detect -language Java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\detect\Java\palindrome.txt"

test\bin\run_test.ps1 -json -name webhook-ruby -action detect -language Ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\detect\Ruby\webhook.txt"

Write-Output "Number of failures: $failures"
