Set-StrictMode -Version Latest

# generic
test\bin\run_test.ps1 -json -name adabkend-adb -action tokens -language generic -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\tokens\generic\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action tokens -language generic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\tokens\generic\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action tokens -language generic -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\tokens\generic\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action tokens -language generic -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\tokens\generic\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action tokens -language generic -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\tokens\generic\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action tokens -language generic -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\tokens\generic\heron.txt"
test\bin\run_test.ps1 -json -name bing -action tokens -language generic -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\tokens\generic\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action tokens -language generic -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\tokens\generic\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action tokens -language generic -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\tokens\generic\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action tokens -language generic -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\tokens\generic\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action tokens -language generic -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\tokens\generic\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action tokens -language generic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\tokens\generic\spider.txt"

# generic with comments
test\bin\run_test.ps1 -json -name adabkend-adb -action tokens -language generic -comment ada -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\tokens\generic-comments\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action tokens -language generic -comment basic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\tokens\generic-comments\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action tokens -language generic -comment c -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\tokens\generic-comments\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action tokens -language generic -comment cobol -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\tokens\generic-comments\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action tokens -language generic -comment cpp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\tokens\generic-comments\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action tokens -language generic -comment fortran -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\tokens\generic-comments\heron.txt"
test\bin\run_test.ps1 -json -name bing -action tokens -language generic -comment hash -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\tokens\generic-comments\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action tokens -language generic -comment percent -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\tokens\generic-comments\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action tokens -language generic -comment hash -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\tokens\generic-comments\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action tokens -language generic -comment hash -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\tokens\generic-comments\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action tokens -language generic -comment ada -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\tokens\generic-comments\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action tokens -language generic -comment basic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\tokens\generic-comments\spider.txt"

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action tokens -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\tokens\Ada-83\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-83 -action tokens -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.ads" -expected ".\test\ref\tokens\Ada-83\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-83 -action tokens -language Ada-83 -inputfile ".\test\data\Ada-83\signup.adb" -expected ".\test\ref\tokens\Ada-83\signup-adb.txt"

# Ada-95
test\bin\run_test.ps1 -json -name philosophers-ads-95 -action tokens -language Ada-95 -inputfile ".\test\data\Ada-95\philosophers.ads" -expected ".\test\ref\tokens\Ada-95\philosophers-ads.txt"

# Ada-2005

# Ada-2012

# Awk
test\bin\run_test.ps1 -json -name funstack-awk -action tokens -language Awk -inputfile ".\test\data\Awk\funstack.awk" -expected ".\test\ref\tokens\Awk\funstack.txt"
test\bin\run_test.ps1 -json -name awkaster-awk -action tokens -language Awk -inputfile ".\test\data\Awk\awkaster.awk" -expected ".\test\ref\tokens\Awk\awkaster.txt"

# BASIC
test\bin\run_test.ps1 -json -name values-bas -action tokens -language BASIC -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\tokens\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple-bas -action tokens -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\tokens\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot-bas -action tokens -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\tokens\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action tokens -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\tokens\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action tokens -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\tokens\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income-bas -action tokens -language BASIC -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\tokens\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2-bas -action tokens -language BASIC -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\tokens\BASIC\rockt2.txt"
test\bin\run_test.ps1 -json -name sea-creature-bas -action tokens -language BASIC -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\tokens\BASIC\sea_creature.txt"

# Microsoft BASIC
test\bin\run_test.ps1 -json -name doctor-mbasic -action tokens -language Microsoft-BASIC -inputfile ".\test\data\BASIC\doctor.bas" -expected ".\test\ref\tokens\Microsoft-BASIC\doctor.txt"
test\bin\run_test.ps1 -json -name sea-creature-mbasic -action tokens -language Microsoft-BASIC -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\tokens\Microsoft-BASIC\sea_creature.txt"

# BASICA
test\bin\run_test.ps1 -json -name sea-creature-basica -action tokens -language BASICA -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\tokens\BASICA\sea_creature.txt"

# C-78
test\bin\run_test.ps1 -json -name diamond-c-78 -action tokens -language C-78 -inputfile ".\test\data\C-78\diamond.c" -expected ".\test\ref\tokens\C-78\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test-c-78 -action tokens -language C-78 -inputfile ".\test\data\C-78\prime_test.c" -expected ".\test\ref\tokens\C-78\prime_test.txt"
test\bin\run_test.ps1 -json -name decl-c-78 -action tokens -language C-78 -inputfile ".\test\data\C-78\c-decl.c" -expected ".\test\ref\tokens\C-78\c-decl-c.txt"
test\bin\run_test.ps1 -json -name values-c-78 -action tokens -language C-78 -inputfile ".\test\data\C-78\values.c" -expected ".\test\ref\tokens\C-78\values.txt"

# C-99
test\bin\run_test.ps1 -json -name parser-h-99 -action tokens -language C-99 -inputfile ".\test\data\C-99\parser.h" -expected ".\test\ref\tokens\C-99\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c-99 -action tokens -language C-99 -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\tokens\C-99\parser-c.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action tokens -language CBASIC -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\tokens\CBASIC\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action tokens -language CBASIC -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\tokens\CBASIC\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action tokens -language CBASIC -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\tokens\CBASIC\GRAPHR.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken1-68 -action tokens -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\tokens\COBOL-68\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action tokens -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\tokens\COBOL-68\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action tokens -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\tokens\COBOL-68\mccracken3.txt"
test\bin\run_test.ps1 -json -name prog1-68 -action tokens -language COBOL-68 -inputfile ".\test\data\COBOL-68\PROG1.COB" -expected ".\test\ref\tokens\COBOL-68\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-68 -action tokens -language COBOL-68 -inputfile ".\test\data\COBOL-68\PROG2.COB" -expected ".\test\ref\tokens\COBOL-68\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-68 -action tokens -language COBOL-68 -inputfile ".\test\data\COBOL-68\PROG2A.COB" -expected ".\test\ref\tokens\COBOL-68\PROG2A.txt"

# COBOL-74

# COBOL-85
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

# COBOL-2002
test\bin\run_test.ps1 -json -name report-card-cob2002 -action tokens -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\tokens\COBOL-2002\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2002 -action tokens -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\tokens\COBOL-2002\Person.txt"
test\bin\run_test.ps1 -json -name report-cob2002 -action tokens -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\tokens\COBOL-2002\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action tokens -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\tokens\COBOL-2002\AdventOfCode.txt"
test\bin\run_test.ps1 -json -name P010-wide-cob2002 -action tokens -language COBOL-2002 -inputfile ".\test\data\COBOL-2002\P010-wide.COB" -wide -expected ".\test\ref\tokens\COBOL-2002\P010-wide.txt"

# COBOL-2014

# COBOL-2014 with AcuCobol extensions

# COBOL-2014 with IBM extensions

# COBOL-2014 with GNU extensions
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action tokens -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\tokens\COBOL-2014-GNU\SalesReport.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\tokens\Cplusplus\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb-cpp -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\tokens\Cplusplus\hrdb.txt"
test\bin\run_test.ps1 -json -name date-h-cpp -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\tokens\Cplusplus\date_h.txt"
test\bin\run_test.ps1 -json -name date-cplusplus-cpp -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\tokens\Cplusplus\date_cpp.txt"
test\bin\run_test.ps1 -json -name inherit-cplusplus-cpp -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\tokens\Cplusplus\inherit.txt"
test\bin\run_test.ps1 -json -name values-cpp -action tokens -language Cplusplus -inputfile ".\test\data\Cplusplus\values.cpp" -expected ".\test\ref\tokens\Cplusplus\values.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action tokens -language Csharp -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\tokens\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2-cs -action tokens -language Csharp -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\tokens\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3-cs -action tokens -language Csharp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\tokens\Csharp\calculator3.txt"

# CoffeeScript
test\bin\run_test.ps1 -json -name provider-coffee -action tokens -language CoffeeScript -inputfile ".\test\data\CoffeeScript\provider.coffee" -expected ".\test\ref\tokens\CoffeeScript\provider.txt"
test\bin\run_test.ps1 -json -name resque-coffee -action tokens -language CoffeeScript -inputfile ".\test\data\CoffeeScript\resque.coffee" -expected ".\test\ref\tokens\CoffeeScript\resque.txt"
test\bin\run_test.ps1 -json -name world-map-coffee -action tokens -language CoffeeScript -inputfile ".\test\data\CoffeeScript\world_map.coffee" -expected ".\test\ref\tokens\CoffeeScript\world_map.txt"

# D
test\bin\run_test.ps1 -json -name regex-d -action tokens -language d -inputfile ".\test\data\d\regex.d" -expected ".\test\ref\tokens\d\regex.txt"
test\bin\run_test.ps1 -json -name halffloat-d -action tokens -language d -inputfile ".\test\data\d\halffloat.d" -expected ".\test\ref\tokens\d\halffloat.txt"
test\bin\run_test.ps1 -json -name wc-d -action tokens -language d -inputfile ".\test\data\d\wc.d" -expected ".\test\ref\tokens\d\wc.txt"

# Dart
test\bin\run_test.ps1 -json -name anagram-dart -action tokens -language Dart -inputfile ".\test\data\Dart\anagram.dart" -expected ".\test\ref\tokens\dart\anagram.txt"
test\bin\run_test.ps1 -json -name note_client-dart -action tokens -language Dart -inputfile ".\test\data\Dart\note_client.dart" -expected ".\test\ref\tokens\dart\note_client.txt"
test\bin\run_test.ps1 -json -name web_app-dart -action tokens -language Dart -inputfile ".\test\data\Dart\web_app.dart" -expected ".\test\ref\tokens\dart\web_app.txt"

# dbase II
test\bin\run_test.ps1 -json -name sample-dbii -action tokens -language dbase-II -inputfile ".\test\data\dbase-II\sample.prg" -expected ".\test\ref\tokens\dbase-II\sample.txt"

# Delphi
test\bin\run_test.ps1 -json -name FmMain-dfm-delphi -action tokens -language Delphi -inputfile ".\test\data\delphi\FmMain.dfm" -expected ".\test\ref\tokens\Delphi\FmMain-dfm.txt"
test\bin\run_test.ps1 -json -name FmMain-pas-delphi -action tokens -language Delphi -inputfile ".\test\data\delphi\FmMain.pas" -expected ".\test\ref\tokens\Delphi\FmMain-pas.txt"
test\bin\run_test.ps1 -json -name UCalc-delphi -action tokens -language Delphi -inputfile ".\test\data\delphi\UCalc.pas" -expected ".\test\ref\tokens\Delphi\UCalc.txt"
test\bin\run_test.ps1 -json -name UChessBoardCmp-delphi -action tokens -language Delphi -inputfile ".\test\data\delphi\UChessBoardCmp.pas" -expected ".\test\ref\tokens\Delphi\UChessBoardCmp.txt"
test\bin\run_test.ps1 -json -name UPlatform-delphi -action tokens -language Delphi -inputfile ".\test\data\delphi\UPlatform.pas" -expected ".\test\ref\tokens\Delphi\UPlatform.txt"

# Eiffel
test\bin\run_test.ps1 -json -name hello-eiffel -action tokens -language Eiffel -inputfile ".\test\data\Eiffel\hello.e" -expected ".\test\ref\tokens\Eiffel\hello.txt"
test\bin\run_test.ps1 -json -name bakery-eiffel -action tokens -language Eiffel -inputfile ".\test\data\Eiffel\bakery.e" -expected ".\test\ref\tokens\Eiffel\bakery.txt"
test\bin\run_test.ps1 -json -name reverse-eiffel -action tokens -language Eiffel -inputfile ".\test\data\Eiffel\reverse.e" -expected ".\test\ref\tokens\Eiffel\reverse.txt"
test\bin\run_test.ps1 -json -name bottles-eiffel -action tokens -language Eiffel -inputfile ".\test\data\Eiffel\bottles_of_beer.e" -expected ".\test\ref\tokens\Eiffel\bottles_of_beer.txt"

# FORTRAN-66
test\bin\run_test.ps1 -json -name hello-ftn66 -action tokens -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\tokens\FORTRAN-66\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action tokens -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\tokens\FORTRAN-66\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action tokens -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\tokens\FORTRAN-66\HERON2.txt"
test\bin\run_test.ps1 -json -name heron-wide-ftn66 -action tokens -language FORTRAN-66 -inputfile ".\test\data\FORTRAN-66\HERON-wide.FOR" -wide -expected ".\test\ref\tokens\FORTRAN-66\HERON-wide.txt"

# FORTRAN-77
test\bin\run_test.ps1 -json -name hello-ftn77 -action tokens -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\tokens\FORTRAN-77\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action tokens -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\tokens\FORTRAN-77\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action tokens -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\tokens\FORTRAN-77\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action tokens -language FORTRAN-77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\tokens\FORTRAN-77\HERON.txt"

# Fortran-90
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\tokens\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\tokens\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\tokens\Fortran-90\hello.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\temp_sub.for" -expected ".\test\ref\tokens\Fortran-90\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\temp_func.for" -expected ".\test\ref\tokens\Fortran-90\temp_func.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn90 -action tokens -language Fortran-90 -inputfile ".\test\data\Fortran-90\Wikibooks-ex2.f95" -expected ".\test\ref\tokens\Fortran-90\Wikibooks-ex2.txt"

# Fortran-95
test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action tokens -language Fortran-95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\tokens\Fortran-95\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action tokens -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\tokens\Fortran-95\Wikibooks-ex1.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name average-ftn2003 -action tokens -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\average.f90" -expected ".\test\ref\tokens\Fortran-2003\average.txt"
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action tokens -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\tokens\Fortran-2003\geo4060.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn2003 -action tokens -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex3.f95" -expected ".\test\ref\tokens\Fortran-2003\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn2003 -action tokens -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex4.f95" -expected ".\test\ref\tokens\Fortran-2003\Wikibooks-ex4.txt"

# Fortran-2008

# F#
test\bin\run_test.ps1 -json -name samples-fs -action tokens -language Fsharp -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\tokens\Fsharp\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action tokens -language Go -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\tokens\Go\find-cli.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action tokens -language HTML -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\tokens\HTML\knuth.txt"
test\bin\run_test.ps1 -json -name developer-html -action tokens -language HTML -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\tokens\HTML\developer-css.txt"
test\bin\run_test.ps1 -json -name codestat-html -action tokens -language HTML -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\tokens\HTML\codestat-css-javascript.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action tokens -language Java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\tokens\Java\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action tokens -language Java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\tokens\Java\palindrome.txt"
test\bin\run_test.ps1 -json -name binary-search-java -action tokens -language Java -inputfile ".\test\data\Java\binary_search.java" -expected ".\test\ref\tokens\Java\binary_search.txt"
test\bin\run_test.ps1 -json -name ObjectServer-java -action tokens -language Java -inputfile ".\test\data\Java\ObjectServer.java" -expected ".\test\ref\tokens\Java\ObjectServer.txt"

# JavaScript
test\bin\run_test.ps1 -json -name values-js -action tokens -language JavaScript -inputfile ".\test\data\JavaScript\values.js" -expected ".\test\ref\tokens\JavaScript\values.txt"
test\bin\run_test.ps1 -json -name codestat-js -action tokens -language JavaScript -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\tokens\JavaScript\codestat.txt"
test\bin\run_test.ps1 -json -name bing-js -action tokens -language JavaScript -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\tokens\JavaScript\bing.txt"
test\bin\run_test.ps1 -json -name calc_prime-js -action tokens -language JavaScript -inputfile ".\test\data\JavaScript\calc_prime.js" -expected ".\test\ref\tokens\JavaScript\calc_prime.txt"
test\bin\run_test.ps1 -json -name backtick-js -action tokens -language JavaScript -inputfile ".\test\data\JavaScript\backtick.js" -expected ".\test\ref\tokens\JavaScript\backtick.txt"

# Julia
test\bin\run_test.ps1 -json -name dsp-jl -action tokens -language Julia -inputfile ".\test\data\Julia\dsp.jl" -expected ".\test\ref\tokens\Julia\dsp.txt"
test\bin\run_test.ps1 -json -name container-jl -action tokens -language Julia -inputfile ".\test\data\Julia\container.jl" -expected ".\test\ref\tokens\Julia\container.txt"
test\bin\run_test.ps1 -json -name microbiome-jl -action tokens -language Julia -inputfile ".\test\data\Julia\microbiome.jl" -expected ".\test\ref\tokens\Julia\microbiome.txt"
test\bin\run_test.ps1 -json -name periodograms-jl -action tokens -language Julia -inputfile ".\test\data\Julia\periodograms.jl" -expected ".\test\ref\tokens\Julia\periodograms.txt"

# Kotlin
test\bin\run_test.ps1 -json -name qksms-kt -action tokens -language Kotlin -inputfile ".\test\data\Kotlin\qksms.kt" -expected ".\test\ref\tokens\Kotlin\qksms.txt"
test\bin\run_test.ps1 -json -name render-kt -action tokens -language Kotlin -inputfile ".\test\data\Kotlin\render.kt" -expected ".\test\ref\tokens\Kotlin\render.txt"

# Lua
test\bin\run_test.ps1 -json -name dissector-lua -action tokens -language Lua -inputfile ".\test\data\Lua\dissector.lua" -expected ".\test\ref\tokens\Lua\dissector.txt"
test\bin\run_test.ps1 -json -name dissector2-lua -action tokens -language Lua -inputfile ".\test\data\Lua\dissector2.lua" -expected ".\test\ref\tokens\Lua\dissector2.txt"
test\bin\run_test.ps1 -json -name dsl-lua -action tokens -language Lua -inputfile ".\test\data\Lua\dsl.lua" -expected ".\test\ref\tokens\Lua\dsl.txt"
test\bin\run_test.ps1 -json -name markov-lua -action tokens -language Lua -inputfile ".\test\data\Lua\markov.lua" -expected ".\test\ref\tokens\Lua\markov.txt"

# Matlab
test\bin\run_test.ps1 -json -name transpose-matlab -action tokens -language Matlab -inputfile ".\test\data\Matlab\transpose.m" -expected ".\test\ref\tokens\Matlab\transpose.txt"
test\bin\run_test.ps1 -json -name choose-matlab -action tokens -language Matlab -inputfile ".\test\data\Matlab\choose.m" -expected ".\test\ref\tokens\Matlab\choose.txt"
test\bin\run_test.ps1 -json -name fitnormal-matlab -action tokens -language Matlab -inputfile ".\test\data\Matlab\fitnormal.m" -expected ".\test\ref\tokens\Matlab\fitnormal.txt"
test\bin\run_test.ps1 -json -name test_fitnormal-matlab -action tokens -language Matlab -inputfile ".\test\data\Matlab\test_fitnormal.m" -expected ".\test\ref\tokens\Matlab\test_fitnormal.txt"
test\bin\run_test.ps1 -json -name tshow-matlab -action tokens -language Matlab -inputfile ".\test\data\Matlab\tshow.m" -expected ".\test\ref\tokens\Matlab\tshow.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action tokens -language Objective-C -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\tokens\Objective-C\HelloWorld.txt"
test\bin\run_test.ps1 -json -name qrmath-objc -action tokens -language Objective-C -inputfile ".\test\data\Objective-C\QRMath.h" -expected ".\test\ref\tokens\Objective-C\QRMath.txt"
test\bin\run_test.ps1 -json -name qrencoder-objc -action tokens -language Objective-C -inputfile ".\test\data\Objective-C\QREncoder.m" -expected ".\test\ref\tokens\Objective-C\QREncoder.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework-pas -action tokens -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\tokens\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello-pas -action tokens -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\tokens\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose-pas -action tokens -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\tokens\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider-pas -action tokens -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\tokens\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16-pas -action tokens -language Pascal -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\tokens\Pascal\TPC16.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action tokens -language PL1-Fixed -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\tokens\PL1\BPGPLI.txt"
test\bin\run_test.ps1 -json -name checkdt-pl1 -action tokens -language PL1-Free -inputfile ".\test\data\PL1\CHECKDT.pl1" -expected ".\test\ref\tokens\PL1\CHECKDT.txt"
test\bin\run_test.ps1 -json -name crtpln3-pl1 -action tokens -language PL1-Fixed -inputfile ".\test\data\PL1\CRTPLN3.pl1" -expected ".\test\ref\tokens\PL1\CRTPLN3.txt"
test\bin\run_test.ps1 -json -name mainfact-pl1 -action tokens -language PL1 -inputfile ".\test\data\PL1\MAINFACT.pl1" -expected ".\test\ref\tokens\PL1\MAINFACT.txt"
test\bin\run_test.ps1 -json -name example-pl1 -action tokens -language PL1 -inputfile ".\test\data\PL1\example.pl1" -expected ".\test\ref\tokens\PL1\example.txt"
test\bin\run_test.ps1 -json -name digrams-pl1 -action tokens -language PL1 -inputfile ".\test\data\PL1\digrams.pl1" -expected ".\test\ref\tokens\PL1\digrams.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main-prolog -action tokens -language Prolog -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\tokens\Prolog\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb-prolog -action tokens -language Prolog -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\tokens\Prolog\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu-prolog -action tokens -language Prolog -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\tokens\Prolog\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries-prolog -action tokens -language Prolog -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\tokens\Prolog\family-queries.txt"
test\bin\run_test.ps1 -json -name web-server-hello-prolog -action tokens -language Prolog -inputfile ".\test\data\Prolog\web-server-hello.pl" -expected ".\test\ref\tokens\Prolog\web-server-hello.txt"
test\bin\run_test.ps1 -json -name web-server-params-prolog -action tokens -language Prolog -inputfile ".\test\data\Prolog\web-server-params.pl" -expected ".\test\ref\tokens\Prolog\web-server-params.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d-python -action tokens -language Python -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\tokens\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor-python -action tokens -language Python -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\tokens\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory-python -action tokens -language Python -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\tokens\Python\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-python -action tokens -language Python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\tokens\Python\values.txt"
test\bin\run_test.ps1 -json -name examiner-python -action tokens -language Python -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\tokens\Python\Examiner.txt"
test\bin\run_test.ps1 -json -name authorized-view-python -action tokens -language Python -inputfile ".\test\data\Python\authorized_view.py" -expected ".\test\ref\tokens\Python\authorized_view.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01-r -action tokens -language R -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\tokens\R\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02-r -action tokens -language R -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\tokens\R\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03-r -action tokens -language R -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\tokens\R\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04-r -action tokens -language R -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\tokens\R\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05-r -action tokens -language R -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\tokens\R\ETM-540-05.txt"
test\bin\run_test.ps1 -json -name basketball-r -action tokens -language R -inputfile ".\test\data\R\basketball.R" -expected ".\test\ref\tokens\R\basketball.txt"
test\bin\run_test.ps1 -json -name render-r -action tokens -language R -inputfile ".\test\data\R\render.R" -expected ".\test\ref\tokens\R\render.txt"

# Ruby
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

# Rust
test\bin\run_test.ps1 -json -name literals-rust -action tokens -language Rust -inputfile ".\test\data\Rust\literals.rs" -expected ".\test\ref\tokens\Rust\literals.txt"
test\bin\run_test.ps1 -json -name dom-rust -action tokens -language Rust -inputfile ".\test\data\Rust\dom.rs" -expected ".\test\ref\tokens\Rust\dom.txt"
test\bin\run_test.ps1 -json -name html-rust -action tokens -language Rust -inputfile ".\test\data\Rust\html.rs" -expected ".\test\ref\tokens\Rust\html.txt"
test\bin\run_test.ps1 -json -name geometry-rust -action tokens -language Rust -inputfile ".\test\data\Rust\geometry.rs" -expected ".\test\ref\tokens\Rust\geometry.txt"
test\bin\run_test.ps1 -json -name scene-rust -action tokens -language Rust -inputfile ".\test\data\Rust\scene.rs" -expected ".\test\ref\tokens\Rust\scene.txt"
test\bin\run_test.ps1 -json -name comments-1-rust -action tokens -language Rust -inputfile ".\test\data\Rust\nested_comments_1.rs" -expected ".\test\ref\tokens\Rust\nested_comments_1.txt"
test\bin\run_test.ps1 -json -name comments-2-rust -action tokens -language Rust -inputfile ".\test\data\Rust\nested_comments_2.rs" -expected ".\test\ref\tokens\Rust\nested_comments_2.txt"
test\bin\run_test.ps1 -json -name comments-3-rust -action tokens -language Rust -inputfile ".\test\data\Rust\nested_comments_3.rs" -expected ".\test\ref\tokens\Rust\nested_comments_3.txt"
test\bin\run_test.ps1 -json -name raw-string-1-rust -action tokens -language Rust -inputfile ".\test\data\Rust\raw_string_1.rs" -expected ".\test\ref\tokens\Rust\raw_string_1.txt"
test\bin\run_test.ps1 -json -name raw-string-2-rust -action tokens -language Rust -inputfile ".\test\data\Rust\raw_string_2.rs" -expected ".\test\ref\tokens\Rust\raw_string_2.txt"
test\bin\run_test.ps1 -json -name attributes-1-rust -action tokens -language Rust -inputfile ".\test\data\Rust\attributes_1.rs" -expected ".\test\ref\tokens\Rust\attributes_1.txt"
test\bin\run_test.ps1 -json -name chip8-rust -action tokens -language Rust -inputfile ".\test\data\Rust\chip8.rs" -expected ".\test\ref\tokens\Rust\chip8.txt"
test\bin\run_test.ps1 -json -name chip8-display-rust -action tokens -language Rust -inputfile ".\test\data\Rust\chip8-display.rs" -expected ".\test\ref\tokens\Rust\chip8-display.txt"
test\bin\run_test.ps1 -json -name chip8-instructions-rust -action tokens -language Rust -inputfile ".\test\data\Rust\chip8-instructions.rs" -expected ".\test\ref\tokens\Rust\chip8-instructions.txt"
test\bin\run_test.ps1 -json -name chip8-main-rust -action tokens -language Rust -inputfile ".\test\data\Rust\chip8-main.rs" -expected ".\test\ref\tokens\Rust\chip8-main.txt"

# Scala
test\bin\run_test.ps1 -json -name hello-scala -action tokens -language Scala -inputfile ".\test\data\Scala\hello.scala" -expected ".\test\ref\tokens\Scala\hello.txt"
test\bin\run_test.ps1 -json -name larger-scala -action tokens -language Scala -inputfile ".\test\data\Scala\larger.scala" -expected ".\test\ref\tokens\Scala\larger.txt"
test\bin\run_test.ps1 -json -name random-scala -action tokens -language Scala -inputfile ".\test\data\Scala\random.scala" -expected ".\test\ref\tokens\Scala\random.txt"
test\bin\run_test.ps1 -json -name variables-scala -action tokens -language Scala -inputfile ".\test\data\Scala\variables.scala" -expected ".\test\ref\tokens\Scala\variables.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action tokens -language SQL-92 -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\tokens\SQL-92\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action tokens -language SQL-92 -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\tokens\SQL-92\table.txt"

# SQL-99

# SQL-2003

# SQL-2008

# SQL-2011

# SQL-2016

# Swift
test\bin\run_test.ps1 -json -name AppDelegate -action tokens -language Swift -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\tokens\Swift\AppDelegate.txt"
test\bin\run_test.ps1 -json -name Meal -action tokens -language Swift -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\tokens\Swift\Meal.txt"
test\bin\run_test.ps1 -json -name MealTableViewCell -action tokens -language Swift -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\tokens\Swift\MealTableViewCell.txt"
test\bin\run_test.ps1 -json -name MealTableViewController -action tokens -language Swift -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\tokens\Swift\MealTableViewController.txt"
test\bin\run_test.ps1 -json -name MealViewController -action tokens -language Swift -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\tokens\Swift\MealViewController.txt"
test\bin\run_test.ps1 -json -name RatingControl -action tokens -language Swift -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\tokens\Swift\RatingControl.txt"
test\bin\run_test.ps1 -json -name URLExtensions -action tokens -language Swift -inputfile ".\test\data\Swift\URLExtensions.swift" -expected ".\test\ref\tokens\Swift\URLExtensions.txt"

# TypeScript
test\bin\run_test.ps1 -json -name TimeReporter-ts -action tokens -language TypeScript -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\tokens\TypeScript\TimeReporter.txt"
test\bin\run_test.ps1 -json -name ImageBoard-ts -action tokens -language TypeScript -inputfile ".\test\data\TypeScript\ImageBoard.ts" -expected ".\test\ref\tokens\TypeScript\ImageBoard.txt"

# Visual Basic 6
test\bin\run_test.ps1 -json -name spider-vb6 -action tokens -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\tokens\VisualBasic-6\spider.txt"
test\bin\run_test.ps1 -json -name azure_metadata-vb6 -action tokens -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\azure_metadata.bas" -expected ".\test\ref\tokens\VisualBasic-6\azure_metadata.txt"
test\bin\run_test.ps1 -json -name diffie_hellman-vb6 -action tokens -language VisualBasic-6 -inputfile ".\test\data\VisualBasic-6\diffie_hellman.bas" -expected ".\test\ref\tokens\VisualBasic-6\diffie_hellman.txt"

# Visual Basic .NET
test\bin\run_test.ps1 -json -name word-processor-vbnet -action tokens -language VisualBasic-NET -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\tokens\VisualBasic-NET\WordProcessor.txt"

# polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c-78 -action tokens -language C-78 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\tokens\C-78\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action tokens -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\tokens\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action tokens -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\tokens\Pascal\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-python -action tokens -language Python -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\tokens\Python\polyglot-py-rb.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-ruby -action tokens -language Ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\tokens\Ruby\polyglot-py-rb.txt"
