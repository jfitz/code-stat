Set-StrictMode -Version Latest

# generic
test\bin\run_test.ps1 -json -name adabkend-adb -action statistics -language generic -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\statistics\generic\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action statistics -language generic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\statistics\generic\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action statistics -language generic -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\statistics\generic\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action statistics -language generic -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\statistics\generic\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action statistics -language generic -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\statistics\generic\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action statistics -language generic -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\statistics\generic\heron.txt"
test\bin\run_test.ps1 -json -name bing -action statistics -language generic -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\statistics\generic\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action statistics -language generic -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\statistics\generic\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action statistics -language generic -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\statistics\generic\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action statistics -language generic -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\statistics\generic\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action statistics -language generic -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\statistics\generic\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action statistics -language generic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\statistics\generic\spider.txt"

# generic with comments
test\bin\run_test.ps1 -json -name adabkend-adb -action statistics -language generic -comment ada -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\statistics\generic-comments\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action statistics -language generic -comment basic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\statistics\generic-comments\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action statistics -language generic -comment c -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\statistics\generic-comments\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action statistics -language generic -comment cobol -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\statistics\generic-comments\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action statistics -language generic -comment cpp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\statistics\generic-comments\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action statistics -language generic -comment fortran -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\statistics\generic-comments\heron.txt"
test\bin\run_test.ps1 -json -name bing -action statistics -language generic -comment hash -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\statistics\generic-comments\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action statistics -language generic -comment percent -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\statistics\generic-comments\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action statistics -language generic -comment hash -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\statistics\generic-comments\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action statistics -language generic -comment hash -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\statistics\generic-comments\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action statistics -language generic -comment ada -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\statistics\generic-comments\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action statistics -language generic -comment basic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\statistics\generic-comments\spider.txt"

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action statistics -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\statistics\Ada-83\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-83 -action statistics -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.ads" -expected ".\test\ref\statistics\Ada-83\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-83 -action statistics -language Ada-83 -inputfile ".\test\data\Ada-83\signup.adb" -expected ".\test\ref\statistics\Ada-83\signup-adb.txt"

# Ada-95
test\bin\run_test.ps1 -json -name philosophers-ads-95 -action statistics -language Ada-95 -inputfile ".\test\data\Ada-95\philosophers.ads" -expected ".\test\ref\statistics\Ada-95\philosophers-ads.txt"

# Ada-2005

# Ada-2012

# Awk
test\bin\run_test.ps1 -json -name funstack-awk -action statistics -language Awk -inputfile ".\test\data\Awk\funstack.awk" -expected ".\test\ref\statistics\Awk\funstack.txt"
test\bin\run_test.ps1 -json -name awkaster-awk -action statistics -language Awk -inputfile ".\test\data\Awk\awkaster.awk" -expected ".\test\ref\statistics\Awk\awkaster.txt"

# BASIC
test\bin\run_test.ps1 -json -name values-bas -action statistics -language BASIC -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\statistics\BASIC\values.txt"
test\bin\run_test.ps1 -json -name simple-bas -action statistics -language BASIC -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\statistics\BASIC\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot-bas -action statistics -language BASIC -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\statistics\BASIC\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action statistics -language BASIC -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\statistics\BASIC\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action statistics -language BASIC -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\statistics\BASIC\life.txt"
test\bin\run_test.ps1 -json -name income-bas -action statistics -language BASIC -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\statistics\BASIC\income.txt"
test\bin\run_test.ps1 -json -name rockt2 -action statistics -language BASIC -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\statistics\BASIC\rockt2.txt"

# Microsoft BASIC
test\bin\run_test.ps1 -json -name doctor-mbasic -action statistics -language Microsoft-BASIC -inputfile ".\test\data\BASIC\doctor.bas" -expected ".\test\ref\statistics\Microsoft-BASIC\doctor.txt"

# BASICA
test\bin\run_test.ps1 -json -name sea-creature-basica -action statistics -language BASICA -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\statistics\BASICA\sea_creature.txt"

# C-78
test\bin\run_test.ps1 -json -name diamond-c-78 -action statistics -language C-78 -inputfile ".\test\data\C-78\diamond.c" -expected ".\test\ref\statistics\C-78\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test-c-78 -action statistics -language C-78 -inputfile ".\test\data\C-78\prime_test.c" -expected ".\test\ref\statistics\C-78\prime_test.txt"
test\bin\run_test.ps1 -json -name decl-c-78 -action statistics -language C-78 -inputfile ".\test\data\C-78\c-decl.c" -expected ".\test\ref\statistics\C-78\c-decl-c.txt"
test\bin\run_test.ps1 -json -name values-c-78 -action statistics -language C-78 -inputfile ".\test\data\C-78\values.c" -expected ".\test\ref\statistics\C-78\values.txt"

# C-99
test\bin\run_test.ps1 -json -name parser-h-99 -action statistics -language C-99 -inputfile ".\test\data\C-99\parser.h" -expected ".\test\ref\statistics\C-99\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c-99 -action statistics -language C-99 -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\statistics\C-99\parser-c.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action statistics -language CBASIC -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\statistics\CBASIC\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action statistics -language CBASIC -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\statistics\CBASIC\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action statistics -language CBASIC -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\statistics\CBASIC\GRAPHR.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken1-68 -action statistics -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\statistics\COBOL-68\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action statistics -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\statistics\COBOL-68\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action statistics -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\statistics\COBOL-68\mccracken3.txt"
test\bin\run_test.ps1 -json -name prog1-68 -action statistics -language COBOL-68 -inputfile ".\test\data\COBOL-68\PROG1.COB" -expected ".\test\ref\statistics\COBOL-68\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-68 -action statistics -language COBOL-68 -inputfile ".\test\data\COBOL-68\PROG2.COB" -expected ".\test\ref\statistics\COBOL-68\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-68 -action statistics -language COBOL-68 -inputfile ".\test\data\COBOL-68\PROG2A.COB" -expected ".\test\ref\statistics\COBOL-68\PROG2A.txt"

# COBOL-74

# COBOL-85
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
test\bin\run_test.ps1 -json -name report-cob2002 -action statistics -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\statistics\COBOL-2002\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action statistics -language COBOL-2002 -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\statistics\COBOL-2002\AdventOfCode.txt"
test\bin\run_test.ps1 -json -name P010-wide-2002 -action statistics -language COBOL-2002 -inputfile ".\test\data\COBOL-2002\P010-wide.COB" -wide -expected ".\test\ref\statistics\COBOL-2002\P010-wide.txt"

# COBOL-2014

# COBOL-2014 with AcuCobol extensions

# COBOL-2014 with IBM extensions

# COBOL-2014 with GNU extensions
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action statistics -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\statistics\COBOL-2014-GNU\SalesReport.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\statistics\Cplusplus\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb-cpp -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\statistics\Cplusplus\hrdb.txt"
test\bin\run_test.ps1 -json -name date-h-cpp -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\statistics\Cplusplus\date_h.txt"
test\bin\run_test.ps1 -json -name date-cplusplus-cpp -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\statistics\Cplusplus\date_cpp.txt"
test\bin\run_test.ps1 -json -name inherit-cplusplus-cpp -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\statistics\Cplusplus\inherit.txt"
test\bin\run_test.ps1 -json -name values-cpp -action statistics -language Cplusplus -inputfile ".\test\data\Cplusplus\values.cpp" -expected ".\test\ref\statistics\Cplusplus\values.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action statistics -language Csharp -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\statistics\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2-cs -action statistics -language Csharp -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\statistics\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3-cs -action statistics -language Csharp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\statistics\Csharp\calculator3.txt"

# CoffeeScript
test\bin\run_test.ps1 -json -name provider-coffee -action statistics -language CoffeeScript -inputfile ".\test\data\CoffeeScript\provider.coffee" -expected ".\test\ref\statistics\CoffeeScript\provider.txt"
test\bin\run_test.ps1 -json -name resque-coffee -action statistics -language CoffeeScript -inputfile ".\test\data\CoffeeScript\resque.coffee" -expected ".\test\ref\statistics\CoffeeScript\resque.txt"
test\bin\run_test.ps1 -json -name world-map-coffee -action statistics -language CoffeeScript -inputfile ".\test\data\CoffeeScript\world_map.coffee" -expected ".\test\ref\statistics\CoffeeScript\world_map.txt"

# D
test\bin\run_test.ps1 -json -name regex-d -action statistics -language d -inputfile ".\test\data\d\regex.d" -expected ".\test\ref\statistics\d\regex.txt"
test\bin\run_test.ps1 -json -name halffloat-d -action statistics -language d -inputfile ".\test\data\d\halffloat.d" -expected ".\test\ref\statistics\d\halffloat.txt"
test\bin\run_test.ps1 -json -name wc-d -action statistics -language d -inputfile ".\test\data\d\wc.d" -expected ".\test\ref\statistics\d\wc.txt"

# Dart
test\bin\run_test.ps1 -json -name anagram-dart -action statistics -language Dart -inputfile ".\test\data\Dart\anagram.dart" -expected ".\test\ref\statistics\dart\anagram.txt"
test\bin\run_test.ps1 -json -name note_client-dart -action statistics -language Dart -inputfile ".\test\data\Dart\note_client.dart" -expected ".\test\ref\statistics\dart\note_client.txt"
test\bin\run_test.ps1 -json -name web_app-dart -action statistics -language Dart -inputfile ".\test\data\Dart\web_app.dart" -expected ".\test\ref\statistics\dart\web_app.txt"

# dbase II
test\bin\run_test.ps1 -json -name sample-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\sample.prg" -expected ".\test\ref\statistics\dbase-II\sample.txt"
test\bin\run_test.ps1 -json -name addm-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\ADDM.PRG" -expected ".\test\ref\statistics\dbase-II\ADDM.txt"
test\bin\run_test.ps1 -json -name changedm-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\CHANGEDM.PRG" -expected ".\test\ref\statistics\dbase-II\CHANGEDM.txt"
test\bin\run_test.ps1 -json -name dater-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\DATER.PRG" -expected ".\test\ref\statistics\dbase-II\DATER.txt"
test\bin\run_test.ps1 -json -name updatedm-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\UPDATEDM.PRG" -expected ".\test\ref\statistics\dbase-II\UPDATEDM.txt"
test\bin\run_test.ps1 -json -name viewdm-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\VIEWDM.PRG" -expected ".\test\ref\statistics\dbase-II\VIEWDM.txt"
test\bin\run_test.ps1 -json -name emain-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\EMAIN.PRG" -expected ".\test\ref\statistics\dbase-II\EMAIN.txt"
test\bin\run_test.ps1 -json -name emp-entr-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\EMP-ENTR.PRG" -expected ".\test\ref\statistics\dbase-II\EMP-ENTR.txt"
test\bin\run_test.ps1 -json -name emp-rprt-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\EMP-RPRT.PRG" -expected ".\test\ref\statistics\dbase-II\EMP-RPRT.txt"
test\bin\run_test.ps1 -json -name emp-term-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\EMP-TERM.PRG" -expected ".\test\ref\statistics\dbase-II\EMP-TERM.txt"
test\bin\run_test.ps1 -json -name emp-upd-dbii -action statistics -language dbase-II -inputfile ".\test\data\dbase-II\EMP-UPD.PRG" -expected ".\test\ref\statistics\dbase-II\EMP-UPD.txt"

# dBase III
test\bin\run_test.ps1 -json -name fdate-dbiii -action statistics -language dbase-III -inputfile ".\test\data\dbase-III\FDATE.PRG" -expected ".\test\ref\statistics\dbase-III\FDATE.txt"
test\bin\run_test.ps1 -json -name library-dbiii -action statistics -language dbase-III -inputfile ".\test\data\dbase-III\LIBRARY.PRG" -expected ".\test\ref\statistics\dbase-III\LIBRARY.txt"
test\bin\run_test.ps1 -json -name dp_sort-dbiii -action statistics -language dbase-III -inputfile ".\test\data\dbase-III\DP_SORT.PRG" -expected ".\test\ref\statistics\dbase-III\DP_SORT.txt"

# Delphi
test\bin\run_test.ps1 -json -name FmMain-dfm-delphi -action statistics -language Delphi -inputfile ".\test\data\delphi\FmMain.dfm" -expected ".\test\ref\statistics\Delphi\FmMain-dfm.txt"
test\bin\run_test.ps1 -json -name FmMain-pas-delphi -action statistics -language Delphi -inputfile ".\test\data\delphi\FmMain.pas" -expected ".\test\ref\statistics\Delphi\FmMain-pas.txt"
test\bin\run_test.ps1 -json -name UCalc-delphi -action statistics -language Delphi -inputfile ".\test\data\delphi\UCalc.pas" -expected ".\test\ref\statistics\Delphi\UCalc.txt"
test\bin\run_test.ps1 -json -name UChessBoardCmp-delphi -action statistics -language Delphi -inputfile ".\test\data\delphi\UChessBoardCmp.pas" -expected ".\test\ref\statistics\Delphi\UChessBoardCmp.txt"
test\bin\run_test.ps1 -json -name UPlatform-delphi -action statistics -language Delphi -inputfile ".\test\data\delphi\UPlatform.pas" -expected ".\test\ref\statistics\Delphi\UPlatform.txt"

# Eiffel
test\bin\run_test.ps1 -json -name hello-eiffel -action statistics -language Eiffel -inputfile ".\test\data\Eiffel\hello.e" -expected ".\test\ref\statistics\Eiffel\hello.txt"
test\bin\run_test.ps1 -json -name bakery-eiffel -action statistics -language Eiffel -inputfile ".\test\data\Eiffel\bakery.e" -expected ".\test\ref\statistics\Eiffel\bakery.txt"
test\bin\run_test.ps1 -json -name reverse-eiffel -action statistics -language Eiffel -inputfile ".\test\data\Eiffel\reverse.e" -expected ".\test\ref\statistics\Eiffel\reverse.txt"
test\bin\run_test.ps1 -json -name bottles-eiffel -action statistics -language Eiffel -inputfile ".\test\data\Eiffel\bottles_of_beer.e" -expected ".\test\ref\statistics\Eiffel\bottles_of_beer.txt"

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
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\statistics\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\statistics\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\statistics\Fortran-90\hello.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\temp_sub.for" -expected ".\test\ref\statistics\Fortran-90\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\temp_func.for" -expected ".\test\ref\statistics\Fortran-90\temp_func.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn90 -action statistics -language Fortran-90 -inputfile ".\test\data\Fortran-90\Wikibooks-ex2.f95" -expected ".\test\ref\statistics\Fortran-90\Wikibooks-ex2.txt"

# Fortran-95
test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action statistics -language Fortran-95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\statistics\Fortran-95\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action statistics -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\statistics\Fortran-95\Wikibooks-ex1.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name average-ftn2003 -action statistics -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\average.f90" -expected ".\test\ref\statistics\Fortran-2003\average.txt"
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action statistics -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\statistics\Fortran-2003\geo4060.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn2003 -action statistics -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex3.f95" -expected ".\test\ref\statistics\Fortran-2003\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn2003 -action statistics -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex4.f95" -expected ".\test\ref\statistics\Fortran-2003\Wikibooks-ex4.txt"

# Fortran-2008

# F#
test\bin\run_test.ps1 -json -name samples-fs -action statistics -language Fsharp -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\statistics\Fsharp\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action statistics -language Go -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\statistics\Go\find-cli.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action statistics -language HTML -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\statistics\HTML\knuth.txt"
test\bin\run_test.ps1 -json -name developer-html -action statistics -language HTML -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\statistics\HTML\developer-css.txt"
test\bin\run_test.ps1 -json -name codestat-html -action statistics -language HTML -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\statistics\HTML\codestat-css-javascript.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action statistics -language Java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\statistics\Java\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action statistics -language Java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\statistics\Java\palindrome.txt"
test\bin\run_test.ps1 -json -name binary-search-java -action statistics -language Java -inputfile ".\test\data\Java\binary_search.java" -expected ".\test\ref\statistics\Java\binary_search.txt"
test\bin\run_test.ps1 -json -name ObjectServer-java -action statistics -language Java -inputfile ".\test\data\Java\ObjectServer.java" -expected ".\test\ref\statistics\Java\ObjectServer.txt"

# JavaScript
test\bin\run_test.ps1 -json -name values-js -action statistics -language JavaScript -inputfile ".\test\data\JavaScript\values.js" -expected ".\test\ref\statistics\JavaScript\values.txt"
test\bin\run_test.ps1 -json -name codestat-js -action statistics -language JavaScript -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\statistics\JavaScript\codestat.txt"
test\bin\run_test.ps1 -json -name bing-js -action statistics -language JavaScript -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\statistics\JavaScript\bing.txt"
test\bin\run_test.ps1 -json -name calc_prime-js -action statistics -language JavaScript -inputfile ".\test\data\JavaScript\calc_prime.js" -expected ".\test\ref\statistics\JavaScript\calc_prime.txt"
test\bin\run_test.ps1 -json -name backtick-js -action statistics -language JavaScript -inputfile ".\test\data\JavaScript\backtick.js" -expected ".\test\ref\statistics\JavaScript\backtick.txt"

# Julia
test\bin\run_test.ps1 -json -name dsp-jl -action statistics -language Julia -inputfile ".\test\data\Julia\dsp.jl" -expected ".\test\ref\statistics\Julia\dsp.txt"
test\bin\run_test.ps1 -json -name container-jl -action statistics -language Julia -inputfile ".\test\data\Julia\container.jl" -expected ".\test\ref\statistics\Julia\container.txt"
test\bin\run_test.ps1 -json -name microbiome-jl -action statistics -language Julia -inputfile ".\test\data\Julia\microbiome.jl" -expected ".\test\ref\statistics\Julia\microbiome.txt"
test\bin\run_test.ps1 -json -name periodograms-jl -action statistics -language Julia -inputfile ".\test\data\Julia\periodograms.jl" -expected ".\test\ref\statistics\Julia\periodograms.txt"

# Kotlin
test\bin\run_test.ps1 -json -name qksms-kt -action statistics -language Kotlin -inputfile ".\test\data\Kotlin\qksms.kt" -expected ".\test\ref\statistics\Kotlin\qksms.txt"
test\bin\run_test.ps1 -json -name render-kt -action statistics -language Kotlin -inputfile ".\test\data\Kotlin\render.kt" -expected ".\test\ref\statistics\Kotlin\render.txt"

# Lua
test\bin\run_test.ps1 -json -name dissector-lua -action statistics -language Lua -inputfile ".\test\data\Lua\dissector.lua" -expected ".\test\ref\statistics\Lua\dissector.txt"
test\bin\run_test.ps1 -json -name dissector2-lua -action statistics -language Lua -inputfile ".\test\data\Lua\dissector2.lua" -expected ".\test\ref\statistics\Lua\dissector2.txt"
test\bin\run_test.ps1 -json -name dsl-lua -action statistics -language Lua -inputfile ".\test\data\Lua\dsl.lua" -expected ".\test\ref\statistics\Lua\dsl.txt"
test\bin\run_test.ps1 -json -name markov-lua -action statistics -language Lua -inputfile ".\test\data\Lua\markov.lua" -expected ".\test\ref\statistics\Lua\markov.txt"

# Matlab
test\bin\run_test.ps1 -json -name transpose-matlab -action statistics -language Matlab -inputfile ".\test\data\Matlab\transpose.m" -expected ".\test\ref\statistics\Matlab\transpose.txt"
test\bin\run_test.ps1 -json -name choose-matlab -action statistics -language Matlab -inputfile ".\test\data\Matlab\choose.m" -expected ".\test\ref\statistics\Matlab\choose.txt"
test\bin\run_test.ps1 -json -name fitnormal-matlab -action statistics -language Matlab -inputfile ".\test\data\Matlab\fitnormal.m" -expected ".\test\ref\statistics\Matlab\fitnormal.txt"
test\bin\run_test.ps1 -json -name test_fitnormal-matlab -action statistics -language Matlab -inputfile ".\test\data\Matlab\test_fitnormal.m" -expected ".\test\ref\statistics\Matlab\test_fitnormal.txt"
test\bin\run_test.ps1 -json -name tshow-matlab -action statistics -language Matlab -inputfile ".\test\data\Matlab\tshow.m" -expected ".\test\ref\statistics\Matlab\tshow.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action statistics -language Objective-C -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\statistics\Objective-C\HelloWorld.txt"
test\bin\run_test.ps1 -json -name qrmath-objc -action statistics -language Objective-C -inputfile ".\test\data\Objective-C\QRMath.h" -expected ".\test\ref\statistics\Objective-C\QRMath.txt"
test\bin\run_test.ps1 -json -name qrencoder-objc -action statistics -language Objective-C -inputfile ".\test\data\Objective-C\QREncoder.m" -expected ".\test\ref\statistics\Objective-C\QREncoder.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework-pas -action statistics -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\statistics\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello-pas -action statistics -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\statistics\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose-pas -action statistics -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\statistics\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider-pas -action statistics -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\statistics\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16-pas -action statistics -language Pascal -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\statistics\Pascal\TPC16.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action statistics -language PL1-Fixed -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\statistics\PL1\BPGPLI.txt"
test\bin\run_test.ps1 -json -name checkdt-pl1 -action statistics -language PL1-Free -inputfile ".\test\data\PL1\CHECKDT.pl1" -expected ".\test\ref\statistics\PL1\CHECKDT.txt"
test\bin\run_test.ps1 -json -name crtpln3-pl1 -action statistics -language PL1-Fixed -inputfile ".\test\data\PL1\CRTPLN3.pl1" -expected ".\test\ref\statistics\PL1\CRTPLN3.txt"
test\bin\run_test.ps1 -json -name mainfact-pl1 -action statistics -language PL1 -inputfile ".\test\data\PL1\MAINFACT.pl1" -expected ".\test\ref\statistics\PL1\MAINFACT.txt"
test\bin\run_test.ps1 -json -name example-pl1 -action statistics -language PL1 -inputfile ".\test\data\PL1\example.pl1" -expected ".\test\ref\statistics\PL1\example.txt"
test\bin\run_test.ps1 -json -name digrams-pl1 -action statistics -language PL1 -inputfile ".\test\data\PL1\digrams.pl1" -expected ".\test\ref\statistics\PL1\digrams.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main-prolog -action statistics -language Prolog -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\statistics\Prolog\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb-prolog -action statistics -language Prolog -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\statistics\Prolog\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu-prolog -action statistics -language Prolog -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\statistics\Prolog\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries-prolog -action statistics -language Prolog -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\statistics\Prolog\family-queries.txt"
test\bin\run_test.ps1 -json -name web-server-hello-prolog -action statistics -language Prolog -inputfile ".\test\data\Prolog\web-server-hello.pl" -expected ".\test\ref\statistics\Prolog\web-server-hello.txt"
test\bin\run_test.ps1 -json -name web-server-params-prolog -action statistics -language Prolog -inputfile ".\test\data\Prolog\web-server-params.pl" -expected ".\test\ref\statistics\Prolog\web-server-params.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d-python -action statistics -language Python -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\statistics\Python\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor-python -action statistics -language Python -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\statistics\Python\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory-python -action statistics -language Python -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\statistics\Python\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-python -action statistics -language Python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\statistics\Python\values.txt"
test\bin\run_test.ps1 -json -name examiner-python -action statistics -language Python -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\statistics\Python\Examiner.txt"
test\bin\run_test.ps1 -json -name authorized-view-python -action statistics -language Python -inputfile ".\test\data\Python\authorized_view.py" -expected ".\test\ref\statistics\Python\authorized_view.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01-r -action statistics -language R -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\statistics\R\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02-r -action statistics -language R -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\statistics\R\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03-r -action statistics -language R -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\statistics\R\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04-r -action statistics -language R -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\statistics\R\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05-r -action statistics -language R -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\statistics\R\ETM-540-05.txt"
test\bin\run_test.ps1 -json -name basketball-r -action statistics -language R -inputfile ".\test\data\R\basketball.R" -expected ".\test\ref\statistics\R\basketball.txt"
test\bin\run_test.ps1 -json -name render-r -action statistics -language R -inputfile ".\test\data\R\render.R" -expected ".\test\ref\statistics\R\render.txt"

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

# Rust
test\bin\run_test.ps1 -json -name literals-rust -action statistics -language Rust -inputfile ".\test\data\Rust\literals.rs" -expected ".\test\ref\statistics\Rust\literals.txt"
test\bin\run_test.ps1 -json -name dom-rust -action statistics -language Rust -inputfile ".\test\data\Rust\dom.rs" -expected ".\test\ref\statistics\Rust\dom.txt"
test\bin\run_test.ps1 -json -name html-rust -action statistics -language Rust -inputfile ".\test\data\Rust\html.rs" -expected ".\test\ref\statistics\Rust\html.txt"
test\bin\run_test.ps1 -json -name geometry-rust -action statistics -language Rust -inputfile ".\test\data\Rust\geometry.rs" -expected ".\test\ref\statistics\Rust\geometry.txt"
test\bin\run_test.ps1 -json -name scene-rust -action statistics -language Rust -inputfile ".\test\data\Rust\scene.rs" -expected ".\test\ref\statistics\Rust\scene.txt"
test\bin\run_test.ps1 -json -name comments-1-rust -action statistics -language Rust -inputfile ".\test\data\Rust\nested_comments_1.rs" -expected ".\test\ref\statistics\Rust\nested_comments_1.txt"
test\bin\run_test.ps1 -json -name comments-2-rust -action statistics -language Rust -inputfile ".\test\data\Rust\nested_comments_2.rs" -expected ".\test\ref\statistics\Rust\nested_comments_2.txt"
test\bin\run_test.ps1 -json -name comments-3-rust -action statistics -language Rust -inputfile ".\test\data\Rust\nested_comments_3.rs" -expected ".\test\ref\statistics\Rust\nested_comments_3.txt"
test\bin\run_test.ps1 -json -name raw-string-1-rust -action statistics -language Rust -inputfile ".\test\data\Rust\raw_string_1.rs" -expected ".\test\ref\statistics\Rust\raw_string_1.txt"
test\bin\run_test.ps1 -json -name raw-string-2-rust -action statistics -language Rust -inputfile ".\test\data\Rust\raw_string_2.rs" -expected ".\test\ref\statistics\Rust\raw_string_2.txt"
test\bin\run_test.ps1 -json -name attributes-1-rust -action statistics -language Rust -inputfile ".\test\data\Rust\attributes_1.rs" -expected ".\test\ref\statistics\Rust\attributes_1.txt"
test\bin\run_test.ps1 -json -name chip8-rust -action statistics -language Rust -inputfile ".\test\data\Rust\chip8.rs" -expected ".\test\ref\statistics\Rust\chip8.txt"
test\bin\run_test.ps1 -json -name chip8-display-rust -action statistics -language Rust -inputfile ".\test\data\Rust\chip8-display.rs" -expected ".\test\ref\statistics\Rust\chip8-display.txt"
test\bin\run_test.ps1 -json -name chip8-instructions-rust -action statistics -language Rust -inputfile ".\test\data\Rust\chip8-instructions.rs" -expected ".\test\ref\statistics\Rust\chip8-instructions.txt"
test\bin\run_test.ps1 -json -name chip8-main-rust -action statistics -language Rust -inputfile ".\test\data\Rust\chip8-main.rs" -expected ".\test\ref\statistics\Rust\chip8-main.txt"

# Scala
test\bin\run_test.ps1 -json -name hello-scala -action statistics -language Scala -inputfile ".\test\data\Scala\hello.scala" -expected ".\test\ref\statistics\Scala\hello.txt"
test\bin\run_test.ps1 -json -name larger-scala -action statistics -language Scala -inputfile ".\test\data\Scala\larger.scala" -expected ".\test\ref\statistics\Scala\larger.txt"
test\bin\run_test.ps1 -json -name random-scala -action statistics -language Scala -inputfile ".\test\data\Scala\random.scala" -expected ".\test\ref\statistics\Scala\random.txt"
test\bin\run_test.ps1 -json -name variables-scala -action statistics -language Scala -inputfile ".\test\data\Scala\variables.scala" -expected ".\test\ref\statistics\Scala\variables.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action statistics -language SQL-92 -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\statistics\SQL-92\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action statistics -language SQL-92 -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\statistics\SQL-92\table.txt"

# SQL-99

# SQL-2003

# SQL-2008

# SQL-2011
test\bin\run_test.ps1 -json -name brackets-sql-2011 -action statistics -language SQL-2011 -inputfile ".\test\data\SQL-2011\brackets.sql" -expected ".\test\ref\statistics\SQL-2011\brackets.txt"

# SQL-2016

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
test\bin\run_test.ps1 -json -name polyglot-c-78 -action statistics -language C-78 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\statistics\C-78\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action statistics -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\statistics\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action statistics -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\statistics\Pascal\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-python -action statistics -language Python -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\statistics\Python\polyglot-py-rb.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-ruby -action statistics -language Ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\statistics\Ruby\polyglot-py-rb.txt"
