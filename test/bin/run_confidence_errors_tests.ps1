Set-StrictMode -Version Latest

# generic
test\bin\run_test.ps1 -json -name adabkend-adb -action confidence -errors -language generic -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\confidence-errors\generic\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action confidence -errors -language generic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\confidence-errors\generic\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action confidence -errors -language generic -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\confidence-errors\generic\parser.txt"
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
test\bin\run_test.ps1 -json -name parser -action confidence -errors -language generic -comment c -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\confidence-errors\generic-comments\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action confidence -errors -language generic -comment cobol -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\confidence-errors\generic-comments\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action confidence -errors -language generic -comment cpp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\confidence-errors\generic-comments\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action confidence -errors -language generic -comment fortran -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\confidence-errors\generic-comments\heron.txt"
test\bin\run_test.ps1 -json -name bing -action confidence -errors -language generic -comment hash -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\confidence-errors\generic-comments\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action confidence -errors -language generic -comment percent -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\confidence-errors\generic-comments\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action confidence -errors -language generic -comment hash -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\confidence-errors\generic-comments\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action confidence -errors -language generic -comment hash -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\confidence-errors\generic-comments\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action confidence -errors -language generic -comment ada -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\confidence-errors\generic-comments\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action confidence -errors -language generic -comment basic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\confidence-errors\generic-comments\spider.txt"

# generic assembly
test\bin\run_test.ps1 -json -name subroutine1-asm -action confidence -errors -language assembly -inputfile ".\test\data\ASM-360\subroutine1.asm" -expected ".\test\ref\confidence-errors\Assembly\subroutine1.txt"
test\bin\run_test.ps1 -json -name subroutine2-asm -action confidence -errors -language assembly -inputfile ".\test\data\ASM-360\subroutine2.asm" -expected ".\test\ref\confidence-errors\Assembly\subroutine2.txt"
test\bin\run_test.ps1 -json -name osint-asm -action confidence -errors -language assembly -inputfile ".\test\data\ASM-360\osint.asm" -expected ".\test\ref\confidence-errors\Assembly\osint.txt"
test\bin\run_test.ps1 -json -name spitbol-asm -action confidence -errors -language assembly -inputfile ".\test\data\ASM-360\spitbol.asm" -expected ".\test\ref\confidence-errors\Assembly\spitbol.txt"

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action confidence -errors -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-83 -action confidence -errors -language Ada-83 -inputfile ".\test\data\Ada-83\adabkend.ads" -expected ".\test\ref\confidence-errors\Ada-83\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-83 -action confidence -errors -language Ada-83 -inputfile ".\test\data\Ada-83\signup.adb" -expected ".\test\ref\confidence-errors\Ada-83\signup-adb.txt"

# Ada-95
test\bin\run_test.ps1 -json -name philosophers-ads-95 -action confidence -errors -language Ada-95 -inputfile ".\test\data\Ada-95\philosophers.ads" -expected ".\test\ref\confidence-errors\Ada-95\philosophers-ads.txt"

# Ada-2005

# Ada-2012

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

# BASIC-80
test\bin\run_test.ps1 -json -name doctor-mbasic -action confidence -errors -language BASIC-80 -inputfile ".\test\data\BASIC\doctor.bas" -expected ".\test\ref\confidence-errors\BASIC-80\doctor.txt"
test\bin\run_test.ps1 -json -name airinput-mbasic -action confidence -errors -language BASIC-80 -inputfile ".\test\data\BASIC\AIRINPUT.bas" -expected ".\test\ref\confidence-errors\BASIC-80\AIRINPUT.txt"
test\bin\run_test.ps1 -json -name backgamm-mbasic -action confidence -errors -language BASIC-80 -inputfile ".\test\data\BASIC\BACKGAMM.bas" -expected ".\test\ref\confidence-errors\BASIC-80\BACKGAMM.txt"
test\bin\run_test.ps1 -json -name planes-mbasic -action confidence -errors -language BASIC-80 -inputfile ".\test\data\BASIC\PLANES.bas" -expected ".\test\ref\confidence-errors\BASIC-80\PLANES.txt"

# BASICA
test\bin\run_test.ps1 -json -name sea-creature-basica -action confidence -errors -language BASICA -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\confidence-errors\BASICA\sea_creature.txt"
test\bin\run_test.ps1 -json -name lanturn-basica -action confidence -errors -language BASICA -inputfile ".\test\data\BASIC\lanturn.bas" -expected ".\test\ref\confidence-errors\BASICA\lanturn.txt"
test\bin\run_test.ps1 -json -name gw3d-basica -action confidence -errors -language BASICA -inputfile ".\test\data\BASIC\GW3D.bas" -expected ".\test\ref\confidence-errors\BASICA\GW3D.txt"

# C-78
test\bin\run_test.ps1 -json -name diamond-c-78 -action confidence -errors -language C-78 -inputfile ".\test\data\C-78\diamond.c" -expected ".\test\ref\confidence-errors\C-78\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test-c-78 -action confidence -errors -language C-78 -inputfile ".\test\data\C-78\prime_test.c" -expected ".\test\ref\confidence-errors\C-78\prime_test.txt"
test\bin\run_test.ps1 -json -name values-c-78 -action confidence -errors -language C-78 -inputfile ".\test\data\C-78\values.c" -expected ".\test\ref\confidence-errors\C-78\values.txt"
test\bin\run_test.ps1 -json -name j_interpreter-c-78 -action confidence -errors -language C-78 -inputfile ".\test\data\C-78\j_interpreter.c" -expected ".\test\ref\confidence-errors\C-78\j_interpreter.txt"

# C-89

# C-99
test\bin\run_test.ps1 -json -name decl-c-99 -action confidence -errors -language C-99 -inputfile ".\test\data\C-99\c-decl.c" -expected ".\test\ref\confidence-errors\C-99\c-decl-c.txt"
test\bin\run_test.ps1 -json -name parser-h-99 -action confidence -errors -language C-99 -inputfile ".\test\data\C-99\parser.h" -expected ".\test\ref\confidence-errors\C-99\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c-99 -action confidence -errors -language C-99 -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\confidence-errors\C-99\parser-c.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action confidence -errors -language CBASIC -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\confidence-errors\CBASIC\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action confidence -errors -language CBASIC -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\confidence-errors\CBASIC\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action confidence -errors -language CBASIC -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\confidence-errors\CBASIC\GRAPHR.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken1-68 -action confidence -errors -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\confidence-errors\COBOL-68\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action confidence -errors -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\confidence-errors\COBOL-68\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action confidence -errors -language COBOL-68 -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\confidence-errors\COBOL-68\mccracken3.txt"
test\bin\run_test.ps1 -json -name prog1-68 -action confidence -errors -language COBOL-68 -inputfile ".\test\data\COBOL-68\PROG1.COB" -expected ".\test\ref\confidence-errors\COBOL-68\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-68 -action confidence -errors -language COBOL-68 -inputfile ".\test\data\COBOL-68\PROG2.COB" -expected ".\test\ref\confidence-errors\COBOL-68\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-68 -action confidence -errors -language COBOL-68 -inputfile ".\test\data\COBOL-68\PROG2A.COB" -expected ".\test\ref\confidence-errors\COBOL-68\PROG2A.txt"

# COBOL-74

# COBOL-85
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
test\bin\run_test.ps1 -json -name report-cob2002 -action confidence -errors -language COBOL-2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\confidence-errors\COBOL-2002\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action confidence -errors -language COBOL-2002 -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\confidence-errors\COBOL-2002\AdventOfCode.txt"
test\bin\run_test.ps1 -json -name P010-wide-2002 -action confidence -errors -language COBOL-2002 -inputfile ".\test\data\COBOL-2002\P010-wide.COB" -wide -expected ".\test\ref\confidence-errors\COBOL-2002\P010-wide.txt"

# COBOL-2014

# COBOL-2014 with AcuCobol extensions

# COBOL-2014 with IBM extensions

# COBOL-2014 with GNU extensions
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action confidence -errors -language COBOL-2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\confidence-errors\COBOL-2014-GNU\SalesReport.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\confidence-errors\Cplusplus\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\confidence-errors\Cplusplus\hrdb.txt"
test\bin\run_test.ps1 -json -name date-h-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\confidence-errors\Cplusplus\date_h.txt"
test\bin\run_test.ps1 -json -name date-cplusplus-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\confidence-errors\Cplusplus\date_cpp.txt"
test\bin\run_test.ps1 -json -name inherit-cplusplus-cpp -action confidence -errors -language Cplusplus -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\confidence-errors\Cplusplus\inherit.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action confidence -errors -language Csharp -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\confidence-errors\Csharp\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2-cs -action confidence -errors -language Csharp -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\confidence-errors\Csharp\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3-cs -action confidence -errors -language Csharp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\confidence-errors\Csharp\calculator3.txt"

# CoffeeScript
test\bin\run_test.ps1 -json -name provider-coffee -action confidence -errors -language CoffeeScript -inputfile ".\test\data\CoffeeScript\provider.coffee" -expected ".\test\ref\confidence-errors\CoffeeScript\provider.txt"
test\bin\run_test.ps1 -json -name resque-coffee -action confidence -errors -language CoffeeScript -inputfile ".\test\data\CoffeeScript\resque.coffee" -expected ".\test\ref\confidence-errors\CoffeeScript\resque.txt"
test\bin\run_test.ps1 -json -name world-map-coffee -action confidence -errors -language CoffeeScript -inputfile ".\test\data\CoffeeScript\world_map.coffee" -expected ".\test\ref\confidence-errors\CoffeeScript\world_map.txt"

# D
test\bin\run_test.ps1 -json -name regex-d -action confidence -errors -language d -inputfile ".\test\data\d\regex.d" -expected ".\test\ref\confidence-errors\d\regex.txt"
test\bin\run_test.ps1 -json -name halffloat-d -action confidence -errors -language d -inputfile ".\test\data\d\halffloat.d" -expected ".\test\ref\confidence-errors\d\halffloat.txt"
test\bin\run_test.ps1 -json -name wc-d -action confidence -errors -language d -inputfile ".\test\data\d\wc.d" -expected ".\test\ref\confidence-errors\d\wc.txt"

# Dart
test\bin\run_test.ps1 -json -name anagram-dart -action confidence -errors -language Dart -inputfile ".\test\data\Dart\anagram.dart" -expected ".\test\ref\confidence-errors\dart\anagram.txt"
test\bin\run_test.ps1 -json -name note_client-dart -action confidence -errors -language Dart -inputfile ".\test\data\Dart\note_client.dart" -expected ".\test\ref\confidence-errors\dart\note_client.txt"
test\bin\run_test.ps1 -json -name web_app-dart -action confidence -errors -language Dart -inputfile ".\test\data\Dart\web_app.dart" -expected ".\test\ref\confidence-errors\dart\web_app.txt"

# dbase II
test\bin\run_test.ps1 -json -name sample-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\sample.prg" -expected ".\test\ref\confidence-errors\dbase-II\sample.txt"
test\bin\run_test.ps1 -json -name addm-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\ADDM.PRG" -expected ".\test\ref\confidence-errors\dbase-II\ADDM.txt"
test\bin\run_test.ps1 -json -name changedm-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\CHANGEDM.PRG" -expected ".\test\ref\confidence-errors\dbase-II\CHANGEDM.txt"
test\bin\run_test.ps1 -json -name dater-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\DATER.PRG" -expected ".\test\ref\confidence-errors\dbase-II\DATER.txt"
test\bin\run_test.ps1 -json -name updatedm-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\UPDATEDM.PRG" -expected ".\test\ref\confidence-errors\dbase-II\UPDATEDM.txt"
test\bin\run_test.ps1 -json -name viewdm-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\VIEWDM.PRG" -expected ".\test\ref\confidence-errors\dbase-II\VIEWDM.txt"
test\bin\run_test.ps1 -json -name emain-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\EMAIN.PRG" -expected ".\test\ref\confidence-errors\dbase-II\EMAIN.txt"
test\bin\run_test.ps1 -json -name emp-entr-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\EMP-ENTR.PRG" -expected ".\test\ref\confidence-errors\dbase-II\EMP-ENTR.txt"
test\bin\run_test.ps1 -json -name emp-rprt-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\EMP-RPRT.PRG" -expected ".\test\ref\confidence-errors\dbase-II\EMP-RPRT.txt"
test\bin\run_test.ps1 -json -name emp-term-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\EMP-TERM.PRG" -expected ".\test\ref\confidence-errors\dbase-II\EMP-TERM.txt"
test\bin\run_test.ps1 -json -name emp-upd-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\EMP-UPD.PRG" -expected ".\test\ref\confidence-errors\dbase-II\EMP-UPD.txt"
test\bin\run_test.ps1 -json -name invmaint-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\INVMAINT.PRG" -expected ".\test\ref\confidence-errors\dbase-II\INVMAINT.txt"
test\bin\run_test.ps1 -json -name invquan-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\INVQUAN.PRG" -expected ".\test\ref\confidence-errors\dbase-II\INVQUAN.txt"
test\bin\run_test.ps1 -json -name invread-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\INVREAD.PRG" -expected ".\test\ref\confidence-errors\dbase-II\INVREAD.txt"
test\bin\run_test.ps1 -json -name invrprt-dbii -action confidence -errors -language dbase-II -inputfile ".\test\data\dbase-II\INVRPRT.PRG" -expected ".\test\ref\confidence-errors\dbase-II\INVRPRT.txt"

# dBase III
test\bin\run_test.ps1 -json -name fdate-dbiii -action confidence -errors -language dbase-III -inputfile ".\test\data\dbase-III\FDATE.PRG" -expected ".\test\ref\confidence-errors\dbase-III\FDATE.txt"
test\bin\run_test.ps1 -json -name library-dbiii -action confidence -errors -language dbase-III -inputfile ".\test\data\dbase-III\LIBRARY.PRG" -expected ".\test\ref\confidence-errors\dbase-III\LIBRARY.txt"
test\bin\run_test.ps1 -json -name dp_sort-dbiii -action confidence -errors -language dbase-III -inputfile ".\test\data\dbase-III\DP_SORT.PRG" -expected ".\test\ref\confidence-errors\dbase-III\DP_SORT.txt"

# Delphi
test\bin\run_test.ps1 -json -name FmMain-dfm-delphi -action confidence -errors -language Delphi -inputfile ".\test\data\delphi\FmMain.dfm" -expected ".\test\ref\confidence-errors\Delphi\FmMain-dfm.txt"
test\bin\run_test.ps1 -json -name FmMain-pas-delphi -action confidence -errors -language Delphi -inputfile ".\test\data\delphi\FmMain.pas" -expected ".\test\ref\confidence-errors\Delphi\FmMain-pas.txt"
test\bin\run_test.ps1 -json -name UCalc-delphi -action confidence -errors -language Delphi -inputfile ".\test\data\delphi\UCalc.pas" -expected ".\test\ref\confidence-errors\Delphi\UCalc.txt"
test\bin\run_test.ps1 -json -name UChessBoardCmp-delphi -action confidence -errors -language Delphi -inputfile ".\test\data\delphi\UChessBoardCmp.pas" -expected ".\test\ref\confidence-errors\Delphi\UChessBoardCmp.txt"
test\bin\run_test.ps1 -json -name UPlatform-delphi -action confidence -errors -language Delphi -inputfile ".\test\data\delphi\UPlatform.pas" -expected ".\test\ref\confidence-errors\Delphi\UPlatform.txt"

# Eiffel
test\bin\run_test.ps1 -json -name hello-eiffel -action confidence -errors -language Eiffel -inputfile ".\test\data\Eiffel\hello.e" -expected ".\test\ref\confidence-errors\Eiffel\hello.txt"
test\bin\run_test.ps1 -json -name bakery-eiffel -action confidence -errors -language Eiffel -inputfile ".\test\data\Eiffel\bakery.e" -expected ".\test\ref\confidence-errors\Eiffel\bakery.txt"
test\bin\run_test.ps1 -json -name reverse-eiffel -action confidence -errors -language Eiffel -inputfile ".\test\data\Eiffel\reverse.e" -expected ".\test\ref\confidence-errors\Eiffel\reverse.txt"
test\bin\run_test.ps1 -json -name bottles-eiffel -action confidence -errors -language Eiffel -inputfile ".\test\data\Eiffel\bottles_of_beer.e" -expected ".\test\ref\confidence-errors\Eiffel\bottles_of_beer.txt"

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
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\confidence-errors\Fortran-90\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\confidence-errors\Fortran-90\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\confidence-errors\Fortran-90\hello.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\temp_sub.for" -expected ".\test\ref\confidence-errors\Fortran-90\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\temp_func.for" -expected ".\test\ref\confidence-errors\Fortran-90\temp_func.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn90 -action confidence -errors -language Fortran-90 -inputfile ".\test\data\Fortran-90\Wikibooks-ex2.f95" -expected ".\test\ref\confidence-errors\Fortran-90\Wikibooks-ex2.txt"

# Fortran-95
test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action confidence -errors -language Fortran-95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\confidence-errors\Fortran-95\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action confidence -errors -language Fortran-95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\confidence-errors\Fortran-95\Wikibooks-ex1.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name average-ftn2003 -action confidence -errors -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\average.f90" -expected ".\test\ref\confidence-errors\Fortran-2003\average.txt"
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action confidence -errors -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\confidence-errors\Fortran-2003\geo4060.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn2003 -action confidence -errors -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex3.f95" -expected ".\test\ref\confidence-errors\Fortran-2003\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn2003 -action confidence -errors -language Fortran-2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex4.f95" -expected ".\test\ref\confidence-errors\Fortran-2003\Wikibooks-ex4.txt"

# Fortran-2008

# F#
test\bin\run_test.ps1 -json -name samples-fs -action confidence -errors -language Fsharp -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\confidence-errors\Fsharp\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action confidence -errors -language Go -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\confidence-errors\Go\find-cli.txt"

# Haskell
test\bin\run_test.ps1 -json -name calendar-hs -action confidence -errors -language Haskell -inputfile ".\test\data\Haskell\calendar.hs" -expected ".\test\ref\confidence-errors\Haskell\calendar.txt"
test\bin\run_test.ps1 -json -name todo-hs -action confidence -errors -language Haskell -inputfile ".\test\data\Haskell\todo.hs" -expected ".\test\ref\confidence-errors\Haskell\todo.txt"
test\bin\run_test.ps1 -json -name rna-hs -action confidence -errors -language Haskell -inputfile ".\test\data\Haskell\todo.hs" -expected ".\test\ref\confidence-errors\Haskell\rna.txt"

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

# Julia
test\bin\run_test.ps1 -json -name dsp-jl -action confidence -errors -language Julia -inputfile ".\test\data\Julia\dsp.jl" -expected ".\test\ref\confidence-errors\Julia\dsp.txt"
test\bin\run_test.ps1 -json -name container-jl -action confidence -errors -language Julia -inputfile ".\test\data\Julia\container.jl" -expected ".\test\ref\confidence-errors\Julia\container.txt"
test\bin\run_test.ps1 -json -name microbiome-jl -action confidence -errors -language Julia -inputfile ".\test\data\Julia\microbiome.jl" -expected ".\test\ref\confidence-errors\Julia\microbiome.txt"
test\bin\run_test.ps1 -json -name periodograms-jl -action confidence -errors -language Julia -inputfile ".\test\data\Julia\periodograms.jl" -expected ".\test\ref\confidence-errors\Julia\periodograms.txt"

# Kotlin
test\bin\run_test.ps1 -json -name qksms-kt -action confidence -errors -language Kotlin -inputfile ".\test\data\Kotlin\qksms.kt" -expected ".\test\ref\confidence-errors\Kotlin\qksms.txt"
test\bin\run_test.ps1 -json -name render-kt -action confidence -errors -language Kotlin -inputfile ".\test\data\Kotlin\render.kt" -expected ".\test\ref\confidence-errors\Kotlin\render.txt"

# Lua
test\bin\run_test.ps1 -json -name dissector-lua -action confidence -errors -language Lua -inputfile ".\test\data\Lua\dissector.lua" -expected ".\test\ref\confidence-errors\Lua\dissector.txt"
test\bin\run_test.ps1 -json -name dissector2-lua -action confidence -errors -language Lua -inputfile ".\test\data\Lua\dissector2.lua" -expected ".\test\ref\confidence-errors\Lua\dissector2.txt"
test\bin\run_test.ps1 -json -name dsl-lua -action confidence -errors -language Lua -inputfile ".\test\data\Lua\dsl.lua" -expected ".\test\ref\confidence-errors\Lua\dsl.txt"
test\bin\run_test.ps1 -json -name markov-lua -action confidence -errors -language Lua -inputfile ".\test\data\Lua\markov.lua" -expected ".\test\ref\confidence-errors\Lua\markov.txt"

# Matlab
test\bin\run_test.ps1 -json -name transpose-matlab -action confidence -errors -language Matlab -inputfile ".\test\data\Matlab\transpose.m" -expected ".\test\ref\confidence-errors\Matlab\transpose.txt"
test\bin\run_test.ps1 -json -name choose-matlab -action confidence -errors -language Matlab -inputfile ".\test\data\Matlab\choose.m" -expected ".\test\ref\confidence-errors\Matlab\choose.txt"
test\bin\run_test.ps1 -json -name fitnormal-matlab -action confidence -errors -language Matlab -inputfile ".\test\data\Matlab\fitnormal.m" -expected ".\test\ref\confidence-errors\Matlab\fitnormal.txt"
test\bin\run_test.ps1 -json -name test_fitnormal-matlab -action confidence -errors -language Matlab -inputfile ".\test\data\Matlab\test_fitnormal.m" -expected ".\test\ref\confidence-errors\Matlab\test_fitnormal.txt"
test\bin\run_test.ps1 -json -name tshow-matlab -action confidence -errors -language Matlab -inputfile ".\test\data\Matlab\tshow.m" -expected ".\test\ref\confidence-errors\Matlab\tshow.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action confidence -errors -language Objective-C -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\confidence-errors\Objective-C\HelloWorld.txt"
test\bin\run_test.ps1 -json -name qrmath-objc -action confidence -errors -language Objective-C -inputfile ".\test\data\Objective-C\QRMath.h" -expected ".\test\ref\confidence-errors\Objective-C\QRMath.txt"
test\bin\run_test.ps1 -json -name qrencoder-objc -action confidence -errors -language Objective-C -inputfile ".\test\data\Objective-C\QREncoder.m" -expected ".\test\ref\confidence-errors\Objective-C\QREncoder.txt"
test\bin\run_test.ps1 -json -name jsonkit_h-objc -action confidence -errors -language Objective-C -inputfile ".\test\data\Objective-C\JSONKit.h" -expected ".\test\ref\confidence-errors\Objective-C\JSONKit_h.txt"
test\bin\run_test.ps1 -json -name jsonkit_m-objc -action confidence -errors -language Objective-C -inputfile ".\test\data\Objective-C\JSONKit.m" -expected ".\test\ref\confidence-errors\Objective-C\JSONKit_m.txt"

# Octave
test\bin\run_test.ps1 -json -name transpose-octave -action confidence -errors -language Octave -inputfile ".\test\data\Octave\transpose.m" -expected ".\test\ref\confidence-errors\Octave\transpose.txt"
test\bin\run_test.ps1 -json -name choose-octave -action confidence -errors -language Octave -inputfile ".\test\data\Octave\choose.m" -expected ".\test\ref\confidence-errors\Octave\choose.txt"
test\bin\run_test.ps1 -json -name fitnormal-octave -action confidence -errors -language Octave -inputfile ".\test\data\Octave\fitnormal.m" -expected ".\test\ref\confidence-errors\Octave\fitnormal.txt"
test\bin\run_test.ps1 -json -name test_fitnormal-octave -action confidence -errors -language Octave -inputfile ".\test\data\Octave\test_fitnormal.m" -expected ".\test\ref\confidence-errors\Octave\test_fitnormal.txt"
test\bin\run_test.ps1 -json -name tshow-octave -action confidence -errors -language Octave -inputfile ".\test\data\Octave\tshow.m" -expected ".\test\ref\confidence-errors\Octave\tshow.txt"
test\bin\run_test.ps1 -json -name ex7_pca-octave -action confidence -errors -language Octave -inputfile ".\test\data\Octave\ex7_pca.m" -expected ".\test\ref\confidence-errors\Octave\ex7_pca.txt"

# Pascal
test\bin\run_test.ps1 -json -name label_declare-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\LabelDeclaration.pas" -expected ".\test\ref\confidence-errors\Pascal\LabelDeclaration.txt"
test\bin\run_test.ps1 -json -name firework-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\confidence-errors\Pascal\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\confidence-errors\Pascal\HELLO.txt"
test\bin\run_test.ps1 -json -name rose-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\confidence-errors\Pascal\ROSE.txt"
test\bin\run_test.ps1 -json -name spider-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\confidence-errors\Pascal\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16-pas -action confidence -errors -language Pascal -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\confidence-errors\Pascal\TPC16.txt"

# Perl
test\bin\run_test.ps1 -json -name perligata-perl -action confidence -errors -language Perl -inputfile ".\test\data\Perl\Perligata.pm" -expected ".\test\ref\confidence-errors\Perl\Perligata.txt"
test\bin\run_test.ps1 -json -name physics-perl -action confidence -errors -language Perl -inputfile ".\test\data\Perl\Physics.pm" -expected ".\test\ref\confidence-errors\Perl\Physics.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action confidence -errors -language PL1-Fixed -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\confidence-errors\PL1\BPGPLI.txt"
test\bin\run_test.ps1 -json -name checkdt-pl1 -action confidence -errors -language PL1-Free -inputfile ".\test\data\PL1\CHECKDT.pl1" -expected ".\test\ref\confidence-errors\PL1\CHECKDT.txt"
test\bin\run_test.ps1 -json -name crtpln3-pl1 -action confidence -errors -language PL1-Fixed -inputfile ".\test\data\PL1\CRTPLN3.pl1" -expected ".\test\ref\confidence-errors\PL1\CRTPLN3.txt"
test\bin\run_test.ps1 -json -name mainfact-pl1 -action confidence -errors -language PL1 -inputfile ".\test\data\PL1\MAINFACT.pl1" -expected ".\test\ref\confidence-errors\PL1\MAINFACT.txt"
test\bin\run_test.ps1 -json -name example-pl1 -action confidence -errors -language PL1 -inputfile ".\test\data\PL1\example.pl1" -expected ".\test\ref\confidence-errors\PL1\example.txt"
test\bin\run_test.ps1 -json -name digrams-pl1 -action confidence -errors -language PL1 -inputfile ".\test\data\PL1\digrams.pl1" -expected ".\test\ref\confidence-errors\PL1\digrams.txt"

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

# Scala
test\bin\run_test.ps1 -json -name hello-scala -action confidence -errors -language Scala -inputfile ".\test\data\Scala\hello.scala" -expected ".\test\ref\confidence-errors\Scala\hello.txt"
test\bin\run_test.ps1 -json -name larger-scala -action confidence -errors -language Scala -inputfile ".\test\data\Scala\larger.scala" -expected ".\test\ref\confidence-errors\Scala\larger.txt"
test\bin\run_test.ps1 -json -name random-scala -action confidence -errors -language Scala -inputfile ".\test\data\Scala\random.scala" -expected ".\test\ref\confidence-errors\Scala\random.txt"
test\bin\run_test.ps1 -json -name variables-scala -action confidence -errors -language Scala -inputfile ".\test\data\Scala\variables.scala" -expected ".\test\ref\confidence-errors\Scala\variables.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action confidence -errors -language SQL-92 -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\confidence-errors\SQL-92\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action confidence -errors -language SQL-92 -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\confidence-errors\SQL-92\table.txt"

# SQL-99

# SQL-2003

# SQL-2008

# SQL-2011
test\bin\run_test.ps1 -json -name brackets-sql-2011 -action confidence -errors -language SQL-2011 -inputfile ".\test\data\SQL-2011\brackets.sql" -expected ".\test\ref\confidence-errors\SQL-2011\brackets.txt"

# SQL-2016

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
test\bin\run_test.ps1 -json -name polyglot-c-78 -action confidence -errors -language C-78 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence-errors\C-78\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action confidence -errors -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence-errors\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action confidence -errors -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\confidence-errors\Pascal\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-python -action confidence -errors -language Python -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\confidence-errors\Python\polyglot-py-rb.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-ruby -action confidence -errors -language Ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\confidence-errors\Ruby\polyglot-py-rb.txt"
