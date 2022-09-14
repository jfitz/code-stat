Set-StrictMode -Version Latest

Set-Variable -Name action "detect"

# generic assembly

# ASM IBM-360
Set-Variable -Name language "ASM-360"
test\bin\run_detect_test.ps1 -json -name subroutine1-$language -inputfile ".\test\data\ASM-360\subroutine1.asm" -expected ".\test\ref\$action\$language\subroutine1.txt"
test\bin\run_detect_test.ps1 -json -name subroutine1_a-$language -inputfile ".\test\data\ASM-360\subroutine1_a.asm" -expected ".\test\ref\$action\$language\subroutine1_a.txt"
test\bin\run_detect_test.ps1 -json -name subroutine2-$language -inputfile ".\test\data\ASM-360\subroutine2.asm" -expected ".\test\ref\$action\$language\subroutine2.txt"
test\bin\run_detect_test.ps1 -json -name subroutine2_a-$language -inputfile ".\test\data\ASM-360\subroutine2_a.asm" -expected ".\test\ref\$action\$language\subroutine2_a.txt"
test\bin\run_detect_test.ps1 -json -name spitbol-$language -inputfile ".\test\data\ASM-360\spitbol.asm" -expected ".\test\ref\$action\$language\spitbol.txt"

# ASM IBM-370
Set-Variable -Name language "ASM-370"
test\bin\run_detect_test.ps1 -json -name asm370a1-$language -inputfile ".\test\data\ASM-370\asm370a1.asm" -expected ".\test\ref\$action\$language\asm370a1.txt"

# ASM IBM-390
Set-Variable -Name language "ASM-390"
test\bin\run_detect_test.ps1 -json -name osint-$language -inputfile ".\test\data\ASM-390\osint.asm" -expected ".\test\ref\$action\$language\osint.txt"

# ASM IBM-system-z

# ASM 1802
Set-Variable -Name language "ASM-1802"
test\bin\run_detect_test.ps1 -json -name test_alu-$language -inputfile ".\test\data\ASM-1802\test_alu.asm" -expected ".\test\ref\$action\$language\test_alu.txt"

# ASM 6502
Set-Variable -Name language "ASM-6502"
test\bin\run_detect_test.ps1 -json -name sweet16-$language -inputfile ".\test\data\ASM-6502\sweet16.asm" -expected ".\test\ref\$action\$language\sweet16.txt"

# ASM 6800
Set-Variable -Name language "ASM-6800"
test\bin\run_detect_test.ps1 -json -name ET3400-S6-$language -inputfile ".\test\data\ASM-6800\ET3400-S6.asm" -expected ".\test\ref\$action\$language\ET3400-S6.txt"

# ASM 68000
Set-Variable -Name language "ASM-68000"
test\bin\run_detect_test.ps1 -json -name basic68k-$language -inputfile ".\test\data\ASM-68000\basic68k.asm" -expected ".\test\ref\$action\$language\basic68k.txt"
test\bin\run_detect_test.ps1 -json -name sprites-$language -inputfile ".\test\data\ASM-68000\sprites.asm" -expected ".\test\ref\$action\$language\sprites.txt"

# ASM 8080
Set-Variable -Name language "ASM-8080"
test\bin\run_detect_test.ps1 -json -name i8080core-$language -inputfile ".\test\data\ASM-8080\i8080core.asm" -expected ".\test\ref\$action\$language\i8080core.txt"
test\bin\run_detect_test.ps1 -json -name microcosm-$language -inputfile ".\test\data\ASM-8080\microcosm.asm" -expected ".\test\ref\$action\$language\microcosm.txt"
test\bin\run_detect_test.ps1 -json -name hello_free-$language -inputfile ".\test\data\ASM-8080\hello_free.asm" -expected ".\test\ref\$action\$language\hello_free.txt"
test\bin\run_detect_test.ps1 -json -name hello_spaced-$language -inputfile ".\test\data\ASM-8080\hello_spaced.asm" -expected ".\test\ref\$action\$language\hello_spaced.txt"

# ASM Z-80
Set-Variable -Name language "ASM-Z-80"
test\bin\run_detect_test.ps1 -json -name intro-$language -inputfile ".\test\data\ASM-Z-80\intro.asm" -expected ".\test\ref\$action\$language\intro.txt"
test\bin\run_detect_test.ps1 -json -name hardmess-$language -inputfile ".\test\data\ASM-Z-80\hardmess.asm" -expected ".\test\ref\$action\$language\hardmess.txt"
test\bin\run_detect_test.ps1 -json -name shftspr-$language -inputfile ".\test\data\ASM-Z-80\shftspr.asm" -expected ".\test\ref\$action\$language\shftspr.txt"
test\bin\run_detect_test.ps1 -json -name sincos-$language -inputfile ".\test\data\ASM-Z-80\sincos.asm" -expected ".\test\ref\$action\$language\sincos.txt"

# ASM 8086
Set-Variable -Name language "ASM-8086"
test\bin\run_detect_test.ps1 -json -name BISTRS-$language -inputfile ".\test\data\ASM-8086\BISTRS.asm" -expected ".\test\ref\$action\$language\BISTRS.txt"
test\bin\run_detect_test.ps1 -json -name DSKCOM-$language -inputfile ".\test\data\ASM-8086\DSKCOM.asm" -expected ".\test\ref\$action\$language\DSKCOM.txt"
test\bin\run_detect_test.ps1 -json -name FIVEO-$language -inputfile ".\test\data\ASM-8086\FIVEO.asm" -expected ".\test\ref\$action\$language\FIVEO.txt"
test\bin\run_detect_test.ps1 -json -name GWEVAL-$language -inputfile ".\test\data\ASM-8086\GWEVAL.asm" -expected ".\test\ref\$action\$language\GWEVAL.txt"
test\bin\run_detect_test.ps1 -json -name NEXT86-$language -inputfile ".\test\data\ASM-8086\NEXT86.asm" -expected ".\test\ref\$action\$language\NEXT86.txt"
test\bin\run_detect_test.ps1 -json -name io_indic-$language -inputfile ".\test\data\ASM-8086\io_indic.asm" -expected ".\test\ref\$action\$language\io_indic.txt"

# ASM 80386
Set-Variable -Name language "ASM-80386"
test\bin\run_detect_test.ps1 -json -name reverser-$language -inputfile ".\test\data\ASM-80386\reverser.asm" -expected ".\test\ref\$action\$language\reverser.txt"
test\bin\run_detect_test.ps1 -json -name crc16-$language -inputfile ".\test\data\ASM-80386\crc16.asm" -expected ".\test\ref\$action\$language\crc16.txt"
test\bin\run_detect_test.ps1 -json -name mat_get_element-$language -inputfile ".\test\data\ASM-80386\mat_get_element.asm" -expected ".\test\ref\$action\$language\mat_get_element.txt"

# ASM 80486

# ASM PDP-8
Set-Variable -Name language "ASM-PDP-8"
test\bin\run_detect_test.ps1 -json -name io-$language -inputfile ".\test\data\ASM-PDP-8\io.asm" -expected ".\test\ref\$action\$language\io.txt"
test\bin\run_detect_test.ps1 -json -name life-$language -inputfile ".\test\data\ASM-PDP-8\life.asm" -expected ".\test\ref\$action\$language\life.txt"
test\bin\run_detect_test.ps1 -json -name sum-$language -inputfile ".\test\data\ASM-PDP-8\sum.asm" -expected ".\test\ref\$action\$language\sum.txt"

# ASM PDP-11
Set-Variable -Name language "ASM-PDP-11"
test\bin\run_detect_test.ps1 -json -name print-$language -inputfile ".\test\data\ASM-PDP-11\print.asm" -expected ".\test\ref\$action\$language\print.txt"
test\bin\run_detect_test.ps1 -json -name k11tsx-$language -inputfile ".\test\data\ASM-PDP-11\k11tsx.mac" -expected ".\test\ref\$action\$language\k11tsx.txt"
test\bin\run_detect_test.ps1 -json -name krtser-$language -inputfile ".\test\data\ASM-PDP-11\krtser.mac" -expected ".\test\ref\$action\$language\krtser.txt"
test\bin\run_detect_test.ps1 -json -name krtsub-$language -inputfile ".\test\data\ASM-PDP-11\krtsub.mac" -expected ".\test\ref\$action\$language\krtsub.txt"

# Ada 83
Set-Variable -Name language "Ada-83"
test\bin\run_detect_test.ps1 -json -name adabkend-adb -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\$action\$language\adabkend-adb.txt"
test\bin\run_detect_test.ps1 -json -name adabkend-ads -inputfile ".\test\data\Ada-83\adabkend.ads" -expected ".\test\ref\$action\$language\adabkend-ads.txt"
test\bin\run_detect_test.ps1 -json -name signup-adb -inputfile ".\test\data\Ada-83\signup.adb" -expected ".\test\ref\$action\$language\signup-adb.txt"
test\bin\run_detect_test.ps1 -json -name naive-sort-ada-83 -inputfile ".\test\data\Ada-83\naive-sort.ada" -expected ".\test\ref\$action\$language\naive-sort-ada.txt"
test\bin\run_detect_test.ps1 -json -name naive-sort-0-ada-83 -inputfile ".\test\data\Ada-83\naive-sort-0.ada" -expected ".\test\ref\$action\$language\naive-sort-0-ada.txt"

# Ada 95
Set-Variable -Name language "Ada-95"
test\bin\run_detect_test.ps1 -json -name philosophers-ads -inputfile ".\test\data\Ada-95\philosophers.ads" -expected ".\test\ref\$action\$language\philosophers-ads.txt"

# Awk
Set-Variable -Name language "Awk"
test\bin\run_detect_test.ps1 -json -name funstack-$language -inputfile ".\test\data\Awk\funstack.awk" -expected ".\test\ref\$action\$language\funstack.txt"
test\bin\run_detect_test.ps1 -json -name awkaster-$language -inputfile ".\test\data\Awk\awkaster.awk" -expected ".\test\ref\$action\$language\awkaster.txt"

# BASIC
Set-Variable -Name language "BASIC"
test\bin\run_detect_test.ps1 -json -name values-$language -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\$action\$language\values.txt"
test\bin\run_detect_test.ps1 -json -name simple-$language -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\$action\$language\simple.txt"
test\bin\run_detect_test.ps1 -json -name 3dplot-$language -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\$action\$language\3dplot.txt"
test\bin\run_detect_test.ps1 -json -name batnum-$language -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\$action\$language\batnum.txt"
test\bin\run_detect_test.ps1 -json -name life-$language -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\$action\$language\life.txt"
test\bin\run_detect_test.ps1 -json -name income-$language -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\$action\$language\income.txt"
test\bin\run_detect_test.ps1 -json -name rockt2-$language -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\$action\$language\rockt2.txt"

# BASIC-80
Set-Variable -Name language "BASIC-80"
test\bin\run_detect_test.ps1 -json -name doctor-$language -inputfile ".\test\data\BASIC\doctor.bas" -expected ".\test\ref\$action\$language\doctor.txt"
test\bin\run_detect_test.ps1 -json -name airinput-$language -inputfile ".\test\data\BASIC\AIRINPUT.bas" -expected ".\test\ref\$action\$language\AIRINPUT.txt"
test\bin\run_detect_test.ps1 -json -name backgamm-$language -inputfile ".\test\data\BASIC\BACKGAMM.bas" -expected ".\test\ref\$action\$language\BACKGAMM.txt"
test\bin\run_detect_test.ps1 -json -name planes-$language -inputfile ".\test\data\BASIC\PLANES.bas" -expected ".\test\ref\$action\$language\PLANES.txt"
test\bin\run_detect_test.ps1 -json -name primes-$language -inputfile ".\test\data\BASIC\primes.bas" -expected ".\test\ref\$action\$language\primes.txt"
test\bin\run_detect_test.ps1 -json -name escape-$language -inputfile ".\test\data\BASIC\escape.bas" -expected ".\test\ref\$action\$language\escape.txt"

# BASICA
Set-Variable -Name language "BASICA"
test\bin\run_detect_test.ps1 -json -name sea-creature-$language -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\$action\$language\sea_creature.txt"
test\bin\run_detect_test.ps1 -json -name lanturn-$language -inputfile ".\test\data\BASIC\lanturn.bas" -expected ".\test\ref\$action\$language\lanturn.txt"
test\bin\run_detect_test.ps1 -json -name gw3d-$language -inputfile ".\test\data\BASIC\GW3D.bas" -expected ".\test\ref\$action\$language\GW3D.txt"
test\bin\run_detect_test.ps1 -json -name guotte-$language -inputfile ".\test\data\BASIC\guotte.bas" -expected ".\test\ref\$action\$language\guotte.txt"

# C-78
Set-Variable -Name language "C-78"
test\bin\run_detect_test.ps1 -json -name diamond-$language -inputfile ".\test\data\C-78\diamond.c" -expected ".\test\ref\$action\$language\diamond.txt"
test\bin\run_detect_test.ps1 -json -name prime_test-$language -inputfile ".\test\data\C-78\prime_test.c" -expected ".\test\ref\$action\$language\prime_test.txt"
test\bin\run_detect_test.ps1 -json -name values-$language -inputfile ".\test\data\C-78\values.c" -expected ".\test\ref\$action\$language\values.txt"
test\bin\run_detect_test.ps1 -json -name j_interpreter-$language -inputfile ".\test\data\C-78\j_interpreter.c" -expected ".\test\ref\$action\$language\j_interpreter.txt"
test\bin\run_detect_test.ps1 -json -name zero-line-$language -inputfile ".\test\data\C-78\zero_line_hello_world.c" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"
test\bin\run_detect_test.ps1 -json -name charlotte-$language -inputfile ".\test\data\C-78\charlotte.c" -expected ".\test\ref\$action\$language\charlotte.txt"
test\bin\run_detect_test.ps1 -json -name light_pen-$language -inputfile ".\test\data\$language\light_pen.c" -expected ".\test\ref\$action\$language\light_pen.txt"
test\bin\run_detect_test.ps1 -json -name cpaint-$language -inputfile ".\test\data\$language\cpaint.c" -expected ".\test\ref\$action\$language\cpaint_c.txt"
test\bin\run_detect_test.ps1 -json -name cpaint-h-78 -inputfile ".\test\data\$language\cpaint.h" -expected ".\test\ref\$action\$language\cpaint_h.txt"

# C-89

# C-99
Set-Variable -Name language "C-99"
test\bin\run_detect_test.ps1 -json -name c-decl-$language -inputfile ".\test\data\C-99\c-decl.c" -expected ".\test\ref\$action\$language\c-decl-c.txt"
test\bin\run_detect_test.ps1 -json -name parser-h-99 -inputfile ".\test\data\C-99\parser.h" -expected ".\test\ref\$action\$language\parser-h.txt"
test\bin\run_detect_test.ps1 -json -name parser-$language -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\$action\$language\parser-c.txt"
test\bin\run_detect_test.ps1 -json -name cleancode-$language -inputfile ".\test\data\$language\cleancode.c" -expected ".\test\ref\$action\$language\cleancode-c.txt"

# CBASIC
Set-Variable -Name language "CBASIC"
test\bin\run_detect_test.ps1 -json -name fibo-$language -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\$action\$language\fibo.txt"
test\bin\run_detect_test.ps1 -json -name demograf-$language -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\$action\$language\DEMOGRAF.txt"
test\bin\run_detect_test.ps1 -json -name graphr-$language -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\$action\$language\GRAPHR.txt"

# COBOL-68
Set-Variable -Name language "COBOL-68"
test\bin\run_detect_test.ps1 -json -name mccracken1-$language -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\$action\$language\mccracken1.txt"
test\bin\run_detect_test.ps1 -json -name mccracken2-$language -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\$action\$language\mccracken2.txt"
test\bin\run_detect_test.ps1 -json -name mccracken3-$language -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\$action\$language\mccracken3.txt"
test\bin\run_detect_test.ps1 -json -name prog1-$language -inputfile ".\test\data\COBOL-68\PROG1.COB" -expected ".\test\ref\$action\$language\PROG1.txt"
test\bin\run_detect_test.ps1 -json -name prog2-$language -inputfile ".\test\data\COBOL-68\PROG2.COB" -expected ".\test\ref\$action\$language\PROG2.txt"
test\bin\run_detect_test.ps1 -json -name prog2A-$language -inputfile ".\test\data\COBOL-68\PROG2A.COB" -expected ".\test\ref\$action\$language\PROG2A.txt"

# COBOL-74

# COBOL-85
Set-Variable -Name language "COBOL-85"
test\bin\run_detect_test.ps1 -json -name prog3-$language -inputfile ".\test\data\COBOL-85\PROG3.COB" -expected ".\test\ref\$action\$language\PROG3.txt"
test\bin\run_detect_test.ps1 -json -name prog4-$language -inputfile ".\test\data\COBOL-85\PROG4.COB" -expected ".\test\ref\$action\$language\PROG4.txt"
test\bin\run_detect_test.ps1 -json -name prog5-$language -inputfile ".\test\data\COBOL-85\PROG5.COB" -expected ".\test\ref\$action\$language\PROG5.txt"
test\bin\run_detect_test.ps1 -json -name prog6-$language -inputfile ".\test\data\COBOL-85\PROG6.COB" -expected ".\test\ref\$action\$language\PROG6.txt"
test\bin\run_detect_test.ps1 -json -name prog12-2-$language -inputfile ".\test\data\COBOL-85\PROG12-2.COB" -expected ".\test\ref\$action\$language\PROG12-2.txt"
test\bin\run_detect_test.ps1 -json -name prog13-3-$language -inputfile ".\test\data\COBOL-85\PROG13-3.COB" -expected ".\test\ref\$action\$language\PROG13-3.txt"
test\bin\run_detect_test.ps1 -json -name prog14-2-$language -inputfile ".\test\data\COBOL-85\PROG14-2.COB" -expected ".\test\ref\$action\$language\PROG14-2.txt"
test\bin\run_detect_test.ps1 -json -name prog15-4-$language -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\$action\$language\PROG15-4.txt"
test\bin\run_detect_test.ps1 -json -name exec1-$language -inputfile ".\test\data\COBOL-85\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\$action\$language\UNLDDBCU2.txt"
test\bin\run_detect_test.ps1 -json -name P010-$language -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\$action\$language\P010.txt"

# COBOL-2002
Set-Variable -Name language "COBOL-2002"
test\bin\run_detect_test.ps1 -json -name report-card-$language -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\$action\$language\ReportCard.txt"
test\bin\run_detect_test.ps1 -json -name person-$language -tabsize 4 -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\$action\$language\Person.txt"
test\bin\run_detect_test.ps1 -json -name report-$language -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\$action\$language\Report.txt"
test\bin\run_detect_test.ps1 -json -name advent-of-code-$language -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\$action\$language\AdventOfCode.txt"
test\bin\run_detect_test.ps1 -json -name P010-wide-2002 -inputfile ".\test\data\COBOL-2002\P010-wide.COB" -wide -expected ".\test\ref\$action\$language\P010-wide.txt"

# COBOL-2014

# COBOL-2014 with AcuCobol extensions

# COBOL-2014 with IBM extensions

# COBOL-2014 with GNU extensions
Set-Variable -Name language "COBOL-2014-GNU"
test\bin\run_detect_test.ps1 -json -name sales-report-$language -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\$action\$language\SalesReport.txt"

# C++
Set-Variable -Name language "Cplusplus"
test\bin\run_detect_test.ps1 -json -name checkers-$language -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\$action\$language\checkers.txt"
test\bin\run_detect_test.ps1 -json -name hrdb-$language -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\$action\$language\hrdb.txt"
test\bin\run_detect_test.ps1 -json -name date_h-$language -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\$action\$language\date_h.txt"
test\bin\run_detect_test.ps1 -json -name date_cplusplus-$language -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\$action\$language\date_cpp.txt"
test\bin\run_detect_test.ps1 -json -name inherit_cplusplus-$language -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\$action\$language\inherit.txt"

# C#
Set-Variable -Name language "Csharp"
test\bin\run_detect_test.ps1 -json -name calculator1-$language -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\$action\$language\calculator1.txt"
test\bin\run_detect_test.ps1 -json -name calculator2-$language -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\$action\$language\calculator2.txt"
test\bin\run_detect_test.ps1 -json -name calculator3-$language -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\$action\$language\calculator3.txt"

# CoffeeScript
Set-Variable -Name language "CoffeeScript"
test\bin\run_detect_test.ps1 -json -name provider-$language -inputfile ".\test\data\CoffeeScript\provider.coffee" -expected ".\test\ref\$action\$language\provider.txt"
test\bin\run_detect_test.ps1 -json -name resque-$language -inputfile ".\test\data\CoffeeScript\resque.coffee" -expected ".\test\ref\$action\$language\resque.txt"
test\bin\run_detect_test.ps1 -json -name world-map-$language -inputfile ".\test\data\CoffeeScript\world_map.coffee" -expected ".\test\ref\$action\$language\world_map.txt"

# D
Set-Variable -Name language "D"
test\bin\run_detect_test.ps1 -json -name regex-$language -inputfile ".\test\data\d\regex.d" -expected ".\test\ref\$action\$language\regex.txt"
test\bin\run_detect_test.ps1 -json -name halffloat-$language -inputfile ".\test\data\d\halffloat.d" -expected ".\test\ref\$action\$language\halffloat.txt"
test\bin\run_detect_test.ps1 -json -name wc-$language -inputfile ".\test\data\d\wc.d" -expected ".\test\ref\$action\$language\wc.txt"

# Dart
Set-Variable -Name language "Dart"
test\bin\run_detect_test.ps1 -json -name anagram-$language -inputfile ".\test\data\Dart\anagram.dart" -expected ".\test\ref\$action\$language\anagram.txt"
test\bin\run_detect_test.ps1 -json -name note_client-$language -inputfile ".\test\data\Dart\note_client.dart" -expected ".\test\ref\$action\$language\note_client.txt"
test\bin\run_detect_test.ps1 -json -name web_app-$language -inputfile ".\test\data\Dart\web_app.dart" -expected ".\test\ref\$action\$language\web_app.txt"

# dbase II
Set-Variable -Name language "dbase-ii"
test\bin\run_detect_test.ps1 -json -name sample-$language -inputfile ".\test\data\dbase-II\sample.prg" -expected ".\test\ref\$action\$language\sample.txt"
test\bin\run_detect_test.ps1 -json -name addm-$language -inputfile ".\test\data\dbase-II\ADDM.PRG" -expected ".\test\ref\$action\$language\ADDM.txt"
test\bin\run_detect_test.ps1 -json -name changedm-$language -inputfile ".\test\data\dbase-II\CHANGEDM.PRG" -expected ".\test\ref\$action\$language\CHANGEDM.txt"
test\bin\run_detect_test.ps1 -json -name dater-$language -inputfile ".\test\data\dbase-II\DATER.PRG" -expected ".\test\ref\$action\$language\DATER.txt"
test\bin\run_detect_test.ps1 -json -name updatedm-$language -inputfile ".\test\data\dbase-II\UPDATEDM.PRG" -expected ".\test\ref\$action\$language\UPDATEDM.txt"
test\bin\run_detect_test.ps1 -json -name viewdm-$language -inputfile ".\test\data\dbase-II\VIEWDM.PRG" -expected ".\test\ref\$action\$language\VIEWDM.txt"
test\bin\run_detect_test.ps1 -json -name emain-$language -inputfile ".\test\data\dbase-II\EMAIN.PRG" -expected ".\test\ref\$action\$language\EMAIN.txt"
test\bin\run_detect_test.ps1 -json -name emp-entr-$language -inputfile ".\test\data\dbase-II\EMP-ENTR.PRG" -expected ".\test\ref\$action\$language\EMP-ENTR.txt"
test\bin\run_detect_test.ps1 -json -name emp-rprt-$language -inputfile ".\test\data\dbase-II\EMP-RPRT.PRG" -expected ".\test\ref\$action\$language\EMP-RPRT.txt"
test\bin\run_detect_test.ps1 -json -name emp-term-$language -inputfile ".\test\data\dbase-II\EMP-TERM.PRG" -expected ".\test\ref\$action\$language\EMP-TERM.txt"
test\bin\run_detect_test.ps1 -json -name emp-upd-$language -inputfile ".\test\data\dbase-II\EMP-UPD.PRG" -expected ".\test\ref\$action\$language\EMP-UPD.txt"
test\bin\run_detect_test.ps1 -json -name invmaint-$language -inputfile ".\test\data\dbase-II\INVMAINT.PRG" -expected ".\test\ref\$action\$language\INVMAINT.txt"
test\bin\run_detect_test.ps1 -json -name invquan-$language -inputfile ".\test\data\dbase-II\INVQUAN.PRG" -expected ".\test\ref\$action\$language\INVQUAN.txt"
test\bin\run_detect_test.ps1 -json -name invread-$language -inputfile ".\test\data\dbase-II\INVREAD.PRG" -expected ".\test\ref\$action\$language\INVREAD.txt"
test\bin\run_detect_test.ps1 -json -name invrprt-$language -inputfile ".\test\data\dbase-II\INVRPRT.PRG" -expected ".\test\ref\$action\$language\INVRPRT.txt"

# dBase III
Set-Variable -Name language "dbase-III"
test\bin\run_detect_test.ps1 -json -name fdate-$language -inputfile ".\test\data\dbase-III\FDATE.PRG" -expected ".\test\ref\$action\$language\FDATE.txt"
test\bin\run_detect_test.ps1 -json -name library-$language -inputfile ".\test\data\dbase-III\LIBRARY.PRG" -expected ".\test\ref\$action\$language\LIBRARY.txt"
test\bin\run_detect_test.ps1 -json -name dp_sort-$language -inputfile ".\test\data\dbase-III\DP_SORT.PRG" -expected ".\test\ref\$action\$language\DP_SORT.txt"

# Delphi
Set-Variable -Name language "Delphi"
test\bin\run_detect_test.ps1 -json -name FmMain-dfm-$language -inputfile ".\test\data\delphi\FmMain.dfm" -expected ".\test\ref\$action\$language\FmMain-dfm.txt"
test\bin\run_detect_test.ps1 -json -name FmMain-$language -inputfile ".\test\data\delphi\FmMain.pas" -expected ".\test\ref\$action\$language\FmMain-pas.txt"
test\bin\run_detect_test.ps1 -json -name UCalc-$language -inputfile ".\test\data\delphi\UCalc.pas" -expected ".\test\ref\$action\$language\UCalc.txt"
test\bin\run_detect_test.ps1 -json -name UChessBoardCmp-$language -inputfile ".\test\data\delphi\UChessBoardCmp.pas" -expected ".\test\ref\$action\$language\UChessBoardCmp.txt"
test\bin\run_detect_test.ps1 -json -name UPlatform-$language -inputfile ".\test\data\delphi\UPlatform.pas" -expected ".\test\ref\$action\$language\UPlatform.txt"

# Dibol
Set-Variable -Name language "Dibol"
test\bin\run_detect_test.ps1 -json -name bottles-$language -inputfile ".\test\data\Dibol\bottles.dbl" -expected ".\test\ref\$action\$language\bottles.txt"
test\bin\run_detect_test.ps1 -json -name bilref-$language -inputfile ".\test\data\Dibol\bilref.dbl" -expected ".\test\ref\$action\$language\bilref.txt"

# Eiffel
Set-Variable -Name language "Eiffel"
test\bin\run_detect_test.ps1 -json -name hello-eiffel -inputfile ".\test\data\Eiffel\hello.e" -expected ".\test\ref\$action\$language\hello.txt"
test\bin\run_detect_test.ps1 -json -name bakery-eiffel -inputfile ".\test\data\Eiffel\bakery.e" -expected ".\test\ref\$action\$language\bakery.txt"
test\bin\run_detect_test.ps1 -json -name reverse-eiffel -inputfile ".\test\data\Eiffel\reverse.e" -expected ".\test\ref\$action\$language\reverse.txt"
test\bin\run_detect_test.ps1 -json -name bottles-eiffel -inputfile ".\test\data\Eiffel\bottles_of_beer.e" -expected ".\test\ref\$action\$language\bottles_of_beer.txt"

# Erlang
Set-Variable -Name language "Erlang"
test\bin\run_detect_test.ps1 -json -name armstrong-erlang -inputfile ".\test\data\Erlang\armstrong.erl" -expected ".\test\ref\$action\$language\armstrong.txt"
test\bin\run_detect_test.ps1 -json -name list_comprehension-erlang -inputfile ".\test\data\Erlang\list_comprehension.erl" -expected ".\test\ref\$action\$language\list_comprehension.txt"
test\bin\run_detect_test.ps1 -json -name send_receive-erlang Erlang -inputfile ".\test\data\Erlang\send_receive.erl" -expected ".\test\ref\$action\$language\send_receive.txt"

# Flowmatic
Set-Variable -Name language "Flowmatic"
test\bin\run_detect_test.ps1 -json -name example-flowmatic -inputfile ".\test\data\Flowmatic\example.txt" -expected ".\test\ref\$action\$language\example.txt"

# FORTRAN-66
Set-Variable -Name language "FORTRAN-66"
test\bin\run_detect_test.ps1 -json -name hello-ftn66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\$action\$language\HELLO.txt"
test\bin\run_detect_test.ps1 -json -name heron-ftn66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\$action\$language\HERON.txt"
test\bin\run_detect_test.ps1 -json -name heron2-ftn66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\$action\$language\HERON2.txt"
test\bin\run_detect_test.ps1 -json -name heron-wide-ftn66 -inputfile ".\test\data\FORTRAN-66\HERON-wide.FOR" -wide -expected ".\test\ref\$action\$language\HERON-wide.txt"

# FORTRAN-77
Set-Variable -Name language "FORTRAN-77"
test\bin\run_detect_test.ps1 -json -name hello-ftn77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\$action\$language\HELLO.txt"
test\bin\run_detect_test.ps1 -json -name complex-ftn77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\$action\$language\COMPLEX.txt"
test\bin\run_detect_test.ps1 -json -name euclid-ftn77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\$action\$language\EUCLID.txt"
test\bin\run_detect_test.ps1 -json -name heron-ftn77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\$action\$language\HERON.txt"

# Fortran-90
Set-Variable -Name language "Fortran-90"
test\bin\run_detect_test.ps1 -json -name cylinder-ftn90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\$action\$language\cylinder.txt"
test\bin\run_detect_test.ps1 -json -name gauss-ftn90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\$action\$language\gauss.txt"
test\bin\run_detect_test.ps1 -json -name hello-ftn90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\$action\$language\hello.txt"
test\bin\run_detect_test.ps1 -json -name temp-sub-ftn90 -inputfile ".\test\data\Fortran-90\temp_sub.for" -expected ".\test\ref\$action\$language\temp_sub.txt"
test\bin\run_detect_test.ps1 -json -name temp-func-ftn90 -inputfile ".\test\data\Fortran-90\temp_func.for" -expected ".\test\ref\$action\$language\temp_func.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex2-ftn90 -inputfile ".\test\data\Fortran-90\Wikibooks-ex2.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex2.txt"

# Fortran-95
Set-Variable -Name language "Fortran-95"
test\bin\run_detect_test.ps1 -json -name ibm-ex1-ftn95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\$action\$language\ibm-ex1.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex1-ftn95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex1.txt"

# Fortran-2003
Set-Variable -Name language "Fortran-2003"
test\bin\run_detect_test.ps1 -json -name average-ftn2003 -inputfile ".\test\data\Fortran-2003\average.f90" -expected ".\test\ref\$action\$language\average.txt"
test\bin\run_detect_test.ps1 -json -name geo4060-ftn2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\$action\$language\geo4060.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex3-ftn2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex3.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex3.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex4-ftn2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex4.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex4.txt"

# Fortran-2008

# F#
Set-Variable -Name language "Fsharp"
test\bin\run_detect_test.ps1 -json -name samples-fs -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\$action\$language\samples.txt"
test\bin\run_detect_test.ps1 -json -name antwalk-fs -inputfile ".\test\data\Fsharp\ant_walk.fs" -expected ".\test\ref\$action\$language\ant_walk.txt"

# Go
Set-Variable -Name language "Go"
test\bin\run_detect_test.ps1 -json -name find-cli-go -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\$action\$language\find-cli.txt"

# Groovy
Set-Variable -Name language "Groovy"
test\bin\run_detect_test.ps1 -json -name koan05-groovy -inputfile ".\test\data\Groovy\koan05.groovy" -expected ".\test\ref\$action\$language\koan05.txt"
test\bin\run_detect_test.ps1 -json -name koan13-groovy -inputfile ".\test\data\Groovy\koan13.groovy" -expected ".\test\ref\$action\$language\koan13.txt"

# Haskell
Set-Variable -Name language "Haskell"
test\bin\run_detect_test.ps1 -json -name calendar-hs -inputfile ".\test\data\Haskell\calendar.hs" -expected ".\test\ref\$action\$language\calendar.txt"
test\bin\run_detect_test.ps1 -json -name todo-hs -inputfile ".\test\data\Haskell\todo.hs" -expected ".\test\ref\$action\$language\todo.txt"
test\bin\run_detect_test.ps1 -json -name rna-hs -inputfile ".\test\data\Haskell\todo.hs" -expected ".\test\ref\$action\$language\rna.txt"

# HTML
Set-Variable -Name language "HTML"
test\bin\run_detect_test.ps1 -json -name knuth-html -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\$action\$language\knuth.txt"
test\bin\run_detect_test.ps1 -json -name developer-html -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\$action\$language\developer-css.txt"
test\bin\run_detect_test.ps1 -json -name codestat-html -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\$action\$language\codestat-css-javascript.txt"

# Intercal
Set-Variable -Name language "Intercal"
test\bin\run_detect_test.ps1 -json -name fizzbuzz-ical -inputfile ".\test\data\Intercal\fizzbuzz.intercal" -expected ".\test\ref\$action\$language\fizzbuzz.txt"

# Java
Set-Variable -Name language "Java"
test\bin\run_detect_test.ps1 -json -name prime_test-java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\$action\$language\prime_test.txt"
test\bin\run_detect_test.ps1 -json -name palindrome-java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\$action\$language\palindrome.txt"
test\bin\run_detect_test.ps1 -json -name binary-search-java -inputfile ".\test\data\Java\binary_search.java" -expected ".\test\ref\$action\$language\binary_search.txt"
test\bin\run_detect_test.ps1 -json -name ObjectServer-java -inputfile ".\test\data\Java\ObjectServer.java" -expected ".\test\ref\$action\$language\ObjectServer.txt"

# JavaScript
Set-Variable -Name language "JavaScript"
test\bin\run_detect_test.ps1 -json -name values-js -inputfile ".\test\data\JavaScript\values.js" -expected ".\test\ref\$action\$language\values.txt"
test\bin\run_detect_test.ps1 -json -name codestat-js -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\$action\$language\codestat.txt"
test\bin\run_detect_test.ps1 -json -name bing-js -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\$action\$language\bing.txt"
test\bin\run_detect_test.ps1 -json -name calc_prime-js -inputfile ".\test\data\JavaScript\calc_prime.js" -expected ".\test\ref\$action\$language\calc_prime.txt"
test\bin\run_detect_test.ps1 -json -name backtick-js -inputfile ".\test\data\JavaScript\backtick.js" -expected ".\test\ref\$action\$language\backtick.txt"

# Julia
Set-Variable -Name language "Julia"
test\bin\run_detect_test.ps1 -json -name dsp-jl -inputfile ".\test\data\Julia\dsp.jl" -expected ".\test\ref\$action\$language\dsp.txt"
test\bin\run_detect_test.ps1 -json -name container-jl -inputfile ".\test\data\Julia\container.jl" -expected ".\test\ref\$action\$language\container.txt"
test\bin\run_detect_test.ps1 -json -name microbiome-jl -inputfile ".\test\data\Julia\microbiome.jl" -expected ".\test\ref\$action\$language\microbiome.txt"
test\bin\run_detect_test.ps1 -json -name periodograms-jl -inputfile ".\test\data\Julia\periodograms.jl" -expected ".\test\ref\$action\$language\periodograms.txt"

# Kotlin
Set-Variable -Name language "Kotlin"
test\bin\run_detect_test.ps1 -json -name qksms-kt -inputfile ".\test\data\Kotlin\qksms.kt" -expected ".\test\ref\$action\$language\qksms.txt"
test\bin\run_detect_test.ps1 -json -name render-kt -inputfile ".\test\data\Kotlin\render.kt" -expected ".\test\ref\$action\$language\render.txt"

# Latino
Set-Variable -Name language "Latino"
test\bin\run_detect_test.ps1 -json -name major-lat -inputfile ".\test\data\$language\major.lat" -expected ".\test\ref\$action\$language\major.txt"
test\bin\run_detect_test.ps1 -json -name fibonacci-lat -inputfile ".\test\data\$language\fibonacci.lat" -expected ".\test\ref\$action\$language\fibonacci.txt"

# Lua
Set-Variable -Name language "Lua"
test\bin\run_detect_test.ps1 -json -name dissector-lua -inputfile ".\test\data\Lua\dissector.lua" -expected ".\test\ref\$action\$language\dissector.txt"
test\bin\run_detect_test.ps1 -json -name dissector2-lua -inputfile ".\test\data\Lua\dissector2.lua" -expected ".\test\ref\$action\$language\dissector2.txt"
test\bin\run_detect_test.ps1 -json -name dsl-lua -inputfile ".\test\data\Lua\dsl.lua" -expected ".\test\ref\$action\$language\dsl.txt"
test\bin\run_detect_test.ps1 -json -name markov-lua -inputfile ".\test\data\Lua\markov.lua" -expected ".\test\ref\$action\$language\markov.txt"

# Matlab
$language= "Matlab"
test\bin\run_detect_test.ps1 -json -name transpose-matlab -inputfile ".\test\data\Matlab\transpose.m" -expected ".\test\ref\$action\$language\transpose.txt"
test\bin\run_detect_test.ps1 -json -name choose-matlab -inputfile ".\test\data\Matlab\choose.m" -expected ".\test\ref\$action\$language\choose.txt"
test\bin\run_detect_test.ps1 -json -name fitnormal-matlab -inputfile ".\test\data\Matlab\fitnormal.m" -expected ".\test\ref\$action\$language\fitnormal.txt"
test\bin\run_detect_test.ps1 -json -name test_fitnormal-matlab -inputfile ".\test\data\Matlab\test_fitnormal.m" -expected ".\test\ref\$action\$language\test_fitnormal.txt"
test\bin\run_detect_test.ps1 -json -name tshow-matlab -inputfile ".\test\data\Matlab\tshow.m" -expected ".\test\ref\$action\$language\tshow.txt"

# Modula-2
Set-Variable -Name language "Modula-2"
test\bin\run_detect_test.ps1 -json -name C64ToIBM-mod2 -inputfile ".\test\data\Modula-2\C64ToIBM.mod" -expected ".\test\ref\$action\$language\C64ToIBM.txt"
test\bin\run_detect_test.ps1 -json -name CaseDemo-mod2 -inputfile ".\test\data\Modula-2\CaseDemo.mod" -expected ".\test\ref\$action\$language\CaseDemo.txt"
test\bin\run_detect_test.ps1 -json -name game_def-mod2 -inputfile ".\test\data\Modula-2\game.def" -expected ".\test\ref\$action\$language\game_def.txt"
test\bin\run_detect_test.ps1 -json -name game-mod2 -inputfile ".\test\data\Modula-2\game.mod" -expected ".\test\ref\$action\$language\game.txt"
test\bin\run_detect_test.ps1 -json -name LoopDemo-mod2 -inputfile ".\test\data\Modula-2\LoopDemo.mod" -expected ".\test\ref\$action\$language\LoopDemo.txt"
test\bin\run_detect_test.ps1 -json -name PigLatin-mod2 -inputfile ".\test\data\Modula-2\PigLatin.mod" -expected ".\test\ref\$action\$language\PigLatin.txt"
test\bin\run_detect_test.ps1 -json -name TempConv-mod2 -inputfile ".\test\data\Modula-2\TempConv.mod" -expected ".\test\ref\$action\$language\TempConv.txt"

# Objective-C
Set-Variable -Name language "Objective-C"
test\bin\run_detect_test.ps1 -json -name hello-objc -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\$action\$language\HelloWorld.txt"
test\bin\run_detect_test.ps1 -json -name qrmath-objc -inputfile ".\test\data\Objective-C\QRMath.h" -expected ".\test\ref\$action\$language\QRMath.txt"
test\bin\run_detect_test.ps1 -json -name qrencoder-objc -inputfile ".\test\data\Objective-C\QREncoder.m" -expected ".\test\ref\$action\$language\QREncoder.txt"
test\bin\run_detect_test.ps1 -json -name jsonkit_h-objc -inputfile ".\test\data\Objective-C\JSONKit.h" -expected ".\test\ref\$action\$language\JSONKit_h.txt"
test\bin\run_detect_test.ps1 -json -name jsonkit_m-objc -inputfile ".\test\data\Objective-C\JSONKit.m" -expected ".\test\ref\$action\$language\JSONKit_m.txt"

# OCaml
Set-Variable -Name language "OCaml"
test\bin\run_detect_test.ps1 -json -name antwalk-ocaml -inputfile ".\test\data\OCaml\ant_walk.ml" -expected ".\test\ref\$action\$language\ant_walk.txt"

# Octave
Set-Variable -Name language "Octave"
test\bin\run_detect_test.ps1 -json -name transpose-octave -inputfile ".\test\data\Octave\transpose.m" -expected ".\test\ref\$action\$language\transpose.txt"
test\bin\run_detect_test.ps1 -json -name choose-octave -inputfile ".\test\data\Octave\choose.m" -expected ".\test\ref\$action\$language\choose.txt"
test\bin\run_detect_test.ps1 -json -name fitnormal-octave -inputfile ".\test\data\Octave\fitnormal.m" -expected ".\test\ref\$action\$language\fitnormal.txt"
test\bin\run_detect_test.ps1 -json -name test_fitnormal-octave -inputfile ".\test\data\Octave\test_fitnormal.m" -expected ".\test\ref\$action\$language\test_fitnormal.txt"
test\bin\run_detect_test.ps1 -json -name tshow-octave -inputfile ".\test\data\Octave\tshow.m" -expected ".\test\ref\$action\$language\tshow.txt"
test\bin\run_detect_test.ps1 -json -name ex7_pca-octave -inputfile ".\test\data\Octave\ex7_pca.m" -expected ".\test\ref\$action\$language\ex7_pca.txt"

# Pascal
Set-Variable -Name language "Pascal"
test\bin\run_detect_test.ps1 -json -name label_declare-pas -inputfile ".\test\data\Pascal\LabelDeclaration.pas" -expected ".\test\ref\$action\$language\LabelDeclaration.txt"
test\bin\run_detect_test.ps1 -json -name firework-pas -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\$action\$language\FIREWORK.txt"
test\bin\run_detect_test.ps1 -json -name hello-pas -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\$action\$language\HELLO.txt"
test\bin\run_detect_test.ps1 -json -name rose-pas -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\$action\$language\ROSE.txt"
test\bin\run_detect_test.ps1 -json -name spider-pas -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\$action\v\SPIDER.txt"
test\bin\run_detect_test.ps1 -json -name tpc16-pas -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\$action\$language\TPC16.txt"

# Perl
Set-Variable -Name language "Perl"
test\bin\run_detect_test.ps1 -json -name perligata-perl -inputfile ".\test\data\Perl\Perligata.pm" -expected ".\test\ref\$action\$language\Perligata.txt"
test\bin\run_detect_test.ps1 -json -name physics-perl -inputfile ".\test\data\Perl\Physics.pm" -expected ".\test\ref\$action\$language\Physics.txt"
test\bin\run_detect_test.ps1 -json -name root_distance-perl -inputfile ".\test\data\$language\root_distance.pl" -expected ".\test\ref\$action\$language\root_distance.txt"

# PL/1
Set-Variable -Name language "PL1"
test\bin\run_detect_test.ps1 -json -name bpgpli-$language -inputfile ".\test\data\$language\BPGPLI.pl1" -expected ".\test\ref\$action\$language\BPGPLI.txt"
test\bin\run_detect_test.ps1 -json -name checkdt-$language -inputfile ".\test\data\$language\CHECKDT.pl1" -expected ".\test\ref\$action\$language\CHECKDT.txt"
test\bin\run_detect_test.ps1 -json -name crtpln3-$language -inputfile ".\test\data\$language\CRTPLN3.pl1" -expected ".\test\ref\$action\$language\CRTPLN3.txt"
test\bin\run_detect_test.ps1 -json -name mainfact-$language -inputfile ".\test\data\$language\MAINFACT.pl1" -expected ".\test\ref\$action\$language\MAINFACT.txt"
test\bin\run_detect_test.ps1 -json -name example-$language -inputfile ".\test\data\$language\example.pl1" -expected ".\test\ref\$action\$language\example.txt"
test\bin\run_detect_test.ps1 -json -name digrams-$language -inputfile ".\test\data\$language\digrams.pl1" -expected ".\test\ref\$action\$language\digrams.txt"
test\bin\run_detect_test.ps1 -json -name ystkp-$language -inputfile ".\test\data\$language\ystkp.pl1" -expected ".\test\ref\$action\$language\ystkp.txt"

# PL/M
Set-Variable -Name language "PLM"
test\bin\run_detect_test.ps1 -json -name example-plm -inputfile ".\test\data\$language\example.plm" -expected ".\test\ref\$action\$language\example.txt"

# Prolog
Set-Variable -Name language "Prolog"
test\bin\run_detect_test.ps1 -json -name family-main -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\$action\$language\family-main.txt"
test\bin\run_detect_test.ps1 -json -name family-kb -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\$action\$language\family-kb.txt"
test\bin\run_detect_test.ps1 -json -name family-menu -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\$action\$language\family-menu.txt"
test\bin\run_detect_test.ps1 -json -name family-queries -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\$action\$language\family-queries.txt"
test\bin\run_detect_test.ps1 -json -name web-server-hello-prolog -inputfile ".\test\data\Prolog\web-server-hello.pl" -expected ".\test\ref\$action\$language\web-server-hello.txt"
test\bin\run_detect_test.ps1 -json -name web-server-params-prolog -inputfile ".\test\data\Prolog\web-server-params.pl" -expected ".\test\ref\$action\$language\web-server-params.txt"

# Python
Set-Variable -Name language "Python"
test\bin\run_detect_test.ps1 -json -name drone-3d -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\$action\$language\drone_3d_trajectory_following.txt"
test\bin\run_detect_test.ps1 -json -name quadrotor -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\$action\$language\Quadrotor.txt"
test\bin\run_detect_test.ps1 -json -name trajectory -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\$action\$language\TrajectoryGenerator.txt"
test\bin\run_detect_test.ps1 -json -name values-python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\$action\$language\values.txt"
test\bin\run_detect_test.ps1 -json -name examiner -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\$action\$language\Examiner.txt"
test\bin\run_detect_test.ps1 -json -name authorized-view-python -inputfile ".\test\data\Python\authorized_view.py" -expected ".\test\ref\$action\$language\authorized_view.txt"

# R
Set-Variable -Name language "R"
test\bin\run_detect_test.ps1 -json -name ETM-540-01 -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\$action\$language\ETM-540-01.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-02 -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\$action\$language\ETM-540-02.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-03 -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\$action\$language\ETM-540-03.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-04 -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\$action\$language\ETM-540-04.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-05 -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\$action\$language\ETM-540-05.txt"
test\bin\run_detect_test.ps1 -json -name basketball-r -inputfile ".\test\data\R\basketball.R" -expected ".\test\ref\$action\$language\basketball.txt"
test\bin\run_detect_test.ps1 -json -name render-r -inputfile ".\test\data\R\render.R" -expected ".\test\ref\$action\$language\render.txt"

# Ruby
Set-Variable -Name language "Ruby"
test\bin\run_detect_test.ps1 -json -name basic-ruby -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\$action\$language\basic.txt"
test\bin\run_detect_test.ps1 -json -name constants-ruby -inputfile ".\test\data\Ruby\constants.rb" -expected ".\test\ref\$action\$language\constants.txt"
test\bin\run_detect_test.ps1 -json -name exceptions-ruby -inputfile ".\test\data\Ruby\exceptions.rb" -expected ".\test\ref\$action\$language\exceptions.txt"
test\bin\run_detect_test.ps1 -json -name expressions-ruby -inputfile ".\test\data\Ruby\expressions.rb" -expected ".\test\ref\$action\$language\expressions.txt"
test\bin\run_detect_test.ps1 -json -name functions-ruby -inputfile ".\test\data\Ruby\functions.rb" -expected ".\test\ref\$action\$language\functions.txt"
test\bin\run_detect_test.ps1 -json -name io-ruby -inputfile ".\test\data\Ruby\io.rb" -expected ".\test\ref\$action\$language\io.txt"
test\bin\run_detect_test.ps1 -json -name modifiers-ruby -inputfile ".\test\data\Ruby\modifiers.rb" -expected ".\test\ref\$action\$language\modifiers.txt"
test\bin\run_detect_test.ps1 -json -name operators-ruby -inputfile ".\test\data\Ruby\operators.rb" -expected ".\test\ref\$action\$language\operators.txt"
test\bin\run_detect_test.ps1 -json -name statements-ruby -inputfile ".\test\data\Ruby\statements.rb" -expected ".\test\ref\$action\$language\statements.txt"
test\bin\run_detect_test.ps1 -json -name tokenbuilders-ruby -inputfile ".\test\data\Ruby\tokenbuilders.rb" -expected ".\test\ref\$action\$language\tokenbuilders.txt"
test\bin\run_detect_test.ps1 -json -name tokenizers-ruby -inputfile ".\test\data\Ruby\tokenizers.rb" -expected ".\test\ref\$action\$language\tokenizers.txt"
test\bin\run_detect_test.ps1 -json -name tokens-ruby -inputfile ".\test\data\Ruby\tokens.rb" -expected ".\test\ref\$action\$language\tokens.txt"
test\bin\run_detect_test.ps1 -json -name webhook-ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\$action\$language\webhook.txt"

# Rust
Set-Variable -Name language "Rust"
test\bin\run_detect_test.ps1 -json -name literals-$language -inputfile ".\test\data\$language\literals.rs" -expected ".\test\ref\$action\$language\literals.txt"
test\bin\run_detect_test.ps1 -json -name dom-$language -inputfile ".\test\data\$language\dom.rs" -expected ".\test\ref\$action\$language\dom.txt"
test\bin\run_detect_test.ps1 -json -name html-$language -inputfile ".\test\data\$language\html.rs" -expected ".\test\ref\$action\$language\html.txt"
test\bin\run_detect_test.ps1 -json -name geometry-$language -inputfile ".\test\data\$language\geometry.rs" -expected ".\test\ref\$action\$language\geometry.txt"
test\bin\run_detect_test.ps1 -json -name scene-$language -inputfile ".\test\data\$language\scene.rs" -expected ".\test\ref\$action\$language\scene.txt"
test\bin\run_detect_test.ps1 -json -name comments-1-$language -inputfile ".\test\data\$language\nested_comments_1.rs" -expected ".\test\ref\$action\$language\nested_comments_1.txt"
test\bin\run_detect_test.ps1 -json -name comments-2-$language -inputfile ".\test\data\$language\nested_comments_2.rs" -expected ".\test\ref\$action\$language\nested_comments_2.txt"
test\bin\run_detect_test.ps1 -json -name comments-3-$language -inputfile ".\test\data\$language\nested_comments_3.rs" -expected ".\test\ref\$action\$language\nested_comments_3.txt"
test\bin\run_detect_test.ps1 -json -name raw-string-1-$language -inputfile ".\test\data\$language\raw_string_1.rs" -expected ".\test\ref\$action\$language\raw_string_1.txt"
test\bin\run_detect_test.ps1 -json -name raw-string-2-$language -inputfile ".\test\data\$language\raw_string_2.rs" -expected ".\test\ref\$action\$language\raw_string_2.txt"
test\bin\run_detect_test.ps1 -json -name attributes-1-$language -inputfile ".\test\data\$language\attributes_1.rs" -expected ".\test\ref\$action\$language\attributes_1.txt"
test\bin\run_detect_test.ps1 -json -name chip8-$language -inputfile ".\test\data\$language\chip8.rs" -expected ".\test\ref\$action\$language\chip8.txt"
test\bin\run_detect_test.ps1 -json -name chip8-display-$language -inputfile ".\test\data\$language\chip8-display.rs" -expected ".\test\ref\$action\$language\chip8-display.txt"
test\bin\run_detect_test.ps1 -json -name chip8-instructions-$language -inputfile ".\test\data\$language\chip8-instructions.rs" -expected ".\test\ref\$action\$language\chip8-instructions.txt"
test\bin\run_detect_test.ps1 -json -name chip8-main-$language -inputfile ".\test\data\$language\chip8-main.rs" -expected ".\test\ref\$action\$language\chip8-main.txt"
test\bin\run_detect_test.ps1 -json -name weird-$language -inputfile ".\test\data\$language\weird.rs" -expected ".\test\ref\$action\$language\weird.txt"

# Scala
Set-Variable -Name language "Scala"
test\bin\run_detect_test.ps1 -json -name hello-$language -inputfile ".\test\data\$language\hello.scala" -expected ".\test\ref\$action\$language\hello.txt"
test\bin\run_detect_test.ps1 -json -name larger-$language -inputfile ".\test\data\$language\larger.scala" -expected ".\test\ref\$action\$language\larger.txt"
test\bin\run_detect_test.ps1 -json -name random-$language -inputfile ".\test\data\$language\random.scala" -expected ".\test\ref\$action\$language\random.txt"
test\bin\run_detect_test.ps1 -json -name variables-$language -inputfile ".\test\data\$language\variables.scala" -expected ".\test\ref\$action\$language\variables.txt"
test\bin\run_detect_test.ps1 -json -name zero_line-$language -inputfile ".\test\data\$language\zero_line_hello_world.scala" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"
test\bin\run_detect_test.ps1 -json -name v3-$language -inputfile ".\test\data\$language\v3_control.$language" -expected ".\test\ref\$action\$language\v3_control.txt"

# SQL

# PL-SQL
Set-Variable -Name language "PL-SQL"
test\bin\run_detect_test.ps1 -json -name table-plsql -inputfile ".\test\data\$language\table.sql" -expected ".\test\ref\$action\$language\table.txt"

# T-SQL
Set-Variable -Name language "T-SQL"
test\bin\run_detect_test.ps1 -json -name microsoft-tsql -inputfile ".\test\data\$language\microsoft.sql" -expected ".\test\ref\$action\$language\microsoft.txt"
test\bin\run_detect_test.ps1 -json -name brackets-tsql -inputfile ".\test\data\$language\brackets.sql" -expected ".\test\ref\$action\$language\brackets.txt"

# Swift
Set-Variable -Name language "Swift"
test\bin\run_detect_test.ps1 -json -name AppDelegate -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\$action\$language\AppDelegate.txt"
test\bin\run_detect_test.ps1 -json -name Meal -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\$action\$language\Meal.txt"
test\bin\run_detect_test.ps1 -json -name MealTableViewCell -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\$action\$language\MealTableViewCell.txt"
test\bin\run_detect_test.ps1 -json -name MealTableViewController -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\$action\$language\MealTableViewController.txt"
test\bin\run_detect_test.ps1 -json -name MealViewController -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\$action\$language\MealViewController.txt"
test\bin\run_detect_test.ps1 -json -name RatingControl -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\$action\$language\RatingControl.txt"
test\bin\run_detect_test.ps1 -json -name URLExtensions -inputfile ".\test\data\Swift\URLExtensions.swift" -expected ".\test\ref\$action\$language\URLExtensions.txt"

# TypeScript
Set-Variable -Name language "TypeScript"
test\bin\run_detect_test.ps1 -json -name TimeReporter-ts -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\$action\$language\TimeReporter.txt"
test\bin\run_detect_test.ps1 -json -name ImageBoard-ts -inputfile ".\test\data\TypeScript\ImageBoard.ts" -expected ".\test\ref\$action\$language\ImageBoard.txt"

# Visual Basic 6
Set-Variable -Name language "VisualBasic-6"
test\bin\run_detect_test.ps1 -json -name spider-vb6 -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\$action\$language\spider.txt"
test\bin\run_detect_test.ps1 -json -name azure_metadata-vb6 -inputfile ".\test\data\VisualBasic-6\azure_metadata.bas" -expected ".\test\ref\$action\$language\azure_metadata.txt"
test\bin\run_detect_test.ps1 -json -name diffie_hellman-vb6 -inputfile ".\test\data\VisualBasic-6\diffie_hellman.bas" -expected ".\test\ref\$action\$language\diffie_hellman.txt"

# Visual Basic .NET
Set-Variable -Name language "VisualBasic-NET"
test\bin\run_detect_test.ps1 -json -name word-processor-vbnet -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\$action\$language\WordProcessor.txt"

# polyglot programs
test\bin\run_detect_test.ps1 -json -name polyglot -notiebreak -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\multiple\polyglot.txt"
test\bin\run_detect_test.ps1 -json -name polyglot-languages -notiebreak -language C-78,Cplusplus,COBOL-68,COBOL-74,cobol-85,objective-c,Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\multiple\polyglot-languages.txt"
test\bin\run_detect_test.ps1 -json -name polyglot-py-rb -notiebreak -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action\multiple\polyglot-py-rb.txt"
test\bin\run_detect_test.ps1 -json -name polyglot-py-rb-languages -notiebreak -language python,ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action\multiple\polyglot-py-rb-languages.txt"

# non-code files
test\bin\run_detect_test.ps1 -json -name noncode-empty -inputfile ".\test\data\noncode\empty.txt" -expected ".\test\ref\$action\noncode\empty.txt"
test\bin\run_detect_test.ps1 -json -name noncode-whitespace -inputfile ".\test\data\noncode\whitespace.txt" -expected ".\test\ref\$action\noncode\whitespace.txt"
test\bin\run_detect_test.ps1 -json -name noncode-win-ini -inputfile ".\test\data\noncode\win.ini" -expected ".\test\ref\$action\noncode\win.txt"
test\bin\run_detect_test.ps1 -json -name noncode-setutc -inputfile ".\test\data\noncode\setutc.reg" -expected ".\test\ref\$action\noncode\setutc.txt"
test\bin\run_detect_test.ps1 -json -name noncode-diagwrn -inputfile ".\test\data\noncode\diagwrn.xml" -expected ".\test\ref\$action\noncode\diagwrn.txt"
test\bin\run_detect_test.ps1 -json -name noncode-profile-xlsx -inputfile ".\test\data\noncode\profile.xlsx" -expected ".\test\ref\$action\noncode\profile.txt"
test\bin\run_detect_test.ps1 -json -name noncode-resume-docx -inputfile ".\test\data\noncode\resume.docx" -expected ".\test\ref\$action\noncode\resume.txt"
test\bin\run_detect_test.ps1 -json -name noncode-hh -inputfile ".\test\data\noncode\hh.exe" -expected ".\test\ref\$action\noncode\hh.txt"
