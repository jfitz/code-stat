Set-StrictMode -Version Latest

Set-Variable -Name action "detect"

# generic assembly

# ASM iBM-360
test\bin\run_detect_test.ps1 -json -name subroutine1-asm -inputfile ".\test\data\ASM-360\subroutine1.asm" -expected ".\test\ref\detect\ASM-360\subroutine1.txt"
test\bin\run_detect_test.ps1 -json -name subroutine1_a-asm -inputfile ".\test\data\ASM-360\subroutine1_a.asm" -expected ".\test\ref\detect\ASM-360\subroutine1_a.txt"
test\bin\run_detect_test.ps1 -json -name subroutine2-asm -inputfile ".\test\data\ASM-360\subroutine2.asm" -expected ".\test\ref\detect\ASM-360\subroutine2.txt"
test\bin\run_detect_test.ps1 -json -name subroutine2_a-asm -inputfile ".\test\data\ASM-360\subroutine2_a.asm" -expected ".\test\ref\detect\ASM-360\subroutine2_a.txt"
test\bin\run_detect_test.ps1 -json -name spitbol-asm -inputfile ".\test\data\ASM-360\spitbol.asm" -expected ".\test\ref\detect\ASM-360\spitbol.txt"

# ASM IBM-370
test\bin\run_detect_test.ps1 -json -name asm370a1-asm -inputfile ".\test\data\ASM-370\asm370a1.asm" -expected ".\test\ref\detect\ASM-370\asm370a1.txt"

# ASM IBM-390
test\bin\run_detect_test.ps1 -json -name osint-asm -inputfile ".\test\data\ASM-390\osint.asm" -expected ".\test\ref\detect\ASM-390\osint.txt"

# ASM IBM-system-z

# ASM 1802
test\bin\run_detect_test.ps1 -json -name test_alu-asm -inputfile ".\test\data\ASM-1802\test_alu.asm" -expected ".\test\ref\detect\ASM-1802\test_alu.txt"

# ASM 6502
test\bin\run_detect_test.ps1 -json -name sweet16-asm -inputfile ".\test\data\ASM-6502\sweet16.asm" -expected ".\test\ref\detect\ASM-6502\sweet16.txt"

# ASM 6800
Set-Variable -Name language "ASM-6800"
test\bin\run_detect_test.ps1 -json -name ET3400-S6-asm -inputfile ".\test\data\ASM-6800\ET3400-S6.asm" -expected ".\test\ref\detect\ASM-6800\ET3400-S6.txt"

# ASM 68000
Set-Variable -Name language "ASM-68000"
test\bin\run_detect_test.ps1 -json -name basic68k-asm -inputfile ".\test\data\ASM-68000\basic68k.asm" -expected ".\test\ref\detect\ASM-68000\basic68k.txt"
test\bin\run_detect_test.ps1 -json -name sprites-asm -inputfile ".\test\data\ASM-68000\sprites.asm" -expected ".\test\ref\detect\ASM-68000\sprites.txt"

# ASM 8080
Set-Variable -Name language "ASM-8080"
test\bin\run_detect_test.ps1 -json -name i8080core-asm -inputfile ".\test\data\ASM-8080\i8080core.asm" -expected ".\test\ref\detect\ASM-8080\i8080core.txt"
test\bin\run_detect_test.ps1 -json -name microcosm-asm -inputfile ".\test\data\ASM-8080\microcosm.asm" -expected ".\test\ref\detect\ASM-8080\microcosm.txt"
test\bin\run_detect_test.ps1 -json -name hello_free-asm -inputfile ".\test\data\ASM-8080\hello_free.asm" -expected ".\test\ref\detect\ASM-8080\hello_free.txt"
test\bin\run_detect_test.ps1 -json -name hello_spaced-asm -inputfile ".\test\data\ASM-8080\hello_spaced.asm" -expected ".\test\ref\detect\ASM-8080\hello_spaced.txt"

# ASM Z-80
Set-Variable -Name language "ASM-Z-80"
test\bin\run_detect_test.ps1 -json -name intro-asm -inputfile ".\test\data\ASM-Z-80\intro.asm" -expected ".\test\ref\detect\ASM-Z-80\intro.txt"
test\bin\run_detect_test.ps1 -json -name hardmess-asm -inputfile ".\test\data\ASM-Z-80\hardmess.asm" -expected ".\test\ref\detect\ASM-Z-80\hardmess.txt"
test\bin\run_detect_test.ps1 -json -name shftspr-asm -inputfile ".\test\data\ASM-Z-80\shftspr.asm" -expected ".\test\ref\detect\ASM-Z-80\shftspr.txt"
test\bin\run_detect_test.ps1 -json -name sincos-asm -inputfile ".\test\data\ASM-Z-80\sincos.asm" -expected ".\test\ref\detect\ASM-Z-80\sincos.txt"

# ASM 8086
Set-Variable -Name language "ASM-8086"
test\bin\run_detect_test.ps1 -json -name BISTRS-asm -inputfile ".\test\data\ASM-8086\BISTRS.asm" -expected ".\test\ref\detect\ASM-8086\BISTRS.txt"
test\bin\run_detect_test.ps1 -json -name DSKCOM-asm -inputfile ".\test\data\ASM-8086\DSKCOM.asm" -expected ".\test\ref\detect\ASM-8086\DSKCOM.txt"
test\bin\run_detect_test.ps1 -json -name FIVEO-asm -inputfile ".\test\data\ASM-8086\FIVEO.asm" -expected ".\test\ref\detect\ASM-8086\FIVEO.txt"
test\bin\run_detect_test.ps1 -json -name GWEVAL-asm -inputfile ".\test\data\ASM-8086\GWEVAL.asm" -expected ".\test\ref\detect\ASM-8086\GWEVAL.txt"
test\bin\run_detect_test.ps1 -json -name NEXT86-asm -inputfile ".\test\data\ASM-8086\NEXT86.asm" -expected ".\test\ref\detect\ASM-8086\NEXT86.txt"
test\bin\run_detect_test.ps1 -json -name io_indic-asm -inputfile ".\test\data\ASM-8086\io_indic.asm" -expected ".\test\ref\detect\ASM-8086\io_indic.txt"

# ASM 80386
Set-Variable -Name language "ASM-80386"
test\bin\run_detect_test.ps1 -json -name reverser-asm -inputfile ".\test\data\ASM-80386\reverser.asm" -expected ".\test\ref\detect\ASM-80386\reverser.txt"
test\bin\run_detect_test.ps1 -json -name crc16-asm -inputfile ".\test\data\ASM-80386\crc16.asm" -expected ".\test\ref\detect\ASM-80386\crc16.txt"
test\bin\run_detect_test.ps1 -json -name mat_get_element-asm -inputfile ".\test\data\ASM-80386\mat_get_element.asm" -expected ".\test\ref\detect\ASM-80386\mat_get_element.txt"

# ASM 80486

# ASM PDP-8
Set-Variable -Name language "ASM-PDP-8"
test\bin\run_detect_test.ps1 -json -name io-asm -inputfile ".\test\data\ASM-PDP-8\io.asm" -expected ".\test\ref\detect\ASM-PDP-8\io.txt"
test\bin\run_detect_test.ps1 -json -name life-asm -inputfile ".\test\data\ASM-PDP-8\life.asm" -expected ".\test\ref\detect\ASM-PDP-8\life.txt"
test\bin\run_detect_test.ps1 -json -name sum-asm -inputfile ".\test\data\ASM-PDP-8\sum.asm" -expected ".\test\ref\detect\ASM-PDP-8\sum.txt"

# ASM PDP-11
Set-Variable -Name language "ASM-PDP-11"
test\bin\run_detect_test.ps1 -json -name print-asm -inputfile ".\test\data\ASM-PDP-11\print.asm" -expected ".\test\ref\detect\ASM-PDP-11\print.txt"
test\bin\run_detect_test.ps1 -json -name k11tsx-asm -inputfile ".\test\data\ASM-PDP-11\k11tsx.mac" -expected ".\test\ref\detect\ASM-PDP-11\k11tsx.txt"
test\bin\run_detect_test.ps1 -json -name krtser-asm -inputfile ".\test\data\ASM-PDP-11\krtser.mac" -expected ".\test\ref\detect\ASM-PDP-11\krtser.txt"
test\bin\run_detect_test.ps1 -json -name krtsub-asm -inputfile ".\test\data\ASM-PDP-11\krtsub.mac" -expected ".\test\ref\detect\ASM-PDP-11\krtsub.txt"

# Ada 83
Set-Variable -Name language "Ada-83"
test\bin\run_detect_test.ps1 -json -name adabkend-adb -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\detect\Ada-83\adabkend-adb.txt"
test\bin\run_detect_test.ps1 -json -name adabkend-ads -inputfile ".\test\data\Ada-83\adabkend.ads" -expected ".\test\ref\detect\Ada-83\adabkend-ads.txt"
test\bin\run_detect_test.ps1 -json -name signup-adb -inputfile ".\test\data\Ada-83\signup.adb" -expected ".\test\ref\detect\Ada-83\signup-adb.txt"
test\bin\run_detect_test.ps1 -json -name naive-sort-ada-83 -inputfile ".\test\data\Ada-83\naive-sort.ada" -expected ".\test\ref\detect\Ada-83\naive-sort-ada.txt"
test\bin\run_detect_test.ps1 -json -name naive-sort-0-ada-83 -inputfile ".\test\data\Ada-83\naive-sort-0.ada" -expected ".\test\ref\detect\Ada-83\naive-sort-0-ada.txt"

# Ada 95
Set-Variable -Name language "Ada-95"
test\bin\run_detect_test.ps1 -json -name philosophers-ads -inputfile ".\test\data\Ada-95\philosophers.ads" -expected ".\test\ref\detect\Ada-95\philosophers-ads.txt"

# Awk
Set-Variable -Name language "Awk"
test\bin\run_detect_test.ps1 -json -name funstack-awk -inputfile ".\test\data\Awk\funstack.awk" -expected ".\test\ref\detect\Awk\funstack.txt"
test\bin\run_detect_test.ps1 -json -name awkaster-awk -inputfile ".\test\data\Awk\awkaster.awk" -expected ".\test\ref\detect\Awk\awkaster.txt"

# BASIC
Set-Variable -Name language "BASIC"
test\bin\run_detect_test.ps1 -json -name values -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\detect\BASIC\values.txt"
test\bin\run_detect_test.ps1 -json -name simple -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\detect\BASIC\simple.txt"
test\bin\run_detect_test.ps1 -json -name 3dplot -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\detect\BASIC\3dplot.txt"
test\bin\run_detect_test.ps1 -json -name batnum -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\detect\BASIC\batnum.txt"
test\bin\run_detect_test.ps1 -json -name life -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\detect\BASIC\life.txt"
test\bin\run_detect_test.ps1 -json -name income -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\detect\BASIC\income.txt"
test\bin\run_detect_test.ps1 -json -name rockt2 -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\detect\BASIC\rockt2.txt"

# BASIC-80
Set-Variable -Name language "BASIC-80"
test\bin\run_detect_test.ps1 -json -name doctor-mbasic -inputfile ".\test\data\BASIC\doctor.bas" -expected ".\test\ref\$action\$language\doctor.txt"
test\bin\run_detect_test.ps1 -json -name airinput-mbasic -inputfile ".\test\data\BASIC\AIRINPUT.bas" -expected ".\test\ref\$action\$language\AIRINPUT.txt"
test\bin\run_detect_test.ps1 -json -name backgamm-mbasic -inputfile ".\test\data\BASIC\BACKGAMM.bas" -expected ".\test\ref\$action\$language\BACKGAMM.txt"
test\bin\run_detect_test.ps1 -json -name planes-mbasic -inputfile ".\test\data\BASIC\PLANES.bas" -expected ".\test\ref\$action\$language\PLANES.txt"
test\bin\run_detect_test.ps1 -json -name primes-mbasic -inputfile ".\test\data\BASIC\primes.bas" -expected ".\test\ref\$action\$language\primes.txt"
test\bin\run_detect_test.ps1 -json -name escape-mbasic -inputfile ".\test\data\BASIC\escape.bas" -expected ".\test\ref\$action\$language\escape.txt"

# BASICA
Set-Variable -Name language "BASICA"
test\bin\run_detect_test.ps1 -json -name sea-creature-basica -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\$action\$language\sea_creature.txt"
test\bin\run_detect_test.ps1 -json -name lanturn-basica -inputfile ".\test\data\BASIC\lanturn.bas" -expected ".\test\ref\$action\$language\lanturn.txt"
test\bin\run_detect_test.ps1 -json -name gw3d-basica -inputfile ".\test\data\BASIC\GW3D.bas" -expected ".\test\ref\$action\$language\GW3D.txt"
test\bin\run_detect_test.ps1 -json -name guotte-basica -inputfile ".\test\data\BASIC\guotte.bas" -expected ".\test\ref\$action\$language\guotte.txt"

# C-78
Set-Variable -Name language "C-78"
test\bin\run_detect_test.ps1 -json -name diamond-c-78 -inputfile ".\test\data\C-78\diamond.c" -expected ".\test\ref\$action\$language\diamond.txt"
test\bin\run_detect_test.ps1 -json -name prime_test-c-78 -inputfile ".\test\data\C-78\prime_test.c" -expected ".\test\ref\$action\$language\prime_test.txt"
test\bin\run_detect_test.ps1 -json -name values-c-78 -inputfile ".\test\data\C-78\values.c" -expected ".\test\ref\$action\$language\values.txt"
test\bin\run_detect_test.ps1 -json -name j_interpreter-c-78 -inputfile ".\test\data\C-78\j_interpreter.c" -expected ".\test\ref\$action\$language\j_interpreter.txt"
test\bin\run_detect_test.ps1 -json -name zero-line-c-78 -inputfile ".\test\data\C-78\zero_line_hello_world.c" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"
test\bin\run_detect_test.ps1 -json -name charlotte-c-78 -inputfile ".\test\data\C-78\charlotte.c" -expected ".\test\ref\$action\$language\charlotte.txt"
test\bin\run_detect_test.ps1 -json -name light_pen-c-78 -inputfile ".\test\data\$language\light_pen.c" -expected ".\test\ref\$action\$language\light_pen.txt"
test\bin\run_detect_test.ps1 -json -name cpaint-c-78 -inputfile ".\test\data\$language\cpaint.c" -expected ".\test\ref\$action\$language\cpaint_c.txt"
test\bin\run_detect_test.ps1 -json -name cpaint-h-78 -inputfile ".\test\data\$language\cpaint.h" -expected ".\test\ref\$action\$language\cpaint_h.txt"

# C-89

# C-99
Set-Variable -Name language "C-99"
test\bin\run_detect_test.ps1 -json -name c-decl-c-99 -inputfile ".\test\data\C-99\c-decl.c" -expected ".\test\ref\$action\$language\c-decl-c.txt"
test\bin\run_detect_test.ps1 -json -name parser-h-99 -inputfile ".\test\data\C-99\parser.h" -expected ".\test\ref\$action\$language\parser-h.txt"
test\bin\run_detect_test.ps1 -json -name parser-c-99 -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\$action\$language\parser-c.txt"
test\bin\run_detect_test.ps1 -json -name cleancode-c-99 -inputfile ".\test\data\$language\cleancode.c" -expected ".\test\ref\$action\$language\cleancode-c.txt"

# CBASIC
Set-Variable -Name language "CBASIC"
test\bin\run_detect_test.ps1 -json -name fibo -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\detect\CBASIC\fibo.txt"
test\bin\run_detect_test.ps1 -json -name demograf -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\detect\CBASIC\DEMOGRAF.txt"
test\bin\run_detect_test.ps1 -json -name graphr -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\detect\CBASIC\GRAPHR.txt"

# COBOL-68
Set-Variable -Name language "COBOL-68"
test\bin\run_detect_test.ps1 -json -name mccracken1-68 -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\detect\COBOL-68\mccracken1.txt"
test\bin\run_detect_test.ps1 -json -name mccracken2-68 -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\detect\COBOL-68\mccracken2.txt"
test\bin\run_detect_test.ps1 -json -name mccracken3-68 -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\detect\COBOL-68\mccracken3.txt"
test\bin\run_detect_test.ps1 -json -name prog1-68 -inputfile ".\test\data\COBOL-68\PROG1.COB" -expected ".\test\ref\detect\COBOL-68\PROG1.txt"
test\bin\run_detect_test.ps1 -json -name prog2-68 -inputfile ".\test\data\COBOL-68\PROG2.COB" -expected ".\test\ref\detect\COBOL-68\PROG2.txt"
test\bin\run_detect_test.ps1 -json -name prog2A-68 -inputfile ".\test\data\COBOL-68\PROG2A.COB" -expected ".\test\ref\detect\COBOL-68\PROG2A.txt"

# COBOL-74

# COBOL-85
Set-Variable -Name language "COBOL-85"
test\bin\run_detect_test.ps1 -json -name prog3-85 -inputfile ".\test\data\COBOL-85\PROG3.COB" -expected ".\test\ref\detect\COBOL-85\PROG3.txt"
test\bin\run_detect_test.ps1 -json -name prog4-85 -inputfile ".\test\data\COBOL-85\PROG4.COB" -expected ".\test\ref\detect\COBOL-85\PROG4.txt"
test\bin\run_detect_test.ps1 -json -name prog5-85 -inputfile ".\test\data\COBOL-85\PROG5.COB" -expected ".\test\ref\detect\COBOL-85\PROG5.txt"
test\bin\run_detect_test.ps1 -json -name prog6-85 -inputfile ".\test\data\COBOL-85\PROG6.COB" -expected ".\test\ref\detect\COBOL-85\PROG6.txt"
test\bin\run_detect_test.ps1 -json -name prog12-2-85 -inputfile ".\test\data\COBOL-85\PROG12-2.COB" -expected ".\test\ref\detect\COBOL-85\PROG12-2.txt"
test\bin\run_detect_test.ps1 -json -name prog13-3-85 -inputfile ".\test\data\COBOL-85\PROG13-3.COB" -expected ".\test\ref\detect\COBOL-85\PROG13-3.txt"
test\bin\run_detect_test.ps1 -json -name prog14-2-85 -inputfile ".\test\data\COBOL-85\PROG14-2.COB" -expected ".\test\ref\detect\COBOL-85\PROG14-2.txt"
test\bin\run_detect_test.ps1 -json -name prog15-4-85 -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\detect\COBOL-85\PROG15-4.txt"
test\bin\run_detect_test.ps1 -json -name exec1-85 -inputfile ".\test\data\COBOL-85\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\detect\COBOL-85\UNLDDBCU2.txt"
test\bin\run_detect_test.ps1 -json -name P010-85 -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\detect\COBOL-85\P010.txt"

# COBOL-2002
Set-Variable -Name language "COBOL-2002"
test\bin\run_detect_test.ps1 -json -name report-card-cob2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\detect\COBOL-2002\ReportCard.txt"
test\bin\run_detect_test.ps1 -json -name person-cob2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\detect\COBOL-2002\Person.txt"
test\bin\run_detect_test.ps1 -json -name report-cob2002 -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\detect\COBOL-2002\Report.txt"
test\bin\run_detect_test.ps1 -json -name advent-of-code-cob2002 -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\detect\COBOL-2002\AdventOfCode.txt"
test\bin\run_detect_test.ps1 -json -name P010-wide-2002 -inputfile ".\test\data\COBOL-2002\P010-wide.COB" -wide -expected ".\test\ref\detect\COBOL-2002\P010-wide.txt"

# COBOL-2014

# COBOL-2014 with AcuCobol extensions

# COBOL-2014 with IBM extensions

# COBOL-2014 with GNU extensions
Set-Variable -Name language "COBOL-2014-GNU"
test\bin\run_detect_test.ps1 -json -name sales-report-cob2014-GNU -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\detect\COBOL-2014-GNU\SalesReport.txt"

# C++
Set-Variable -Name language "Cplusplus"
test\bin\run_detect_test.ps1 -json -name checkers -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\detect\Cplusplus\checkers.txt"
test\bin\run_detect_test.ps1 -json -name hrdb -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\detect\Cplusplus\hrdb.txt"
test\bin\run_detect_test.ps1 -json -name date_h -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\detect\Cplusplus\date_h.txt"
test\bin\run_detect_test.ps1 -json -name date_cplusplus -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\detect\Cplusplus\date_cpp.txt"
test\bin\run_detect_test.ps1 -json -name inherit_cplusplus -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\detect\Cplusplus\inherit.txt"

# C#
Set-Variable -Name language "Csharp"
test\bin\run_detect_test.ps1 -json -name calculator1 -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\detect\Csharp\calculator1.txt"
test\bin\run_detect_test.ps1 -json -name calculator2 -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\detect\Csharp\calculator2.txt"
test\bin\run_detect_test.ps1 -json -name calculator3 -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\detect\Csharp\calculator3.txt"

# CoffeeScript
Set-Variable -Name language "CoffeeScript"
test\bin\run_detect_test.ps1 -json -name provider-coffee -inputfile ".\test\data\CoffeeScript\provider.coffee" -expected ".\test\ref\detect\CoffeeScript\provider.txt"
test\bin\run_detect_test.ps1 -json -name resque-coffee -inputfile ".\test\data\CoffeeScript\resque.coffee" -expected ".\test\ref\detect\CoffeeScript\resque.txt"
test\bin\run_detect_test.ps1 -json -name world-map-coffee -inputfile ".\test\data\CoffeeScript\world_map.coffee" -expected ".\test\ref\detect\CoffeeScript\world_map.txt"

# D
Set-Variable -Name language "D"
test\bin\run_detect_test.ps1 -json -name regex-d -inputfile ".\test\data\d\regex.d" -expected ".\test\ref\detect\d\regex.txt"
test\bin\run_detect_test.ps1 -json -name halffloat-d -inputfile ".\test\data\d\halffloat.d" -expected ".\test\ref\detect\d\halffloat.txt"
test\bin\run_detect_test.ps1 -json -name wc-d -inputfile ".\test\data\d\wc.d" -expected ".\test\ref\detect\d\wc.txt"

# Dart
Set-Variable -Name language "Dart"
test\bin\run_detect_test.ps1 -json -name anagram-dart -inputfile ".\test\data\Dart\anagram.dart" -expected ".\test\ref\detect\dart\anagram.txt"
test\bin\run_detect_test.ps1 -json -name note_client-dart -inputfile ".\test\data\Dart\note_client.dart" -expected ".\test\ref\detect\dart\note_client.txt"
test\bin\run_detect_test.ps1 -json -name web_app-dart -inputfile ".\test\data\Dart\web_app.dart" -expected ".\test\ref\detect\dart\web_app.txt"

# dbase II
Set-Variable -Name language "dbase-ii"
test\bin\run_detect_test.ps1 -json -name sample-dbii -inputfile ".\test\data\dbase-II\sample.prg" -expected ".\test\ref\detect\dbase-II\sample.txt"
test\bin\run_detect_test.ps1 -json -name addm-dbii -inputfile ".\test\data\dbase-II\ADDM.PRG" -expected ".\test\ref\detect\dbase-II\ADDM.txt"
test\bin\run_detect_test.ps1 -json -name changedm-dbii -inputfile ".\test\data\dbase-II\CHANGEDM.PRG" -expected ".\test\ref\detect\dbase-II\CHANGEDM.txt"
test\bin\run_detect_test.ps1 -json -name dater-dbii -inputfile ".\test\data\dbase-II\DATER.PRG" -expected ".\test\ref\detect\dbase-II\DATER.txt"
test\bin\run_detect_test.ps1 -json -name updatedm-dbii -inputfile ".\test\data\dbase-II\UPDATEDM.PRG" -expected ".\test\ref\detect\dbase-II\UPDATEDM.txt"
test\bin\run_detect_test.ps1 -json -name viewdm-dbii -inputfile ".\test\data\dbase-II\VIEWDM.PRG" -expected ".\test\ref\detect\dbase-II\VIEWDM.txt"
test\bin\run_detect_test.ps1 -json -name emain-dbii -inputfile ".\test\data\dbase-II\EMAIN.PRG" -expected ".\test\ref\detect\dbase-II\EMAIN.txt"
test\bin\run_detect_test.ps1 -json -name emp-entr-dbii -inputfile ".\test\data\dbase-II\EMP-ENTR.PRG" -expected ".\test\ref\detect\dbase-II\EMP-ENTR.txt"
test\bin\run_detect_test.ps1 -json -name emp-rprt-dbii -inputfile ".\test\data\dbase-II\EMP-RPRT.PRG" -expected ".\test\ref\detect\dbase-II\EMP-RPRT.txt"
test\bin\run_detect_test.ps1 -json -name emp-term-dbii -inputfile ".\test\data\dbase-II\EMP-TERM.PRG" -expected ".\test\ref\detect\dbase-II\EMP-TERM.txt"
test\bin\run_detect_test.ps1 -json -name emp-upd-dbii -inputfile ".\test\data\dbase-II\EMP-UPD.PRG" -expected ".\test\ref\detect\dbase-II\EMP-UPD.txt"
test\bin\run_detect_test.ps1 -json -name invmaint-dbii -inputfile ".\test\data\dbase-II\INVMAINT.PRG" -expected ".\test\ref\detect\dbase-II\INVMAINT.txt"
test\bin\run_detect_test.ps1 -json -name invquan-dbii -inputfile ".\test\data\dbase-II\INVQUAN.PRG" -expected ".\test\ref\detect\dbase-II\INVQUAN.txt"
test\bin\run_detect_test.ps1 -json -name invread-dbii -inputfile ".\test\data\dbase-II\INVREAD.PRG" -expected ".\test\ref\detect\dbase-II\INVREAD.txt"
test\bin\run_detect_test.ps1 -json -name invrprt-dbii -inputfile ".\test\data\dbase-II\INVRPRT.PRG" -expected ".\test\ref\detect\dbase-II\INVRPRT.txt"

# dBase III
Set-Variable -Name language "dbase-III"
test\bin\run_detect_test.ps1 -json -name fdate-dbiii -inputfile ".\test\data\dbase-III\FDATE.PRG" -expected ".\test\ref\detect\dbase-III\FDATE.txt"
test\bin\run_detect_test.ps1 -json -name library-dbiii -inputfile ".\test\data\dbase-III\LIBRARY.PRG" -expected ".\test\ref\detect\dbase-III\LIBRARY.txt"
test\bin\run_detect_test.ps1 -json -name dp_sort-dbiii -inputfile ".\test\data\dbase-III\DP_SORT.PRG" -expected ".\test\ref\detect\dbase-III\DP_SORT.txt"

# Delphi
Set-Variable -Name language "Delphi"
test\bin\run_detect_test.ps1 -json -name FmMain-dfm-delphi -inputfile ".\test\data\delphi\FmMain.dfm" -expected ".\test\ref\detect\Delphi\FmMain-dfm.txt"
test\bin\run_detect_test.ps1 -json -name FmMain-pas-delphi -inputfile ".\test\data\delphi\FmMain.pas" -expected ".\test\ref\detect\Delphi\FmMain-pas.txt"
test\bin\run_detect_test.ps1 -json -name UCalc-delphi -inputfile ".\test\data\delphi\UCalc.pas" -expected ".\test\ref\detect\Delphi\UCalc.txt"
test\bin\run_detect_test.ps1 -json -name UChessBoardCmp-delphi -inputfile ".\test\data\delphi\UChessBoardCmp.pas" -expected ".\test\ref\detect\Delphi\UChessBoardCmp.txt"
test\bin\run_detect_test.ps1 -json -name UPlatform-delphi -inputfile ".\test\data\delphi\UPlatform.pas" -expected ".\test\ref\detect\Delphi\UPlatform.txt"

# Dibol
Set-Variable -Name language "Dibol"
Set-Variable -Name extension "dbl"
test\bin\run_detect_test.ps1 -json -name bottles-$language -inputfile ".\test\data\Dibol\bottles.$extension" -expected ".\test\ref\$action\$language\bottles.txt"
test\bin\run_detect_test.ps1 -json -name bilref-$language -inputfile ".\test\data\Dibol\bilref.$extension" -expected ".\test\ref\$action\$language\bilref.txt"

# Eiffel
Set-Variable -Name language "Eiffel"
test\bin\run_detect_test.ps1 -json -name hello-eiffel -inputfile ".\test\data\Eiffel\hello.e" -expected ".\test\ref\detect\Eiffel\hello.txt"
test\bin\run_detect_test.ps1 -json -name bakery-eiffel -inputfile ".\test\data\Eiffel\bakery.e" -expected ".\test\ref\detect\Eiffel\bakery.txt"
test\bin\run_detect_test.ps1 -json -name reverse-eiffel -inputfile ".\test\data\Eiffel\reverse.e" -expected ".\test\ref\detect\Eiffel\reverse.txt"
test\bin\run_detect_test.ps1 -json -name bottles-eiffel -inputfile ".\test\data\Eiffel\bottles_of_beer.e" -expected ".\test\ref\detect\Eiffel\bottles_of_beer.txt"

# Erlang
Set-Variable -Name language "Erlang"
test\bin\run_detect_test.ps1 -json -name armstrong-erlang -inputfile ".\test\data\Erlang\armstrong.erl" -expected ".\test\ref\detect\Erlang\armstrong.txt"
test\bin\run_detect_test.ps1 -json -name list_comprehension-erlang -inputfile ".\test\data\Erlang\list_comprehension.erl" -expected ".\test\ref\detect\Erlang\list_comprehension.txt"
test\bin\run_detect_test.ps1 -json -name send_receive-erlang Erlang -inputfile ".\test\data\Erlang\send_receive.erl" -expected ".\test\ref\detect\Erlang\send_receive.txt"

# Flowmatic
Set-Variable -Name language "Flowmatic"
test\bin\run_detect_test.ps1 -json -name example-flowmatic -inputfile ".\test\data\Flowmatic\example.txt" -expected ".\test\ref\detect\Flowmatic\example.txt"

# FORTRAN-66
Set-Variable -Name language "FORTRAN-66"
test\bin\run_detect_test.ps1 -json -name hello-ftn66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\detect\FORTRAN-66\HELLO.txt"
test\bin\run_detect_test.ps1 -json -name heron-ftn66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\detect\FORTRAN-66\HERON.txt"
test\bin\run_detect_test.ps1 -json -name heron2-ftn66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\detect\FORTRAN-66\HERON2.txt"
test\bin\run_detect_test.ps1 -json -name heron-wide-ftn66 -inputfile ".\test\data\FORTRAN-66\HERON-wide.FOR" -wide -expected ".\test\ref\detect\FORTRAN-66\HERON-wide.txt"

# FORTRAN-77
Set-Variable -Name language "FORTRAN-77"
test\bin\run_detect_test.ps1 -json -name hello-ftn77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\detect\FORTRAN-77\HELLO.txt"
test\bin\run_detect_test.ps1 -json -name complex-ftn77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\detect\FORTRAN-77\COMPLEX.txt"
test\bin\run_detect_test.ps1 -json -name euclid-ftn77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\detect\FORTRAN-77\EUCLID.txt"
test\bin\run_detect_test.ps1 -json -name heron-ftn77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\detect\FORTRAN-77\HERON.txt"

# Fortran-90
Set-Variable -Name language "Fortran-90"
test\bin\run_detect_test.ps1 -json -name cylinder-ftn90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\detect\Fortran-90\cylinder.txt"
test\bin\run_detect_test.ps1 -json -name gauss-ftn90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\detect\Fortran-90\gauss.txt"
test\bin\run_detect_test.ps1 -json -name hello-ftn90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\detect\Fortran-90\hello.txt"
test\bin\run_detect_test.ps1 -json -name temp-sub-ftn90 -inputfile ".\test\data\Fortran-90\temp_sub.for" -expected ".\test\ref\detect\Fortran-90\temp_sub.txt"
test\bin\run_detect_test.ps1 -json -name temp-func-ftn90 -inputfile ".\test\data\Fortran-90\temp_func.for" -expected ".\test\ref\detect\Fortran-90\temp_func.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex2-ftn90 -inputfile ".\test\data\Fortran-90\Wikibooks-ex2.f95" -expected ".\test\ref\detect\Fortran-90\Wikibooks-ex2.txt"

# Fortran-95
Set-Variable -Name language "Fortran-95"
test\bin\run_detect_test.ps1 -json -name ibm-ex1-ftn95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\detect\Fortran-95\ibm-ex1.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex1-ftn95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\detect\Fortran-95\Wikibooks-ex1.txt"

# Fortran-2003
Set-Variable -Name language "Fortran-2003"
test\bin\run_detect_test.ps1 -json -name average-ftn2003 -inputfile ".\test\data\Fortran-2003\average.f90" -expected ".\test\ref\detect\Fortran-2003\average.txt"
test\bin\run_detect_test.ps1 -json -name geo4060-ftn2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\detect\Fortran-2003\geo4060.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex3-ftn2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex3.f95" -expected ".\test\ref\detect\Fortran-2003\Wikibooks-ex3.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex4-ftn2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex4.f95" -expected ".\test\ref\detect\Fortran-2003\Wikibooks-ex4.txt"

# Fortran-2008

# F#
Set-Variable -Name language "Fsharp"
test\bin\run_detect_test.ps1 -json -name samples-fs -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\detect\Fsharp\samples.txt"
test\bin\run_detect_test.ps1 -json -name antwalk-fs -inputfile ".\test\data\Fsharp\ant_walk.fs" -expected ".\test\ref\detect\Fsharp\ant_walk.txt"

# Go
Set-Variable -Name language "Go"
test\bin\run_detect_test.ps1 -json -name find-cli-go -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\detect\Go\find-cli.txt"

# Groovy
Set-Variable -Name language "Groovy"
test\bin\run_detect_test.ps1 -json -name koan05-groovy -inputfile ".\test\data\Groovy\koan05.groovy" -expected ".\test\ref\detect\Groovy\koan05.txt"
test\bin\run_detect_test.ps1 -json -name koan13-groovy -inputfile ".\test\data\Groovy\koan13.groovy" -expected ".\test\ref\detect\Groovy\koan13.txt"

# Haskell
Set-Variable -Name language "Haskell"
test\bin\run_detect_test.ps1 -json -name calendar-hs -inputfile ".\test\data\Haskell\calendar.hs" -expected ".\test\ref\detect\Haskell\calendar.txt"
test\bin\run_detect_test.ps1 -json -name todo-hs -inputfile ".\test\data\Haskell\todo.hs" -expected ".\test\ref\detect\Haskell\todo.txt"
test\bin\run_detect_test.ps1 -json -name rna-hs -inputfile ".\test\data\Haskell\todo.hs" -expected ".\test\ref\detect\Haskell\rna.txt"

# HTML
Set-Variable -Name language "HTML"
test\bin\run_detect_test.ps1 -json -name knuth-html -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\detect\HTML\knuth.txt"
test\bin\run_detect_test.ps1 -json -name developer-html -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\detect\HTML\developer-css.txt"
test\bin\run_detect_test.ps1 -json -name codestat-html -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\detect\HTML\codestat-css-javascript.txt"

# Intercal
Set-Variable -Name language "Intercal"
test\bin\run_detect_test.ps1 -json -name fizzbuzz-ical -inputfile ".\test\data\Intercal\fizzbuzz.intercal" -expected ".\test\ref\detect\Intercal\fizzbuzz.txt"

# Java
Set-Variable -Name language "Java"
test\bin\run_detect_test.ps1 -json -name prime_test-java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\detect\Java\prime_test.txt"
test\bin\run_detect_test.ps1 -json -name palindrome-java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\detect\Java\palindrome.txt"
test\bin\run_detect_test.ps1 -json -name binary-search-java -inputfile ".\test\data\Java\binary_search.java" -expected ".\test\ref\detect\Java\binary_search.txt"
test\bin\run_detect_test.ps1 -json -name ObjectServer-java -inputfile ".\test\data\Java\ObjectServer.java" -expected ".\test\ref\detect\Java\ObjectServer.txt"

# JavaScript
Set-Variable -Name language "JavaScript"
test\bin\run_detect_test.ps1 -json -name values-js -inputfile ".\test\data\JavaScript\values.js" -expected ".\test\ref\detect\JavaScript\values.txt"
test\bin\run_detect_test.ps1 -json -name codestat-js -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\detect\JavaScript\codestat.txt"
test\bin\run_detect_test.ps1 -json -name bing-js -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\detect\JavaScript\bing.txt"
test\bin\run_detect_test.ps1 -json -name calc_prime-js -inputfile ".\test\data\JavaScript\calc_prime.js" -expected ".\test\ref\detect\JavaScript\calc_prime.txt"
test\bin\run_detect_test.ps1 -json -name backtick-js -inputfile ".\test\data\JavaScript\backtick.js" -expected ".\test\ref\detect\JavaScript\backtick.txt"

# Julia
Set-Variable -Name language "Julia"
test\bin\run_detect_test.ps1 -json -name dsp-jl -inputfile ".\test\data\Julia\dsp.jl" -expected ".\test\ref\detect\Julia\dsp.txt"
test\bin\run_detect_test.ps1 -json -name container-jl -inputfile ".\test\data\Julia\container.jl" -expected ".\test\ref\detect\Julia\container.txt"
test\bin\run_detect_test.ps1 -json -name microbiome-jl -inputfile ".\test\data\Julia\microbiome.jl" -expected ".\test\ref\detect\Julia\microbiome.txt"
test\bin\run_detect_test.ps1 -json -name periodograms-jl -inputfile ".\test\data\Julia\periodograms.jl" -expected ".\test\ref\detect\Julia\periodograms.txt"

# Kotlin
Set-Variable -Name language "Kotlin"
test\bin\run_detect_test.ps1 -json -name qksms-kt -inputfile ".\test\data\Kotlin\qksms.kt" -expected ".\test\ref\detect\Kotlin\qksms.txt"
test\bin\run_detect_test.ps1 -json -name render-kt -inputfile ".\test\data\Kotlin\render.kt" -expected ".\test\ref\detect\Kotlin\render.txt"

# Latino
Set-Variable -Name language "Latino"
test\bin\run_detect_test.ps1 -json -name major-lat -inputfile ".\test\data\$language\major.lat" -expected ".\test\ref\$action\$language\major.txt"
test\bin\run_detect_test.ps1 -json -name fibonacci-lat -inputfile ".\test\data\$language\fibonacci.lat" -expected ".\test\ref\$action\$language\fibonacci.txt"

# Lua
Set-Variable -Name language "Lua"
test\bin\run_detect_test.ps1 -json -name dissector-lua -inputfile ".\test\data\Lua\dissector.lua" -expected ".\test\ref\detect\Lua\dissector.txt"
test\bin\run_detect_test.ps1 -json -name dissector2-lua -inputfile ".\test\data\Lua\dissector2.lua" -expected ".\test\ref\detect\Lua\dissector2.txt"
test\bin\run_detect_test.ps1 -json -name dsl-lua -inputfile ".\test\data\Lua\dsl.lua" -expected ".\test\ref\detect\Lua\dsl.txt"
test\bin\run_detect_test.ps1 -json -name markov-lua -inputfile ".\test\data\Lua\markov.lua" -expected ".\test\ref\detect\Lua\markov.txt"

# Matlab
$language= "Matlab"
test\bin\run_detect_test.ps1 -json -name transpose-matlab -inputfile ".\test\data\Matlab\transpose.m" -expected ".\test\ref\detect\Matlab\transpose.txt"
test\bin\run_detect_test.ps1 -json -name choose-matlab -inputfile ".\test\data\Matlab\choose.m" -expected ".\test\ref\detect\Matlab\choose.txt"
test\bin\run_detect_test.ps1 -json -name fitnormal-matlab -inputfile ".\test\data\Matlab\fitnormal.m" -expected ".\test\ref\detect\Matlab\fitnormal.txt"
test\bin\run_detect_test.ps1 -json -name test_fitnormal-matlab -inputfile ".\test\data\Matlab\test_fitnormal.m" -expected ".\test\ref\detect\Matlab\test_fitnormal.txt"
test\bin\run_detect_test.ps1 -json -name tshow-matlab -inputfile ".\test\data\Matlab\tshow.m" -expected ".\test\ref\detect\Matlab\tshow.txt"

# Modula-2
Set-Variable -Name language "Modula-2"
test\bin\run_detect_test.ps1 -json -name C64ToIBM-mod2 -inputfile ".\test\data\Modula-2\C64ToIBM.mod" -expected ".\test\ref\detect\Modula-2\C64ToIBM.txt"
test\bin\run_detect_test.ps1 -json -name CaseDemo-mod2 -inputfile ".\test\data\Modula-2\CaseDemo.mod" -expected ".\test\ref\detect\Modula-2\CaseDemo.txt"
test\bin\run_detect_test.ps1 -json -name game_def-mod2 -inputfile ".\test\data\Modula-2\game.def" -expected ".\test\ref\detect\Modula-2\game_def.txt"
test\bin\run_detect_test.ps1 -json -name game-mod2 -inputfile ".\test\data\Modula-2\game.mod" -expected ".\test\ref\detect\Modula-2\game.txt"
test\bin\run_detect_test.ps1 -json -name LoopDemo-mod2 -inputfile ".\test\data\Modula-2\LoopDemo.mod" -expected ".\test\ref\detect\Modula-2\LoopDemo.txt"
test\bin\run_detect_test.ps1 -json -name PigLatin-mod2 -inputfile ".\test\data\Modula-2\PigLatin.mod" -expected ".\test\ref\detect\Modula-2\PigLatin.txt"
test\bin\run_detect_test.ps1 -json -name TempConv-mod2 -inputfile ".\test\data\Modula-2\TempConv.mod" -expected ".\test\ref\detect\Modula-2\TempConv.txt"

# Objective-C
Set-Variable -Name language "Objective-C"
test\bin\run_detect_test.ps1 -json -name hello-objc -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\detect\Objective-C\HelloWorld.txt"
test\bin\run_detect_test.ps1 -json -name qrmath-objc -inputfile ".\test\data\Objective-C\QRMath.h" -expected ".\test\ref\detect\Objective-C\QRMath.txt"
test\bin\run_detect_test.ps1 -json -name qrencoder-objc -inputfile ".\test\data\Objective-C\QREncoder.m" -expected ".\test\ref\detect\Objective-C\QREncoder.txt"
test\bin\run_detect_test.ps1 -json -name jsonkit_h-objc -inputfile ".\test\data\Objective-C\JSONKit.h" -expected ".\test\ref\detect\Objective-C\JSONKit_h.txt"
test\bin\run_detect_test.ps1 -json -name jsonkit_m-objc -inputfile ".\test\data\Objective-C\JSONKit.m" -expected ".\test\ref\detect\Objective-C\JSONKit_m.txt"

# OCaml
Set-Variable -Name language "OCaml"
test\bin\run_detect_test.ps1 -json -name antwalk-ocaml -inputfile ".\test\data\OCaml\ant_walk.ml" -expected ".\test\ref\detect\OCaml\ant_walk.txt"

# Octave
Set-Variable -Name language "Octave"
test\bin\run_detect_test.ps1 -json -name transpose-octave -inputfile ".\test\data\Octave\transpose.m" -expected ".\test\ref\detect\Octave\transpose.txt"
test\bin\run_detect_test.ps1 -json -name choose-octave -inputfile ".\test\data\Octave\choose.m" -expected ".\test\ref\detect\Octave\choose.txt"
test\bin\run_detect_test.ps1 -json -name fitnormal-octave -inputfile ".\test\data\Octave\fitnormal.m" -expected ".\test\ref\detect\Octave\fitnormal.txt"
test\bin\run_detect_test.ps1 -json -name test_fitnormal-octave -inputfile ".\test\data\Octave\test_fitnormal.m" -expected ".\test\ref\detect\Octave\test_fitnormal.txt"
test\bin\run_detect_test.ps1 -json -name tshow-octave -inputfile ".\test\data\Octave\tshow.m" -expected ".\test\ref\detect\Octave\tshow.txt"
test\bin\run_detect_test.ps1 -json -name ex7_pca-octave -inputfile ".\test\data\Octave\ex7_pca.m" -expected ".\test\ref\detect\Octave\ex7_pca.txt"

# Pascal
Set-Variable -Name language "Pascal"
test\bin\run_detect_test.ps1 -json -name label_declare-pas -inputfile ".\test\data\Pascal\LabelDeclaration.pas" -expected ".\test\ref\detect\Pascal\LabelDeclaration.txt"
test\bin\run_detect_test.ps1 -json -name firework-pas -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect\Pascal\FIREWORK.txt"
test\bin\run_detect_test.ps1 -json -name hello-pas -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\detect\Pascal\HELLO.txt"
test\bin\run_detect_test.ps1 -json -name rose-pas -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\detect\Pascal\ROSE.txt"
test\bin\run_detect_test.ps1 -json -name spider-pas -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\detect\Pascal\SPIDER.txt"
test\bin\run_detect_test.ps1 -json -name tpc16-pas -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\detect\Pascal\TPC16.txt"

# Perl
Set-Variable -Name language "Perl"
test\bin\run_detect_test.ps1 -json -name perligata-perl -inputfile ".\test\data\Perl\Perligata.pm" -expected ".\test\ref\$action\$language\Perligata.txt"
test\bin\run_detect_test.ps1 -json -name physics-perl -inputfile ".\test\data\Perl\Physics.pm" -expected ".\test\ref\$action\$language\Physics.txt"
test\bin\run_detect_test.ps1 -json -name root_distance-perl -inputfile ".\test\data\$language\root_distance.pl" -expected ".\test\ref\$action\$language\root_distance.txt"

# PL/1
Set-Variable -Name language "PL1"
Set-Variable -Name extension "pl1"
test\bin\run_detect_test.ps1 -json -name bpgpli-$extension -inputfile ".\test\data\$language\BPGPLI.$extension" -expected ".\test\ref\$action\$language\BPGPLI.txt"
test\bin\run_detect_test.ps1 -json -name checkdt-$extension -inputfile ".\test\data\$language\CHECKDT.$extension" -expected ".\test\ref\$action\$language\CHECKDT.txt"
test\bin\run_detect_test.ps1 -json -name crtpln3-$extension -inputfile ".\test\data\$language\CRTPLN3.$extension" -expected ".\test\ref\$action\$language\CRTPLN3.txt"
test\bin\run_detect_test.ps1 -json -name mainfact-$extension -inputfile ".\test\data\$language\MAINFACT.$extension" -expected ".\test\ref\$action\$language\MAINFACT.txt"
test\bin\run_detect_test.ps1 -json -name example-$extension -inputfile ".\test\data\$language\example.$extension" -expected ".\test\ref\$action\$language\example.txt"
test\bin\run_detect_test.ps1 -json -name digrams-$extension -inputfile ".\test\data\$language\digrams.$extension" -expected ".\test\ref\$action\$language\digrams.txt"
test\bin\run_detect_test.ps1 -json -name ystkp-$extension -inputfile ".\test\data\$language\ystkp.$extension" -expected ".\test\ref\$action\$language\ystkp.txt"

# PL/M
Set-Variable -Name language "PLM"
test\bin\run_detect_test.ps1 -json -name example-plm -inputfile ".\test\data\$language\example.plm" -expected ".\test\ref\$action\$language\example.txt"

# Prolog
Set-Variable -Name language "Prolog"
test\bin\run_detect_test.ps1 -json -name family-main -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\detect\Prolog\family-main.txt"
test\bin\run_detect_test.ps1 -json -name family-kb -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\detect\Prolog\family-kb.txt"
test\bin\run_detect_test.ps1 -json -name family-menu -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\detect\Prolog\family-menu.txt"
test\bin\run_detect_test.ps1 -json -name family-queries -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\detect\Prolog\family-queries.txt"
test\bin\run_detect_test.ps1 -json -name web-server-hello-prolog -inputfile ".\test\data\Prolog\web-server-hello.pl" -expected ".\test\ref\detect\Prolog\web-server-hello.txt"
test\bin\run_detect_test.ps1 -json -name web-server-params-prolog -inputfile ".\test\data\Prolog\web-server-params.pl" -expected ".\test\ref\detect\Prolog\web-server-params.txt"

# Python
Set-Variable -Name language "Python"
test\bin\run_detect_test.ps1 -json -name drone-3d -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\detect\Python\drone_3d_trajectory_following.txt"
test\bin\run_detect_test.ps1 -json -name quadrotor -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\detect\Python\Quadrotor.txt"
test\bin\run_detect_test.ps1 -json -name trajectory -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\detect\Python\TrajectoryGenerator.txt"
test\bin\run_detect_test.ps1 -json -name values-python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\detect\Python\values.txt"
test\bin\run_detect_test.ps1 -json -name examiner -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\detect\Python\Examiner.txt"
test\bin\run_detect_test.ps1 -json -name authorized-view-python -inputfile ".\test\data\Python\authorized_view.py" -expected ".\test\ref\detect\Python\authorized_view.txt"

# R
Set-Variable -Name language "R"
test\bin\run_detect_test.ps1 -json -name ETM-540-01 -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\detect\R\ETM-540-01.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-02 -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\detect\R\ETM-540-02.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-03 -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\detect\R\ETM-540-03.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-04 -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\detect\R\ETM-540-04.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-05 -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\detect\R\ETM-540-05.txt"
test\bin\run_detect_test.ps1 -json -name basketball-r -inputfile ".\test\data\R\basketball.R" -expected ".\test\ref\detect\R\basketball.txt"
test\bin\run_detect_test.ps1 -json -name render-r -inputfile ".\test\data\R\render.R" -expected ".\test\ref\detect\R\render.txt"

# Ruby
Set-Variable -Name language "Ruby"
test\bin\run_detect_test.ps1 -json -name basic-ruby -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\detect\Ruby\basic.txt"
test\bin\run_detect_test.ps1 -json -name constants-ruby -inputfile ".\test\data\Ruby\constants.rb" -expected ".\test\ref\detect\Ruby\constants.txt"
test\bin\run_detect_test.ps1 -json -name exceptions-ruby -inputfile ".\test\data\Ruby\exceptions.rb" -expected ".\test\ref\detect\Ruby\exceptions.txt"
test\bin\run_detect_test.ps1 -json -name expressions-ruby -inputfile ".\test\data\Ruby\expressions.rb" -expected ".\test\ref\detect\Ruby\expressions.txt"
test\bin\run_detect_test.ps1 -json -name functions-ruby -inputfile ".\test\data\Ruby\functions.rb" -expected ".\test\ref\detect\Ruby\functions.txt"
test\bin\run_detect_test.ps1 -json -name io-ruby -inputfile ".\test\data\Ruby\io.rb" -expected ".\test\ref\detect\Ruby\io.txt"
test\bin\run_detect_test.ps1 -json -name modifiers-ruby -inputfile ".\test\data\Ruby\modifiers.rb" -expected ".\test\ref\detect\Ruby\modifiers.txt"
test\bin\run_detect_test.ps1 -json -name operators-ruby -inputfile ".\test\data\Ruby\operators.rb" -expected ".\test\ref\detect\Ruby\operators.txt"
test\bin\run_detect_test.ps1 -json -name statements-ruby -inputfile ".\test\data\Ruby\statements.rb" -expected ".\test\ref\detect\Ruby\statements.txt"
test\bin\run_detect_test.ps1 -json -name tokenbuilders-ruby -inputfile ".\test\data\Ruby\tokenbuilders.rb" -expected ".\test\ref\detect\Ruby\tokenbuilders.txt"
test\bin\run_detect_test.ps1 -json -name tokenizers-ruby -inputfile ".\test\data\Ruby\tokenizers.rb" -expected ".\test\ref\detect\Ruby\tokenizers.txt"
test\bin\run_detect_test.ps1 -json -name tokens-ruby -inputfile ".\test\data\Ruby\tokens.rb" -expected ".\test\ref\detect\Ruby\tokens.txt"
test\bin\run_detect_test.ps1 -json -name webhook-ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\detect\Ruby\webhook.txt"

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
test\bin\run_detect_test.ps1 -json -name AppDelegate -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\detect\Swift\AppDelegate.txt"
test\bin\run_detect_test.ps1 -json -name Meal -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\detect\Swift\Meal.txt"
test\bin\run_detect_test.ps1 -json -name MealTableViewCell -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\detect\Swift\MealTableViewCell.txt"
test\bin\run_detect_test.ps1 -json -name MealTableViewController -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\detect\Swift\MealTableViewController.txt"
test\bin\run_detect_test.ps1 -json -name MealViewController -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\detect\Swift\MealViewController.txt"
test\bin\run_detect_test.ps1 -json -name RatingControl -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\detect\Swift\RatingControl.txt"
test\bin\run_detect_test.ps1 -json -name URLExtensions -inputfile ".\test\data\Swift\URLExtensions.swift" -expected ".\test\ref\detect\Swift\URLExtensions.txt"

# TypeScript
Set-Variable -Name language "TypeScript"
test\bin\run_detect_test.ps1 -json -name TimeReporter-ts -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\detect\TypeScript\TimeReporter.txt"
test\bin\run_detect_test.ps1 -json -name ImageBoard-ts -inputfile ".\test\data\TypeScript\ImageBoard.ts" -expected ".\test\ref\detect\TypeScript\ImageBoard.txt"

# Visual Basic 6
Set-Variable -Name language "VisualBasic-6"
test\bin\run_detect_test.ps1 -json -name spider-vb6 -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\detect\VisualBasic-6\spider.txt"
test\bin\run_detect_test.ps1 -json -name azure_metadata-vb6 -inputfile ".\test\data\VisualBasic-6\azure_metadata.bas" -expected ".\test\ref\detect\VisualBasic-6\azure_metadata.txt"
test\bin\run_detect_test.ps1 -json -name diffie_hellman-vb6 -inputfile ".\test\data\VisualBasic-6\diffie_hellman.bas" -expected ".\test\ref\detect\VisualBasic-6\diffie_hellman.txt"

# Visual Basic .NET
Set-Variable -Name language "VisualBasic-NET"
test\bin\run_detect_test.ps1 -json -name word-processor-vbnet -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\detect\VisualBasic-NET\WordProcessor.txt"

# polyglot programs
test\bin\run_detect_test.ps1 -json -name polyglot -notiebreak -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect\multiple\polyglot.txt"
test\bin\run_detect_test.ps1 -json -name polyglot-languages -notiebreak -language C-78,Cplusplus,COBOL-68,COBOL-74,cobol-85,objective-c,Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect\multiple\polyglot-languages.txt"
test\bin\run_detect_test.ps1 -json -name polyglot-py-rb -notiebreak -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\detect\multiple\polyglot-py-rb.txt"
test\bin\run_detect_test.ps1 -json -name polyglot-py-rb-languages -notiebreak -language python,ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\detect\multiple\polyglot-py-rb-languages.txt"

# non-code files
test\bin\run_detect_test.ps1 -json -name noncode-empty -inputfile ".\test\data\noncode\empty.txt" -expected ".\test\ref\detect\noncode\empty.txt"
test\bin\run_detect_test.ps1 -json -name noncode-whitespace -inputfile ".\test\data\noncode\whitespace.txt" -expected ".\test\ref\detect\noncode\whitespace.txt"
test\bin\run_detect_test.ps1 -json -name noncode-win-ini -inputfile ".\test\data\noncode\win.ini" -expected ".\test\ref\detect\noncode\win.txt"
test\bin\run_detect_test.ps1 -json -name noncode-setutc -inputfile ".\test\data\noncode\setutc.reg" -expected ".\test\ref\detect\noncode\setutc.txt"
test\bin\run_detect_test.ps1 -json -name noncode-diagwrn -inputfile ".\test\data\noncode\diagwrn.xml" -expected ".\test\ref\detect\noncode\diagwrn.txt"
test\bin\run_detect_test.ps1 -json -name noncode-profile-xlsx -inputfile ".\test\data\noncode\profile.xlsx" -expected ".\test\ref\detect\noncode\profile.txt"
test\bin\run_detect_test.ps1 -json -name noncode-resume-docx -inputfile ".\test\data\noncode\resume.docx" -expected ".\test\ref\detect\noncode\resume.txt"
test\bin\run_detect_test.ps1 -json -name noncode-hh -inputfile ".\test\data\noncode\hh.exe" -expected ".\test\ref\detect\noncode\hh.txt"
