Set-StrictMode -Version Latest

Set-Variable -Name action "detect"

# generic assembly

# ASM IBM-360
Set-Variable -Name language "ASM-360"
test\bin\run_detect_test.ps1 -json -name subroutine1-$language -inputfile ".\test\data\ASM-360\subroutine1.asm" -expected ".\test\ref\$action\$language\subroutine1.asm.txt"
test\bin\run_detect_test.ps1 -json -name subroutine1_a-$language -inputfile ".\test\data\ASM-360\subroutine1_a.asm" -expected ".\test\ref\$action\$language\subroutine1_a.asm.txt"
test\bin\run_detect_test.ps1 -json -name subroutine2-$language -inputfile ".\test\data\ASM-360\subroutine2.asm" -expected ".\test\ref\$action\$language\subroutine2.asm.txt"
test\bin\run_detect_test.ps1 -json -name subroutine2_a-$language -inputfile ".\test\data\ASM-360\subroutine2_a.asm" -expected ".\test\ref\$action\$language\subroutine2_a.asm.txt"
test\bin\run_detect_test.ps1 -json -name spitbol-$language -inputfile ".\test\data\ASM-360\spitbol.asm" -expected ".\test\ref\$action\$language\spitbol.asm.txt"

# ASM IBM-370
Set-Variable -Name language "ASM-370"
test\bin\run_detect_test.ps1 -json -name asm370a1-$language -inputfile ".\test\data\ASM-370\asm370a1.asm" -expected ".\test\ref\$action\$language\asm370a1.asm.txt"

# ASM IBM-390
Set-Variable -Name language "ASM-390"
test\bin\run_detect_test.ps1 -json -name osint-$language -inputfile ".\test\data\ASM-390\osint.asm" -expected ".\test\ref\$action\$language\osint.asm.txt"

# ASM IBM-system-z

# ASM 1802
Set-Variable -Name language "ASM-1802"
test\bin\run_detect_test.ps1 -json -name test_alu-$language -inputfile ".\test\data\ASM-1802\test_alu.asm" -expected ".\test\ref\$action\$language\test_alu.asm.txt"

# ASM 6502
Set-Variable -Name language "ASM-6502"
test\bin\run_detect_test.ps1 -json -name sweet16-$language -inputfile ".\test\data\ASM-6502\sweet16.asm" -expected ".\test\ref\$action\$language\sweet16.asm.txt"

# ASM 6800
Set-Variable -Name language "ASM-6800"
test\bin\run_detect_test.ps1 -json -name ET3400-S6-$language -inputfile ".\test\data\ASM-6800\ET3400-S6.asm" -expected ".\test\ref\$action\$language\ET3400-S6.asm.txt"

# ASM 68000
Set-Variable -Name language "ASM-68000"
test\bin\run_detect_test.ps1 -json -name basic68k-$language -inputfile ".\test\data\ASM-68000\basic68k.asm" -expected ".\test\ref\$action\$language\basic68k.asm.txt"
test\bin\run_detect_test.ps1 -json -name sprites-$language -inputfile ".\test\data\ASM-68000\sprites.asm" -expected ".\test\ref\$action\$language\sprites.asm.txt"

# ASM 8080
Set-Variable -Name language "ASM-8080"
test\bin\run_detect_test.ps1 -json -name i8080core-$language -inputfile ".\test\data\ASM-8080\i8080core.asm" -expected ".\test\ref\$action\$language\i8080core.asm.txt"
test\bin\run_detect_test.ps1 -json -name microcosm-$language -inputfile ".\test\data\ASM-8080\microcosm.asm" -expected ".\test\ref\$action\$language\microcosm.asm.txt"
test\bin\run_detect_test.ps1 -json -name hello_free-$language -inputfile ".\test\data\ASM-8080\hello_free.asm" -expected ".\test\ref\$action\$language\hello_free.asm.txt"
test\bin\run_detect_test.ps1 -json -name hello_spaced-$language -inputfile ".\test\data\ASM-8080\hello_spaced.asm" -expected ".\test\ref\$action\$language\hello_spaced.asm.txt"

# ASM Z-80
Set-Variable -Name language "ASM-Z-80"
test\bin\run_detect_test.ps1 -json -name intro-$language -inputfile ".\test\data\ASM-Z-80\intro.asm" -expected ".\test\ref\$action\$language\intro.asm.txt"
test\bin\run_detect_test.ps1 -json -name hardmess-$language -inputfile ".\test\data\ASM-Z-80\hardmess.asm" -expected ".\test\ref\$action\$language\hardmess.asm.txt"
test\bin\run_detect_test.ps1 -json -name shftspr-$language -inputfile ".\test\data\ASM-Z-80\shftspr.asm" -expected ".\test\ref\$action\$language\shftspr.asm.txt"
test\bin\run_detect_test.ps1 -json -name sincos-$language -inputfile ".\test\data\ASM-Z-80\sincos.asm" -expected ".\test\ref\$action\$language\sincos.asm.txt"

# ASM 8086
Set-Variable -Name language "ASM-8086"
test\bin\run_detect_test.ps1 -json -name BISTRS-$language -inputfile ".\test\data\ASM-8086\BISTRS.asm" -expected ".\test\ref\$action\$language\BISTRS.asm.txt"
test\bin\run_detect_test.ps1 -json -name DSKCOM-$language -inputfile ".\test\data\ASM-8086\DSKCOM.asm" -expected ".\test\ref\$action\$language\DSKCOM.asm.txt"
test\bin\run_detect_test.ps1 -json -name FIVEO-$language -inputfile ".\test\data\ASM-8086\FIVEO.asm" -expected ".\test\ref\$action\$language\FIVEO.asm.txt"
test\bin\run_detect_test.ps1 -json -name GWEVAL-$language -inputfile ".\test\data\ASM-8086\GWEVAL.asm" -expected ".\test\ref\$action\$language\GWEVAL.asm.txt"
test\bin\run_detect_test.ps1 -json -name NEXT86-$language -inputfile ".\test\data\ASM-8086\NEXT86.asm" -expected ".\test\ref\$action\$language\NEXT86.asm.txt"
test\bin\run_detect_test.ps1 -json -name io_indic-$language -inputfile ".\test\data\ASM-8086\io_indic.asm" -expected ".\test\ref\$action\$language\io_indic.asm.txt"

# ASM 80386
Set-Variable -Name language "ASM-80386"
test\bin\run_detect_test.ps1 -json -name reverser-$language -inputfile ".\test\data\ASM-80386\reverser.asm" -expected ".\test\ref\$action\$language\reverser.asm.txt"
test\bin\run_detect_test.ps1 -json -name crc16-$language -inputfile ".\test\data\ASM-80386\crc16.asm" -expected ".\test\ref\$action\$language\crc16.asm.txt"
test\bin\run_detect_test.ps1 -json -name mat_get_element-$language -inputfile ".\test\data\ASM-80386\mat_get_element.asm" -expected ".\test\ref\$action\$language\mat_get_element.asm.txt"

# ASM 80486

# ASM PDP-8
Set-Variable -Name language "ASM-PDP-8"
test\bin\run_detect_test.ps1 -json -name io-$language -inputfile ".\test\data\ASM-PDP-8\io.asm" -expected ".\test\ref\$action\$language\io.asm.txt"
test\bin\run_detect_test.ps1 -json -name life-$language -inputfile ".\test\data\ASM-PDP-8\life.asm" -expected ".\test\ref\$action\$language\life.asm.txt"
test\bin\run_detect_test.ps1 -json -name sum-$language -inputfile ".\test\data\ASM-PDP-8\sum.asm" -expected ".\test\ref\$action\$language\sum.asm.txt"

# ASM PDP-11
Set-Variable -Name language "ASM-PDP-11"
test\bin\run_detect_test.ps1 -json -name print-$language -inputfile ".\test\data\ASM-PDP-11\print.asm" -expected ".\test\ref\$action\$language\print.asm.txt"
test\bin\run_detect_test.ps1 -json -name k11tsx-$language -inputfile ".\test\data\ASM-PDP-11\k11tsx.mac" -expected ".\test\ref\$action\$language\k11tsx.asm.txt"
test\bin\run_detect_test.ps1 -json -name krtser-$language -inputfile ".\test\data\ASM-PDP-11\krtser.mac" -expected ".\test\ref\$action\$language\krtser.asm.txt"
test\bin\run_detect_test.ps1 -json -name krtsub-$language -inputfile ".\test\data\ASM-PDP-11\krtsub.mac" -expected ".\test\ref\$action\$language\krtsub.asm.txt"

# Ada 83
Set-Variable -Name language "Ada-83"
test\bin\run_detect_test.ps1 -json -name adabkend-adb -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\$action\$language\adabkend.adb.txt"
test\bin\run_detect_test.ps1 -json -name adabkend-ads -inputfile ".\test\data\Ada-83\adabkend.ads" -expected ".\test\ref\$action\$language\adabkend.ads.txt"
test\bin\run_detect_test.ps1 -json -name signup-adb -inputfile ".\test\data\Ada-83\signup.adb" -expected ".\test\ref\$action\$language\signup.adb.txt"
test\bin\run_detect_test.ps1 -json -name naive-sort-ada-83 -inputfile ".\test\data\Ada-83\naive-sort.ada" -expected ".\test\ref\$action\$language\naive-sort.ada.txt"
test\bin\run_detect_test.ps1 -json -name naive-sort-0-ada-83 -inputfile ".\test\data\Ada-83\naive-sort-0.ada" -expected ".\test\ref\$action\$language\naive-sort-0.ada.txt"

# Ada 95
Set-Variable -Name language "Ada-95"
test\bin\run_detect_test.ps1 -json -name philosophers-ads -inputfile ".\test\data\Ada-95\philosophers.ads" -expected ".\test\ref\$action\$language\philosophers.ads.txt"

# Awk
Set-Variable -Name language "Awk"
test\bin\run_detect_test.ps1 -json -name funstack-$language -inputfile ".\test\data\Awk\funstack.awk" -expected ".\test\ref\$action\$language\funstack.awk.txt"
test\bin\run_detect_test.ps1 -json -name awkaster-$language -inputfile ".\test\data\Awk\awkaster.awk" -expected ".\test\ref\$action\$language\awkaster.awk.txt"

# BASIC
Set-Variable -Name language "BASIC"
test\bin\run_detect_test.ps1 -json -name values-$language -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\$action\$language\values.bas.txt"
test\bin\run_detect_test.ps1 -json -name simple-$language -inputfile ".\test\data\BASIC\simple.bas" -expected ".\test\ref\$action\$language\simple.bas.txt"
test\bin\run_detect_test.ps1 -json -name 3dplot-$language -inputfile ".\test\data\BASIC\3dplot.bas" -expected ".\test\ref\$action\$language\3dplot.bas.txt"
test\bin\run_detect_test.ps1 -json -name batnum-$language -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\$action\$language\batnum.bas.txt"
test\bin\run_detect_test.ps1 -json -name life-$language -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\$action\$language\life.bas.txt"
test\bin\run_detect_test.ps1 -json -name income-$language -inputfile ".\test\data\BASIC\income.bas" -expected ".\test\ref\$action\$language\income.bas.txt"
test\bin\run_detect_test.ps1 -json -name rockt2-$language -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\$action\$language\rockt2.bas.txt"

# BASIC-80
Set-Variable -Name language "BASIC-80"
test\bin\run_detect_test.ps1 -json -name doctor-$language -inputfile ".\test\data\BASIC\doctor.bas" -expected ".\test\ref\$action\$language\doctor.bas.txt"
test\bin\run_detect_test.ps1 -json -name airinput-$language -inputfile ".\test\data\BASIC\AIRINPUT.bas" -expected ".\test\ref\$action\$language\AIRINPUT.bas.txt"
test\bin\run_detect_test.ps1 -json -name backgamm-$language -inputfile ".\test\data\BASIC\BACKGAMM.bas" -expected ".\test\ref\$action\$language\BACKGAMM.bas.txt"
test\bin\run_detect_test.ps1 -json -name planes-$language -inputfile ".\test\data\BASIC\PLANES.bas" -expected ".\test\ref\$action\$language\PLANES.bas.txt"
test\bin\run_detect_test.ps1 -json -name primes-$language -inputfile ".\test\data\BASIC\primes.bas" -expected ".\test\ref\$action\$language\primes.bas.txt"
test\bin\run_detect_test.ps1 -json -name escape-$language -inputfile ".\test\data\BASIC\escape.bas" -expected ".\test\ref\$action\$language\escape.bas.txt"

# BASICA
Set-Variable -Name language "BASICA"
test\bin\run_detect_test.ps1 -json -name sea-creature-$language -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\$action\$language\sea_creature.bas.txt"
test\bin\run_detect_test.ps1 -json -name lanturn-$language -inputfile ".\test\data\BASIC\lanturn.bas" -expected ".\test\ref\$action\$language\lanturn.bas.txt"
test\bin\run_detect_test.ps1 -json -name gw3d-$language -inputfile ".\test\data\BASIC\GW3D.bas" -expected ".\test\ref\$action\$language\GW3D.bas.txt"
test\bin\run_detect_test.ps1 -json -name guotte-$language -inputfile ".\test\data\BASIC\guotte.bas" -expected ".\test\ref\$action\$language\guotte.bas.txt"

# C-78
Set-Variable -Name language "C-78"
test\bin\run_detect_test.ps1 -json -name diamond-$language -inputfile ".\test\data\C-78\diamond.c" -expected ".\test\ref\$action\$language\diamond.c.txt"
test\bin\run_detect_test.ps1 -json -name prime_test-$language -inputfile ".\test\data\C-78\prime_test.c" -expected ".\test\ref\$action\$language\prime_test.c.txt"
test\bin\run_detect_test.ps1 -json -name values-$language -inputfile ".\test\data\C-78\values.c" -expected ".\test\ref\$action\$language\values.c.txt"
test\bin\run_detect_test.ps1 -json -name j_interpreter-$language -inputfile ".\test\data\C-78\j_interpreter.c" -expected ".\test\ref\$action\$language\j_interpreter.c.txt"
test\bin\run_detect_test.ps1 -json -name zero-line-$language -inputfile ".\test\data\C-78\zero_line_hello_world.c" -expected ".\test\ref\$action\$language\zero_line_hello_world.c.txt"
test\bin\run_detect_test.ps1 -json -name charlotte-$language -inputfile ".\test\data\C-78\charlotte.c" -expected ".\test\ref\$action\$language\charlotte.c.txt"
test\bin\run_detect_test.ps1 -json -name light_pen-$language -inputfile ".\test\data\$language\light_pen.c" -expected ".\test\ref\$action\$language\light_pen.c.txt"
test\bin\run_detect_test.ps1 -json -name cpaint-$language -inputfile ".\test\data\$language\cpaint.c" -expected ".\test\ref\$action\$language\cpaint.c.txt"
test\bin\run_detect_test.ps1 -json -name cpaint-h-78 -inputfile ".\test\data\$language\cpaint.h" -expected ".\test\ref\$action\$language\cpaint.h.txt"

# C-89

# C-99
Set-Variable -Name language "C-99"
test\bin\run_detect_test.ps1 -json -name c-decl-$language -inputfile ".\test\data\C-99\c-decl.c" -expected ".\test\ref\$action\$language\c-decl.c.txt"
test\bin\run_detect_test.ps1 -json -name parser-h-99 -inputfile ".\test\data\C-99\parser.h" -expected ".\test\ref\$action\$language\parser.h.txt"
test\bin\run_detect_test.ps1 -json -name parser-$language -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\$action\$language\parser.c.txt"
test\bin\run_detect_test.ps1 -json -name cleancode-$language -inputfile ".\test\data\$language\cleancode.c" -expected ".\test\ref\$action\$language\cleancode.c.txt"

# CBASIC
Set-Variable -Name language "CBASIC"
test\bin\run_detect_test.ps1 -json -name fibo-$language -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\$action\$language\fibo.bas.txt"
test\bin\run_detect_test.ps1 -json -name demograf-$language -inputfile ".\test\data\CBASIC\DEMOGRAF.BAS" -expected ".\test\ref\$action\$language\DEMOGRAF.bas.txt"
test\bin\run_detect_test.ps1 -json -name graphr-$language -inputfile ".\test\data\CBASIC\GRAPHR.BAS" -expected ".\test\ref\$action\$language\GRAPHR.bas.txt"

# COBOL-68
Set-Variable -Name language "COBOL-68"
test\bin\run_detect_test.ps1 -json -name mccracken1-$language -inputfile ".\test\data\COBOL-68\mccracken1.cob" -expected ".\test\ref\$action\$language\mccracken1.cob.txt"
test\bin\run_detect_test.ps1 -json -name mccracken2-$language -inputfile ".\test\data\COBOL-68\mccracken2.cob" -expected ".\test\ref\$action\$language\mccracken2.cob.txt"
test\bin\run_detect_test.ps1 -json -name mccracken3-$language -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\$action\$language\mccracken3.cob.txt"
test\bin\run_detect_test.ps1 -json -name prog1-$language -inputfile ".\test\data\COBOL-68\PROG1.COB" -expected ".\test\ref\$action\$language\PROG1.COB.txt"
test\bin\run_detect_test.ps1 -json -name prog2-$language -inputfile ".\test\data\COBOL-68\PROG2.COB" -expected ".\test\ref\$action\$language\PROG2.COB.txt"
test\bin\run_detect_test.ps1 -json -name prog2A-$language -inputfile ".\test\data\COBOL-68\PROG2A.COB" -expected ".\test\ref\$action\$language\PROG2A.COB.txt"

# COBOL-74

# COBOL-85
Set-Variable -Name language "COBOL-85"
test\bin\run_detect_test.ps1 -json -name prog3-$language -inputfile ".\test\data\COBOL-85\PROG3.COB" -expected ".\test\ref\$action\$language\PROG3.COB.txt"
test\bin\run_detect_test.ps1 -json -name prog4-$language -inputfile ".\test\data\COBOL-85\PROG4.COB" -expected ".\test\ref\$action\$language\PROG4.COB.txt"
test\bin\run_detect_test.ps1 -json -name prog5-$language -inputfile ".\test\data\COBOL-85\PROG5.COB" -expected ".\test\ref\$action\$language\PROG5.COB.txt"
test\bin\run_detect_test.ps1 -json -name prog6-$language -inputfile ".\test\data\COBOL-85\PROG6.COB" -expected ".\test\ref\$action\$language\PROG6.COB.txt"
test\bin\run_detect_test.ps1 -json -name prog12-2-$language -inputfile ".\test\data\COBOL-85\PROG12-2.COB" -expected ".\test\ref\$action\$language\PROG12-2.COB.txt"
test\bin\run_detect_test.ps1 -json -name prog13-3-$language -inputfile ".\test\data\COBOL-85\PROG13-3.COB" -expected ".\test\ref\$action\$language\PROG13-3.COB.txt"
test\bin\run_detect_test.ps1 -json -name prog14-2-$language -inputfile ".\test\data\COBOL-85\PROG14-2.COB" -expected ".\test\ref\$action\$language\PROG14-2.COB.txt"
test\bin\run_detect_test.ps1 -json -name prog15-4-$language -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\$action\$language\PROG15-4.COB.txt"
test\bin\run_detect_test.ps1 -json -name exec1-$language -inputfile ".\test\data\COBOL-85\UNLDDBCU2.COB" -tabsize 4 -expected ".\test\ref\$action\$language\UNLDDBCU2.COB.txt"
test\bin\run_detect_test.ps1 -json -name P010-$language -inputfile ".\test\data\COBOL-85\P010.cob" -expected ".\test\ref\$action\$language\P010.cob.txt"

# COBOL-2002
Set-Variable -Name language "COBOL-2002"
test\bin\run_detect_test.ps1 -json -name report-card-$language -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\$action\$language\ReportCard.cob.txt"
test\bin\run_detect_test.ps1 -json -name person-$language -tabsize 4 -inputfile ".\test\data\COBOL-2002\Person.cob" -expected ".\test\ref\$action\$language\Person.cob.txt"
test\bin\run_detect_test.ps1 -json -name report-$language -tabsize 4 -inputfile ".\test\data\COBOL-2002\Report.cob" -expected ".\test\ref\$action\$language\Report.cob.txt"
test\bin\run_detect_test.ps1 -json -name advent-of-code-$language -inputfile ".\test\data\COBOL-2002\AdventOfCode.cob" -expected ".\test\ref\$action\$language\AdventOfCode.cob.txt"
test\bin\run_detect_test.ps1 -json -name P010-wide-2002 -inputfile ".\test\data\COBOL-2002\P010-wide.COB" -expected ".\test\ref\$action\$language\P010-wide.COB.txt"

# COBOL-2014

# COBOL-2014 with AcuCobol extensions

# COBOL-2014 with IBM extensions

# COBOL-2014 with GNU extensions
Set-Variable -Name language "COBOL-2014-GNU"
test\bin\run_detect_test.ps1 -json -name sales-report-$language -tabsize 4 -inputfile ".\test\data\COBOL-2014-GNU\SalesReport.cob" -expected ".\test\ref\$action\$language\SalesReport.cob.txt"

# C++
Set-Variable -Name language "Cplusplus"
test\bin\run_detect_test.ps1 -json -name checkers-$language -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\$action\$language\checkers.cpp.txt"
test\bin\run_detect_test.ps1 -json -name hrdb-$language -inputfile ".\test\data\Cplusplus\hrdb.cpp" -expected ".\test\ref\$action\$language\hrdb.cpp.txt"
test\bin\run_detect_test.ps1 -json -name date_h-$language -inputfile ".\test\data\Cplusplus\date.h" -expected ".\test\ref\$action\$language\date.h.txt"
test\bin\run_detect_test.ps1 -json -name date-$language -inputfile ".\test\data\Cplusplus\date.cpp" -expected ".\test\ref\$action\$language\date.cpp.txt"
test\bin\run_detect_test.ps1 -json -name inherit-$language -inputfile ".\test\data\Cplusplus\inherit.cpp" -expected ".\test\ref\$action\$language\inherit.cpp.txt"

# C#
Set-Variable -Name language "Csharp"
test\bin\run_detect_test.ps1 -json -name calculator1-$language -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\$action\$language\calculator1.cs.txt"
test\bin\run_detect_test.ps1 -json -name calculator2-$language -inputfile ".\test\data\Csharp\calculator2.cs" -expected ".\test\ref\$action\$language\calculator2.cs.txt"
test\bin\run_detect_test.ps1 -json -name calculator3-$language -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\$action\$language\calculator3.cs.txt"

# CoffeeScript
Set-Variable -Name language "CoffeeScript"
test\bin\run_detect_test.ps1 -json -name provider-$language -inputfile ".\test\data\CoffeeScript\provider.coffee" -expected ".\test\ref\$action\$language\provider.coffee.txt"
test\bin\run_detect_test.ps1 -json -name resque-$language -inputfile ".\test\data\CoffeeScript\resque.coffee" -expected ".\test\ref\$action\$language\resque.coffee.txt"
test\bin\run_detect_test.ps1 -json -name world-map-$language -inputfile ".\test\data\CoffeeScript\world_map.coffee" -expected ".\test\ref\$action\$language\world_map.coffee.txt"

# D
Set-Variable -Name language "D"
test\bin\run_detect_test.ps1 -json -name regex-$language -inputfile ".\test\data\d\regex.d" -expected ".\test\ref\$action\$language\regex.d.txt"
test\bin\run_detect_test.ps1 -json -name halffloat-$language -inputfile ".\test\data\d\halffloat.d" -expected ".\test\ref\$action\$language\halffloat.d.txt"
test\bin\run_detect_test.ps1 -json -name wc-$language -inputfile ".\test\data\d\wc.d" -expected ".\test\ref\$action\$language\wc.d.txt"

# Dart
Set-Variable -Name language "Dart"
test\bin\run_detect_test.ps1 -json -name anagram-$language -inputfile ".\test\data\Dart\anagram.dart" -expected ".\test\ref\$action\$language\anagram.dart.txt"
test\bin\run_detect_test.ps1 -json -name note_client-$language -inputfile ".\test\data\Dart\note_client.dart" -expected ".\test\ref\$action\$language\note_client.dart.txt"
test\bin\run_detect_test.ps1 -json -name web_app-$language -inputfile ".\test\data\Dart\web_app.dart" -expected ".\test\ref\$action\$language\web_app.dart.txt"

# dbase II
Set-Variable -Name language "dbase-ii"
test\bin\run_detect_test.ps1 -json -name sample-$language -inputfile ".\test\data\dbase-II\sample.prg" -expected ".\test\ref\$action\$language\sample.prg.txt"
test\bin\run_detect_test.ps1 -json -name addm-$language -inputfile ".\test\data\dbase-II\ADDM.PRG" -expected ".\test\ref\$action\$language\ADDM.PRG.txt"
test\bin\run_detect_test.ps1 -json -name changedm-$language -inputfile ".\test\data\dbase-II\CHANGEDM.PRG" -expected ".\test\ref\$action\$language\CHANGEDM.PRG.txt"
test\bin\run_detect_test.ps1 -json -name dater-$language -inputfile ".\test\data\dbase-II\DATER.PRG" -expected ".\test\ref\$action\$language\DATER.PRG.txt"
test\bin\run_detect_test.ps1 -json -name updatedm-$language -inputfile ".\test\data\dbase-II\UPDATEDM.PRG" -expected ".\test\ref\$action\$language\UPDATEDM.PRG.txt"
test\bin\run_detect_test.ps1 -json -name viewdm-$language -inputfile ".\test\data\dbase-II\VIEWDM.PRG" -expected ".\test\ref\$action\$language\VIEWDM.PRG.txt"
test\bin\run_detect_test.ps1 -json -name emain-$language -inputfile ".\test\data\dbase-II\EMAIN.PRG" -expected ".\test\ref\$action\$language\EMAIN.PRG.txt"
test\bin\run_detect_test.ps1 -json -name emp-entr-$language -inputfile ".\test\data\dbase-II\EMP-ENTR.PRG" -expected ".\test\ref\$action\$language\EMP-ENTR.PRG.txt"
test\bin\run_detect_test.ps1 -json -name emp-rprt-$language -inputfile ".\test\data\dbase-II\EMP-RPRT.PRG" -expected ".\test\ref\$action\$language\EMP-RPRT.PRG.txt"
test\bin\run_detect_test.ps1 -json -name emp-term-$language -inputfile ".\test\data\dbase-II\EMP-TERM.PRG" -expected ".\test\ref\$action\$language\EMP-TERM.PRG.txt"
test\bin\run_detect_test.ps1 -json -name emp-upd-$language -inputfile ".\test\data\dbase-II\EMP-UPD.PRG" -expected ".\test\ref\$action\$language\EMP-UPD.PRG.txt"
test\bin\run_detect_test.ps1 -json -name invmaint-$language -inputfile ".\test\data\dbase-II\INVMAINT.PRG" -expected ".\test\ref\$action\$language\INVMAINT.PRG.txt"
test\bin\run_detect_test.ps1 -json -name invquan-$language -inputfile ".\test\data\dbase-II\INVQUAN.PRG" -expected ".\test\ref\$action\$language\INVQUAN.PRG.txt"
test\bin\run_detect_test.ps1 -json -name invread-$language -inputfile ".\test\data\dbase-II\INVREAD.PRG" -expected ".\test\ref\$action\$language\INVREAD.PRG.txt"
test\bin\run_detect_test.ps1 -json -name invrprt-$language -inputfile ".\test\data\dbase-II\INVRPRT.PRG" -expected ".\test\ref\$action\$language\INVRPRT.PRG.txt"

# dBase III
Set-Variable -Name language "dbase-III"
test\bin\run_detect_test.ps1 -json -name fdate-$language -inputfile ".\test\data\dbase-III\FDATE.PRG" -expected ".\test\ref\$action\$language\FDATE.PRG.txt"
test\bin\run_detect_test.ps1 -json -name library-$language -inputfile ".\test\data\dbase-III\LIBRARY.PRG" -expected ".\test\ref\$action\$language\LIBRARY.PRG.txt"
test\bin\run_detect_test.ps1 -json -name dp_sort-$language -inputfile ".\test\data\dbase-III\DP_SORT.PRG" -expected ".\test\ref\$action\$language\DP_SORT.PRG.txt"

# Delphi
Set-Variable -Name language "Delphi"
test\bin\run_detect_test.ps1 -json -name FmMain-dfm-$language -inputfile ".\test\data\delphi\FmMain.dfm" -expected ".\test\ref\$action\$language\FmMain.dfm.txt"
test\bin\run_detect_test.ps1 -json -name FmMain-$language -inputfile ".\test\data\delphi\FmMain.pas" -expected ".\test\ref\$action\$language\FmMain.pas.txt"
test\bin\run_detect_test.ps1 -json -name UCalc-$language -inputfile ".\test\data\delphi\UCalc.pas" -expected ".\test\ref\$action\$language\UCalc.pas.txt"
test\bin\run_detect_test.ps1 -json -name UChessBoardCmp-$language -inputfile ".\test\data\delphi\UChessBoardCmp.pas" -expected ".\test\ref\$action\$language\UChessBoardCmp.pas.txt"
test\bin\run_detect_test.ps1 -json -name UPlatform-$language -inputfile ".\test\data\delphi\UPlatform.pas" -expected ".\test\ref\$action\$language\UPlatform.pas.txt"

# Dibol
Set-Variable -Name language "Dibol"
test\bin\run_detect_test.ps1 -json -name bottles-$language -inputfile ".\test\data\Dibol\bottles.dbl" -expected ".\test\ref\$action\$language\bottles.dbl.txt"
test\bin\run_detect_test.ps1 -json -name bilref-$language -inputfile ".\test\data\Dibol\bilref.dbl" -expected ".\test\ref\$action\$language\bilref.dbl.txt"

# Eiffel
Set-Variable -Name language "Eiffel"
test\bin\run_detect_test.ps1 -json -name hello-eiffel -inputfile ".\test\data\Eiffel\hello.e" -expected ".\test\ref\$action\$language\hello.e.txt"
test\bin\run_detect_test.ps1 -json -name bakery-eiffel -inputfile ".\test\data\Eiffel\bakery.e" -expected ".\test\ref\$action\$language\bakery.e.txt"
test\bin\run_detect_test.ps1 -json -name reverse-eiffel -inputfile ".\test\data\Eiffel\reverse.e" -expected ".\test\ref\$action\$language\reverse.e.txt"
test\bin\run_detect_test.ps1 -json -name bottles-eiffel -inputfile ".\test\data\Eiffel\bottles_of_beer.e" -expected ".\test\ref\$action\$language\bottles_of_beer.e.txt"

# Erlang
Set-Variable -Name language "Erlang"
test\bin\run_detect_test.ps1 -json -name armstrong-erlang -inputfile ".\test\data\Erlang\armstrong.erl" -expected ".\test\ref\$action\$language\armstrong.erl.txt"
test\bin\run_detect_test.ps1 -json -name list_comprehension-erlang -inputfile ".\test\data\Erlang\list_comprehension.erl" -expected ".\test\ref\$action\$language\list_comprehension.erl.txt"
test\bin\run_detect_test.ps1 -json -name send_receive-erlang Erlang -inputfile ".\test\data\Erlang\send_receive.erl" -expected ".\test\ref\$action\$language\send_receive.erl.txt"

# Flowmatic
Set-Variable -Name language "Flowmatic"
test\bin\run_detect_test.ps1 -json -name example-flowmatic -inputfile ".\test\data\Flowmatic\example.txt" -expected ".\test\ref\$action\$language\example.txt.txt"

# FORTRAN-66
Set-Variable -Name language "FORTRAN-66"
test\bin\run_detect_test.ps1 -json -name hello-ftn66 -inputfile ".\test\data\FORTRAN-66\HELLO.FOR" -expected ".\test\ref\$action\$language\HELLO.FOR.txt"
test\bin\run_detect_test.ps1 -json -name heron-ftn66 -inputfile ".\test\data\FORTRAN-66\HERON.FOR" -expected ".\test\ref\$action\$language\HERON.FOR.txt"
test\bin\run_detect_test.ps1 -json -name heron2-ftn66 -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\$action\$language\HERON2.FOR.txt"
test\bin\run_detect_test.ps1 -json -name heron-wide-ftn66 -inputfile ".\test\data\FORTRAN-66\HERON-wide.FOR" -expected ".\test\ref\$action\$language\HERON-wide.FOR.txt"

# FORTRAN-77
Set-Variable -Name language "FORTRAN-77"
test\bin\run_detect_test.ps1 -json -name hello-ftn77 -inputfile ".\test\data\FORTRAN-77\HELLO.F77" -expected ".\test\ref\$action\$language\HELLO.FOR.txt"
test\bin\run_detect_test.ps1 -json -name complex-ftn77 -inputfile ".\test\data\FORTRAN-77\COMPLEX.F77" -expected ".\test\ref\$action\$language\COMPLEX.FOR.txt"
test\bin\run_detect_test.ps1 -json -name euclid-ftn77 -inputfile ".\test\data\FORTRAN-77\EUCLID.F77" -expected ".\test\ref\$action\$language\EUCLID.FOR.txt"
test\bin\run_detect_test.ps1 -json -name heron-ftn77 -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\$action\$language\HERON.FOR.txt"

# Fortran-90
Set-Variable -Name language "Fortran-90"
test\bin\run_detect_test.ps1 -json -name cylinder-ftn90 -inputfile ".\test\data\Fortran-90\cylinder.f90" -expected ".\test\ref\$action\$language\cylinder.f90.txt"
test\bin\run_detect_test.ps1 -json -name gauss-ftn90 -inputfile ".\test\data\Fortran-90\gauss.f90" -expected ".\test\ref\$action\$language\gauss.f90.txt"
test\bin\run_detect_test.ps1 -json -name hello-ftn90 -inputfile ".\test\data\Fortran-90\hello.f90" -expected ".\test\ref\$action\$language\hello.f90.txt"
test\bin\run_detect_test.ps1 -json -name temp-sub-ftn90 -inputfile ".\test\data\Fortran-90\temp_sub.for" -expected ".\test\ref\$action\$language\temp_sub.for.txt"
test\bin\run_detect_test.ps1 -json -name temp-func-ftn90 -inputfile ".\test\data\Fortran-90\temp_func.for" -expected ".\test\ref\$action\$language\temp_func.for.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex2-ftn90 -inputfile ".\test\data\Fortran-90\Wikibooks-ex2.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex2.f95.txt"

# Fortran-95
Set-Variable -Name language "Fortran-95"
test\bin\run_detect_test.ps1 -json -name ibm-ex1-ftn95 -inputfile ".\test\data\Fortran-95\ibm-ex1.f95" -expected ".\test\ref\$action\$language\ibm-ex1.f95.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex1-ftn95 -inputfile ".\test\data\Fortran-95\Wikibooks-ex1.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex1.f95.txt"

# Fortran-2003
Set-Variable -Name language "Fortran-2003"
test\bin\run_detect_test.ps1 -json -name average-ftn2003 -inputfile ".\test\data\Fortran-2003\average.f90" -expected ".\test\ref\$action\$language\average.f90.txt"
test\bin\run_detect_test.ps1 -json -name geo4060-ftn2003 -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\$action\$language\geo4060.for.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex3-ftn2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex3.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex3.f95.txt"
test\bin\run_detect_test.ps1 -json -name Wikibooks-ex4-ftn2003 -inputfile ".\test\data\Fortran-2003\Wikibooks-ex4.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex4.f95.txt"

# Fortran-2008

# F#
Set-Variable -Name language "Fsharp"
test\bin\run_detect_test.ps1 -json -name samples-fs -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\$action\$language\samples.fs.txt"
test\bin\run_detect_test.ps1 -json -name antwalk-fs -inputfile ".\test\data\Fsharp\ant_walk.fs" -expected ".\test\ref\$action\$language\ant_walk.fs.txt"

# Go
Set-Variable -Name language "Go"
test\bin\run_detect_test.ps1 -json -name find-cli-go -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\$action\$language\find-cli.go.txt"

# Groovy
Set-Variable -Name language "Groovy"
test\bin\run_detect_test.ps1 -json -name koan05-groovy -inputfile ".\test\data\Groovy\koan05.groovy" -expected ".\test\ref\$action\$language\koan05.groovy.txt"
test\bin\run_detect_test.ps1 -json -name koan13-groovy -inputfile ".\test\data\Groovy\koan13.groovy" -expected ".\test\ref\$action\$language\koan13.groovy.txt"

# Haskell
Set-Variable -Name language "Haskell"
test\bin\run_detect_test.ps1 -json -name calendar-hs -inputfile ".\test\data\Haskell\calendar.hs" -expected ".\test\ref\$action\$language\calendar.hs.txt"
test\bin\run_detect_test.ps1 -json -name todo-hs -inputfile ".\test\data\Haskell\todo.hs" -expected ".\test\ref\$action\$language\todo.hs.txt"
test\bin\run_detect_test.ps1 -json -name rna-hs -inputfile ".\test\data\Haskell\todo.hs" -expected ".\test\ref\$action\$language\rna.hs.txt"

# HTML
Set-Variable -Name language "HTML"
test\bin\run_detect_test.ps1 -json -name knuth-html -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\$action\$language\knuth.html.txt"
test\bin\run_detect_test.ps1 -json -name developer-html -inputfile ".\test\data\HTML\developer-css.html" -expected ".\test\ref\$action\$language\developer-css.html.txt"
test\bin\run_detect_test.ps1 -json -name codestat-html -inputfile ".\test\data\HTML\codestat-css-javascript.html" -expected ".\test\ref\$action\$language\codestat-css-javascript.html.txt"

# Intercal
Set-Variable -Name language "Intercal"
test\bin\run_detect_test.ps1 -json -name fizzbuzz-ical -inputfile ".\test\data\Intercal\fizzbuzz.intercal" -expected ".\test\ref\$action\$language\fizzbuzz.intercal.txt"

# Java
Set-Variable -Name language "Java"
test\bin\run_detect_test.ps1 -json -name prime_test-java -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\$action\$language\prime_test.java.txt"
test\bin\run_detect_test.ps1 -json -name palindrome-java -inputfile ".\test\data\Java\palindrome.java" -expected ".\test\ref\$action\$language\palindrome.java.txt"
test\bin\run_detect_test.ps1 -json -name binary-search-java -inputfile ".\test\data\Java\binary_search.java" -expected ".\test\ref\$action\$language\binary_search.java.txt"
test\bin\run_detect_test.ps1 -json -name ObjectServer-java -inputfile ".\test\data\Java\ObjectServer.java" -expected ".\test\ref\$action\$language\ObjectServer.java.txt"

# JavaScript
Set-Variable -Name language "JavaScript"
test\bin\run_detect_test.ps1 -json -name values-js -inputfile ".\test\data\JavaScript\values.js" -expected ".\test\ref\$action\$language\values.js.txt"
test\bin\run_detect_test.ps1 -json -name codestat-js -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\$action\$language\codestat.js.txt"
test\bin\run_detect_test.ps1 -json -name bing-js -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\$action\$language\bing.js.txt"
test\bin\run_detect_test.ps1 -json -name calc_prime-js -inputfile ".\test\data\JavaScript\calc_prime.js" -expected ".\test\ref\$action\$language\calc_prime.js.txt"
test\bin\run_detect_test.ps1 -json -name backtick-js -inputfile ".\test\data\JavaScript\backtick.js" -expected ".\test\ref\$action\$language\backtick.js.txt"

# Julia
Set-Variable -Name language "Julia"
test\bin\run_detect_test.ps1 -json -name dsp-jl -inputfile ".\test\data\Julia\dsp.jl" -expected ".\test\ref\$action\$language\dsp.jl.txt"
test\bin\run_detect_test.ps1 -json -name container-jl -inputfile ".\test\data\Julia\container.jl" -expected ".\test\ref\$action\$language\container.jl.txt"
test\bin\run_detect_test.ps1 -json -name microbiome-jl -inputfile ".\test\data\Julia\microbiome.jl" -expected ".\test\ref\$action\$language\microbiome.jl.txt"
test\bin\run_detect_test.ps1 -json -name periodograms-jl -inputfile ".\test\data\Julia\periodograms.jl" -expected ".\test\ref\$action\$language\periodograms.jl.txt"

# Kotlin
Set-Variable -Name language "Kotlin"
test\bin\run_detect_test.ps1 -json -name qksms-kt -inputfile ".\test\data\Kotlin\qksms.kt" -expected ".\test\ref\$action\$language\qksms.kt.txt"
test\bin\run_detect_test.ps1 -json -name render-kt -inputfile ".\test\data\Kotlin\render.kt" -expected ".\test\ref\$action\$language\render.kt.txt"

# Latino
Set-Variable -Name language "Latino"
test\bin\run_detect_test.ps1 -json -name major-lat -inputfile ".\test\data\$language\major.lat" -expected ".\test\ref\$action\$language\major.lat.txt"
test\bin\run_detect_test.ps1 -json -name fibonacci-lat -inputfile ".\test\data\$language\fibonacci.lat" -expected ".\test\ref\$action\$language\fibonacci.lat.txt"

# Lua
Set-Variable -Name language "Lua"
test\bin\run_detect_test.ps1 -json -name dissector-lua -inputfile ".\test\data\Lua\dissector.lua" -expected ".\test\ref\$action\$language\dissector.lua.txt"
test\bin\run_detect_test.ps1 -json -name dissector2-lua -inputfile ".\test\data\Lua\dissector2.lua" -expected ".\test\ref\$action\$language\dissector2.lua.txt"
test\bin\run_detect_test.ps1 -json -name dsl-lua -inputfile ".\test\data\Lua\dsl.lua" -expected ".\test\ref\$action\$language\dsl.lua.txt"
test\bin\run_detect_test.ps1 -json -name markov-lua -inputfile ".\test\data\Lua\markov.lua" -expected ".\test\ref\$action\$language\markov.lua.txt"

# Matlab
$language= "Matlab"
test\bin\run_detect_test.ps1 -json -name transpose-matlab -inputfile ".\test\data\Matlab\transpose.m" -expected ".\test\ref\$action\$language\transpose.m.txt"
test\bin\run_detect_test.ps1 -json -name choose-matlab -inputfile ".\test\data\Matlab\choose.m" -expected ".\test\ref\$action\$language\choose.m.txt"
test\bin\run_detect_test.ps1 -json -name fitnormal-matlab -inputfile ".\test\data\Matlab\fitnormal.m" -expected ".\test\ref\$action\$language\fitnormal.m.txt"
test\bin\run_detect_test.ps1 -json -name test_fitnormal-matlab -inputfile ".\test\data\Matlab\test_fitnormal.m" -expected ".\test\ref\$action\$language\test_fitnormal.m.txt"
test\bin\run_detect_test.ps1 -json -name tshow-matlab -inputfile ".\test\data\Matlab\tshow.m" -expected ".\test\ref\$action\$language\tshow.m.txt"

# Modula-2
Set-Variable -Name language "Modula-2"
test\bin\run_detect_test.ps1 -json -name C64ToIBM-mod2 -inputfile ".\test\data\Modula-2\C64ToIBM.mod" -expected ".\test\ref\$action\$language\C64ToIBM.mod.txt"
test\bin\run_detect_test.ps1 -json -name CaseDemo-mod2 -inputfile ".\test\data\Modula-2\CaseDemo.mod" -expected ".\test\ref\$action\$language\CaseDemo.mod.txt"
test\bin\run_detect_test.ps1 -json -name game_def-mod2 -inputfile ".\test\data\Modula-2\game.def" -expected ".\test\ref\$action\$language\game_def.mod.txt"
test\bin\run_detect_test.ps1 -json -name game-mod2 -inputfile ".\test\data\Modula-2\game.mod" -expected ".\test\ref\$action\$language\game.mod.txt"
test\bin\run_detect_test.ps1 -json -name LoopDemo-mod2 -inputfile ".\test\data\Modula-2\LoopDemo.mod" -expected ".\test\ref\$action\$language\LoopDemo.mod.txt"
test\bin\run_detect_test.ps1 -json -name PigLatin-mod2 -inputfile ".\test\data\Modula-2\PigLatin.mod" -expected ".\test\ref\$action\$language\PigLatin.mod.txt"
test\bin\run_detect_test.ps1 -json -name TempConv-mod2 -inputfile ".\test\data\Modula-2\TempConv.mod" -expected ".\test\ref\$action\$language\TempConv.mod.txt"

# Objective-C
Set-Variable -Name language "Objective-C"
test\bin\run_detect_test.ps1 -json -name hello-objc -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\$action\$language\HelloWorld.objc.txt"
test\bin\run_detect_test.ps1 -json -name qrmath-objc -inputfile ".\test\data\Objective-C\QRMath.h" -expected ".\test\ref\$action\$language\QRMath.h.txt"
test\bin\run_detect_test.ps1 -json -name qrencoder-objc -inputfile ".\test\data\Objective-C\QREncoder.m" -expected ".\test\ref\$action\$language\QREncoder.m.txt"
test\bin\run_detect_test.ps1 -json -name jsonkit_h-objc -inputfile ".\test\data\Objective-C\JSONKit.h" -expected ".\test\ref\$action\$language\JSONKit.h.txt"
test\bin\run_detect_test.ps1 -json -name jsonkit_m-objc -inputfile ".\test\data\Objective-C\JSONKit.m" -expected ".\test\ref\$action\$language\JSONKit.m.txt"

# OCaml
Set-Variable -Name language "OCaml"
test\bin\run_detect_test.ps1 -json -name antwalk-ocaml -inputfile ".\test\data\OCaml\ant_walk.ml" -expected ".\test\ref\$action\$language\ant_walk.ml.txt"

# Octave
Set-Variable -Name language "Octave"
test\bin\run_detect_test.ps1 -json -name transpose-octave -inputfile ".\test\data\Octave\transpose.m" -expected ".\test\ref\$action\$language\transpose.m.txt"
test\bin\run_detect_test.ps1 -json -name choose-octave -inputfile ".\test\data\Octave\choose.m" -expected ".\test\ref\$action\$language\choose.m.txt"
test\bin\run_detect_test.ps1 -json -name fitnormal-octave -inputfile ".\test\data\Octave\fitnormal.m" -expected ".\test\ref\$action\$language\fitnormal.m.txt"
test\bin\run_detect_test.ps1 -json -name test_fitnormal-octave -inputfile ".\test\data\Octave\test_fitnormal.m" -expected ".\test\ref\$action\$language\test_fitnormal.m.txt"
test\bin\run_detect_test.ps1 -json -name tshow-octave -inputfile ".\test\data\Octave\tshow.m" -expected ".\test\ref\$action\$language\tshow.m.txt"
test\bin\run_detect_test.ps1 -json -name ex7_pca-octave -inputfile ".\test\data\Octave\ex7_pca.m" -expected ".\test\ref\$action\$language\ex7_pca.m.txt"

# Pascal
Set-Variable -Name language "Pascal"
test\bin\run_detect_test.ps1 -json -name label_declare-pas -inputfile ".\test\data\Pascal\LabelDeclaration.pas" -expected ".\test\ref\$action\$language\LabelDeclaration.pas.txt"
test\bin\run_detect_test.ps1 -json -name firework-pas -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\$action\$language\FIREWORK.PAS.txt"
test\bin\run_detect_test.ps1 -json -name hello-pas -inputfile ".\test\data\Pascal\HELLO.PAS" -expected ".\test\ref\$action\$language\HELLO.PAS.txt"
test\bin\run_detect_test.ps1 -json -name rose-pas -inputfile ".\test\data\Pascal\ROSE.PAS" -expected ".\test\ref\$action\$language\ROSE.PAS.txt"
test\bin\run_detect_test.ps1 -json -name spider-pas -inputfile ".\test\data\Pascal\SPIDER.PAS" -expected ".\test\ref\$action\v\SPIDER.PAS.txt"
test\bin\run_detect_test.ps1 -json -name tpc16-pas -inputfile ".\test\data\Pascal\TPC16.PAS" -expected ".\test\ref\$action\$language\TPC16.PAS.txt"

# Perl
Set-Variable -Name language "Perl"
test\bin\run_detect_test.ps1 -json -name perligata-perl -inputfile ".\test\data\Perl\Perligata.pm" -expected ".\test\ref\$action\$language\Perligata.pm.txt"
test\bin\run_detect_test.ps1 -json -name physics-perl -inputfile ".\test\data\Perl\Physics.pm" -expected ".\test\ref\$action\$language\Physics.pm.txt"
test\bin\run_detect_test.ps1 -json -name root_distance-perl -inputfile ".\test\data\$language\root_distance.pl" -expected ".\test\ref\$action\$language\root_distance.pl.txt"

# PL/1
Set-Variable -Name language "PL1"
test\bin\run_detect_test.ps1 -json -name bpgpli-$language -inputfile ".\test\data\$language\BPGPLI.pl1" -expected ".\test\ref\$action\$language\BPGPLI.pl1.txt"
test\bin\run_detect_test.ps1 -json -name checkdt-$language -inputfile ".\test\data\$language\CHECKDT.pl1" -expected ".\test\ref\$action\$language\CHECKDT.pl1.txt"
test\bin\run_detect_test.ps1 -json -name crtpln3-$language -inputfile ".\test\data\$language\CRTPLN3.pl1" -expected ".\test\ref\$action\$language\CRTPLN3.pl1.txt"
test\bin\run_detect_test.ps1 -json -name mainfact-$language -inputfile ".\test\data\$language\MAINFACT.pl1" -expected ".\test\ref\$action\$language\MAINFACT.pl1.txt"
test\bin\run_detect_test.ps1 -json -name example-$language -inputfile ".\test\data\$language\example.pl1" -expected ".\test\ref\$action\$language\example.pl1.txt"
test\bin\run_detect_test.ps1 -json -name digrams-$language -inputfile ".\test\data\$language\digrams.pl1" -expected ".\test\ref\$action\$language\digrams.pl1.txt"
test\bin\run_detect_test.ps1 -json -name ystkp-$language -inputfile ".\test\data\$language\ystkp.pl1" -expected ".\test\ref\$action\$language\ystkp.pl1.txt"

# PL/M
Set-Variable -Name language "PLM"
test\bin\run_detect_test.ps1 -json -name example-plm -inputfile ".\test\data\$language\example.plm" -expected ".\test\ref\$action\$language\example.plmtxt"

# Prolog
Set-Variable -Name language "Prolog"
test\bin\run_detect_test.ps1 -json -name family-main -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\$action\$language\family-main.pl.txt"
test\bin\run_detect_test.ps1 -json -name family-kb -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\$action\$language\family-kb.pl.txt"
test\bin\run_detect_test.ps1 -json -name family-menu -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\$action\$language\family-menu.pl.txt"
test\bin\run_detect_test.ps1 -json -name family-queries -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\$action\$language\family-queries.pl.txt"
test\bin\run_detect_test.ps1 -json -name web-server-hello-prolog -inputfile ".\test\data\Prolog\web-server-hello.pl" -expected ".\test\ref\$action\$language\web-server-hello.pl.txt"
test\bin\run_detect_test.ps1 -json -name web-server-params-prolog -inputfile ".\test\data\Prolog\web-server-params.pl" -expected ".\test\ref\$action\$language\web-server-params.pl.txt"

# Python
Set-Variable -Name language "Python"
test\bin\run_detect_test.ps1 -json -name drone-3d -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\$action\$language\drone_3d_trajectory_following.py.txt"
test\bin\run_detect_test.ps1 -json -name quadrotor -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\$action\$language\Quadrotor.py.txt"
test\bin\run_detect_test.ps1 -json -name trajectory -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\$action\$language\TrajectoryGenerator.py.txt"
test\bin\run_detect_test.ps1 -json -name values-python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\$action\$language\values.py.txt"
test\bin\run_detect_test.ps1 -json -name examiner -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\$action\$language\Examiner.py.txt"
test\bin\run_detect_test.ps1 -json -name authorized-view-python -inputfile ".\test\data\Python\authorized_view.py" -expected ".\test\ref\$action\$language\authorized_view.py.txt"

# R
Set-Variable -Name language "R"
test\bin\run_detect_test.ps1 -json -name ETM-540-01 -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\$action\$language\ETM-540-01.R.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-02 -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\$action\$language\ETM-540-02.R.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-03 -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\$action\$language\ETM-540-03.R.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-04 -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\$action\$language\ETM-540-04.R.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-05 -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\$action\$language\ETM-540-05.R.txt"
test\bin\run_detect_test.ps1 -json -name basketball-r -inputfile ".\test\data\R\basketball.R" -expected ".\test\ref\$action\$language\basketball.R.txt"
test\bin\run_detect_test.ps1 -json -name render-r -inputfile ".\test\data\R\render.R" -expected ".\test\ref\$action\$language\render.R.txt"

# Ruby
Set-Variable -Name language "Ruby"
test\bin\run_detect_test.ps1 -json -name basic-ruby -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\$action\$language\basic.rb.txt"
test\bin\run_detect_test.ps1 -json -name constants-ruby -inputfile ".\test\data\Ruby\constants.rb" -expected ".\test\ref\$action\$language\constants.rb.txt"
test\bin\run_detect_test.ps1 -json -name exceptions-ruby -inputfile ".\test\data\Ruby\exceptions.rb" -expected ".\test\ref\$action\$language\exceptions.rb.txt"
test\bin\run_detect_test.ps1 -json -name expressions-ruby -inputfile ".\test\data\Ruby\expressions.rb" -expected ".\test\ref\$action\$language\expressions.rb.txt"
test\bin\run_detect_test.ps1 -json -name functions-ruby -inputfile ".\test\data\Ruby\functions.rb" -expected ".\test\ref\$action\$language\functions.rb.txt"
test\bin\run_detect_test.ps1 -json -name io-ruby -inputfile ".\test\data\Ruby\io.rb" -expected ".\test\ref\$action\$language\io.rb.txt"
test\bin\run_detect_test.ps1 -json -name modifiers-ruby -inputfile ".\test\data\Ruby\modifiers.rb" -expected ".\test\ref\$action\$language\modifiers.rb.txt"
test\bin\run_detect_test.ps1 -json -name operators-ruby -inputfile ".\test\data\Ruby\operators.rb" -expected ".\test\ref\$action\$language\operators.rb.txt"
test\bin\run_detect_test.ps1 -json -name statements-ruby -inputfile ".\test\data\Ruby\statements.rb" -expected ".\test\ref\$action\$language\statements.rb.txt"
test\bin\run_detect_test.ps1 -json -name tokenbuilders-ruby -inputfile ".\test\data\Ruby\tokenbuilders.rb" -expected ".\test\ref\$action\$language\tokenbuilders.rb.txt"
test\bin\run_detect_test.ps1 -json -name tokenizers-ruby -inputfile ".\test\data\Ruby\tokenizers.rb" -expected ".\test\ref\$action\$language\tokenizers.rb.txt"
test\bin\run_detect_test.ps1 -json -name tokens-ruby -inputfile ".\test\data\Ruby\tokens.rb" -expected ".\test\ref\$action\$language\tokens.rb.txt"
test\bin\run_detect_test.ps1 -json -name webhook-ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\$action\$language\webhook.rb.txt"

# Rust
Set-Variable -Name language "Rust"
test\bin\run_detect_test.ps1 -json -name literals-$language -inputfile ".\test\data\$language\literals.rs" -expected ".\test\ref\$action\$language\literals.rs.txt"
test\bin\run_detect_test.ps1 -json -name dom-$language -inputfile ".\test\data\$language\dom.rs" -expected ".\test\ref\$action\$language\dom.rs.txt"
test\bin\run_detect_test.ps1 -json -name html-$language -inputfile ".\test\data\$language\html.rs" -expected ".\test\ref\$action\$language\html.rs.txt"
test\bin\run_detect_test.ps1 -json -name geometry-$language -inputfile ".\test\data\$language\geometry.rs" -expected ".\test\ref\$action\$language\geometry.rs.txt"
test\bin\run_detect_test.ps1 -json -name scene-$language -inputfile ".\test\data\$language\scene.rs" -expected ".\test\ref\$action\$language\scene.rs.txt"
test\bin\run_detect_test.ps1 -json -name comments-1-$language -inputfile ".\test\data\$language\nested_comments_1.rs" -expected ".\test\ref\$action\$language\nested_comments_1.rs.txt"
test\bin\run_detect_test.ps1 -json -name comments-2-$language -inputfile ".\test\data\$language\nested_comments_2.rs" -expected ".\test\ref\$action\$language\nested_comments_2.rs.txt"
test\bin\run_detect_test.ps1 -json -name comments-3-$language -inputfile ".\test\data\$language\nested_comments_3.rs" -expected ".\test\ref\$action\$language\nested_comments_3.rs.txt"
test\bin\run_detect_test.ps1 -json -name raw-string-1-$language -inputfile ".\test\data\$language\raw_string_1.rs" -expected ".\test\ref\$action\$language\raw_string_1.rs.txt"
test\bin\run_detect_test.ps1 -json -name raw-string-2-$language -inputfile ".\test\data\$language\raw_string_2.rs" -expected ".\test\ref\$action\$language\raw_string_2.rs.txt"
test\bin\run_detect_test.ps1 -json -name attributes-1-$language -inputfile ".\test\data\$language\attributes_1.rs" -expected ".\test\ref\$action\$language\attributes_1.rs.txt"
test\bin\run_detect_test.ps1 -json -name chip8-$language -inputfile ".\test\data\$language\chip8.rs" -expected ".\test\ref\$action\$language\chip8.rs.txt"
test\bin\run_detect_test.ps1 -json -name chip8-display-$language -inputfile ".\test\data\$language\chip8-display.rs" -expected ".\test\ref\$action\$language\chip8-display.rs.txt"
test\bin\run_detect_test.ps1 -json -name chip8-instructions-$language -inputfile ".\test\data\$language\chip8-instructions.rs" -expected ".\test\ref\$action\$language\chip8-instructions.rs.txt"
test\bin\run_detect_test.ps1 -json -name chip8-main-$language -inputfile ".\test\data\$language\chip8-main.rs" -expected ".\test\ref\$action\$language\chip8-main.rs.txt"
test\bin\run_detect_test.ps1 -json -name weird-$language -inputfile ".\test\data\$language\weird.rs" -expected ".\test\ref\$action\$language\weird.rs.txt"

# Scala
Set-Variable -Name language "Scala"
test\bin\run_detect_test.ps1 -json -name hello-$language -inputfile ".\test\data\$language\hello.scala" -expected ".\test\ref\$action\$language\hello.scala.txt"
test\bin\run_detect_test.ps1 -json -name larger-$language -inputfile ".\test\data\$language\larger.scala" -expected ".\test\ref\$action\$language\larger.scala.txt"
test\bin\run_detect_test.ps1 -json -name random-$language -inputfile ".\test\data\$language\random.scala" -expected ".\test\ref\$action\$language\random.scala.txt"
test\bin\run_detect_test.ps1 -json -name variables-$language -inputfile ".\test\data\$language\variables.scala" -expected ".\test\ref\$action\$language\variables.scala.txt"
test\bin\run_detect_test.ps1 -json -name zero_line-$language -inputfile ".\test\data\$language\zero_line_hello_world.scala" -expected ".\test\ref\$action\$language\zero_line_hello_world.scala.txt"
test\bin\run_detect_test.ps1 -json -name v3-$language -inputfile ".\test\data\$language\v3_control.$language" -expected ".\test\ref\$action\$language\v3_control.scala.txt"

# SQL

# PL-SQL
Set-Variable -Name language "PL-SQL"
test\bin\run_detect_test.ps1 -json -name table-plsql -inputfile ".\test\data\$language\table.sql" -expected ".\test\ref\$action\$language\table.sql.txt"

# T-SQL
Set-Variable -Name language "T-SQL"
test\bin\run_detect_test.ps1 -json -name microsoft-tsql -inputfile ".\test\data\$language\microsoft.sql" -expected ".\test\ref\$action\$language\microsoft.sql.txt"
test\bin\run_detect_test.ps1 -json -name brackets-tsql -inputfile ".\test\data\$language\brackets.sql" -expected ".\test\ref\$action\$language\brackets.sql.txt"

# Swift
Set-Variable -Name language "Swift"
test\bin\run_detect_test.ps1 -json -name AppDelegate -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\$action\$language\AppDelegate.swift.txt"
test\bin\run_detect_test.ps1 -json -name Meal -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\$action\$language\Meal.swift.txt"
test\bin\run_detect_test.ps1 -json -name MealTableViewCell -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\$action\$language\MealTableViewCell.swift.txt"
test\bin\run_detect_test.ps1 -json -name MealTableViewController -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\$action\$language\MealTableViewController.swift.txt"
test\bin\run_detect_test.ps1 -json -name MealViewController -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\$action\$language\MealViewController.swift.txt"
test\bin\run_detect_test.ps1 -json -name RatingControl -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\$action\$language\RatingControl.swift.txt"
test\bin\run_detect_test.ps1 -json -name URLExtensions -inputfile ".\test\data\Swift\URLExtensions.swift" -expected ".\test\ref\$action\$language\URLExtensions.swift.txt"

# TypeScript
Set-Variable -Name language "TypeScript"
test\bin\run_detect_test.ps1 -json -name TimeReporter-ts -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\$action\$language\TimeReporter.ts.txt"
test\bin\run_detect_test.ps1 -json -name ImageBoard-ts -inputfile ".\test\data\TypeScript\ImageBoard.ts" -expected ".\test\ref\$action\$language\ImageBoard.ts.txt"

# Visual Basic 6
Set-Variable -Name language "VisualBasic-6"
test\bin\run_detect_test.ps1 -json -name spider-vb6 -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\$action\$language\spider.bas.txt"
test\bin\run_detect_test.ps1 -json -name azure_metadata-vb6 -inputfile ".\test\data\VisualBasic-6\azure_metadata.bas" -expected ".\test\ref\$action\$language\azure_metadata.bas.txt"
test\bin\run_detect_test.ps1 -json -name diffie_hellman-vb6 -inputfile ".\test\data\VisualBasic-6\diffie_hellman.bas" -expected ".\test\ref\$action\$language\diffie_hellman.bas.txt"

# Visual Basic .NET
Set-Variable -Name language "VisualBasic-NET"
test\bin\run_detect_test.ps1 -json -name word-processor-vbnet -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\$action\$language\WordProcessor.bas.txt"

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
