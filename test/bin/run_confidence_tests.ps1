Set-StrictMode -Version Latest

Set-Variable -Name action "confidence"

# generic
test\bin\run_test.ps1 -json -name adabkend-adb -action $action -language generic -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\$action\generic\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action $action -language generic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\$action\generic\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action $action -language generic -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\$action\generic\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action $action -language generic -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\$action\generic\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action $action -language generic -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\$action\generic\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action $action -language generic -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\$action\generic\heron.txt"
test\bin\run_test.ps1 -json -name bing -action $action -language generic -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\$action\generic\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action $action -language generic -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\$action\generic\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action $action -language generic -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\$action\generic\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action $action -language generic -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\$action\generic\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action $action -language generic -inputfile ".\test\data\SQL\microsoft.sql" -expected ".\test\ref\$action\generic\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action $action -language generic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\$action\generic\spider.txt"

# generic with comments
test\bin\run_test.ps1 -json -name adabkend-adb -action $action -language generic -comment ada -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\$action\generic-comments\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action $action -language generic -comment basic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\$action\generic-comments\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action $action -language generic -comment c -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\$action\generic-comments\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action $action -language generic -comment cobol -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\$action\generic-comments\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action $action -language generic -comment cpp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\$action\generic-comments\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action $action -language generic -comment fortran -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\$action\generic-comments\heron.txt"
test\bin\run_test.ps1 -json -name bing -action $action -language generic -comment hash -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\$action\generic-comments\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action $action -language generic -comment percent -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\$action\generic-comments\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action $action -language generic -comment hash -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\$action\generic-comments\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action $action -language generic -comment hash -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\$action\generic-comments\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action $action -language generic -comment ada -inputfile ".\test\data\SQL\microsoft.sql" -expected ".\test\ref\$action\generic-comments\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action $action -language generic -comment basic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\$action\generic-comments\spider.txt"

# generic assembly

# ASM IBM-360
Set-Variable -Name language "ASM-360"
test\bin\run_test.ps1 -json -name subroutine1-asm -action $action -language $language -inputfile ".\test\data\$language\subroutine1.asm" -expected ".\test\ref\$action\$language\subroutine1.txt"
test\bin\run_test.ps1 -json -name subroutine1_a-asm -action $action -language $language -inputfile ".\test\data\$language\subroutine1_a.asm" -expected ".\test\ref\$action\$language\subroutine1_a.txt"
test\bin\run_test.ps1 -json -name subroutine2-asm -action $action -language $language -inputfile ".\test\data\$language\subroutine2.asm" -expected ".\test\ref\$action\$language\subroutine2.txt"
test\bin\run_test.ps1 -json -name subroutine2_a-asm -action $action -language $language -inputfile ".\test\data\$language\subroutine2_a.asm" -expected ".\test\ref\$action\$language\subroutine2_a.txt"
test\bin\run_test.ps1 -json -name spitbol-asm -action $action -language $language -inputfile ".\test\data\$language\spitbol.asm" -expected ".\test\ref\$action\$language\spitbol.txt"

# ASM IBM-370
Set-Variable -Name language "ASM-370"
test\bin\run_test.ps1 -json -name asm370a1-asm -action $action -language $language -inputfile ".\test\data\$language\asm370a1.asm" -expected ".\test\ref\$action\$language\asm370a1.txt"

# ASM IBM-390
Set-Variable -Name language "ASM-390"
test\bin\run_test.ps1 -json -name osint-asm -action $action -language $language -inputfile ".\test\data\$language\osint.asm" -expected ".\test\ref\$action\$language\osint.txt"

# ASM IBM-system-z

# ASM 1802
Set-Variable -Name language "ASM-1802"
test\bin\run_test.ps1 -json -name test_alu-asm -action $action -language $language -inputfile ".\test\data\$language\test_alu.asm" -expected ".\test\ref\$action\$language\test_alu.txt"

# ASM 6502
Set-Variable -Name language "ASM-6502"
test\bin\run_test.ps1 -json -name sweet16-asm -action $action -language $language -inputfile ".\test\data\$language\sweet16.asm" -expected ".\test\ref\$action\$language\sweet16.txt"

# ASM 6800
Set-Variable -Name language "ASM-6800"
test\bin\run_test.ps1 -json -name ET3400-S6-asm -action $action -language $language -inputfile ".\test\data\$language\ET3400-S6.asm" -expected ".\test\ref\$action\$language\ET3400-S6.txt"

# ASM 68000
Set-Variable -Name language "ASM-68000"
test\bin\run_test.ps1 -json -name basic68k-asm -action $action -language $language -inputfile ".\test\data\$language\basic68k.asm" -expected ".\test\ref\$action\$language\basic68k.txt"
test\bin\run_test.ps1 -json -name sprites-asm -action $action -language $language -inputfile ".\test\data\$language\sprites.asm" -expected ".\test\ref\$action\$language\sprites.txt"

# ASM 8080
Set-Variable -Name language "ASM-8080"
test\bin\run_test.ps1 -json -name i8080core-asm -action $action -language $language -inputfile ".\test\data\$language\i8080core.asm" -expected ".\test\ref\$action\$language\i8080core.txt"
test\bin\run_test.ps1 -json -name microcosm-asm -action $action -language $language -inputfile ".\test\data\$language\microcosm.asm" -expected ".\test\ref\$action\$language\microcosm.txt"
test\bin\run_test.ps1 -json -name hello_free-asm -action $action -language $language -inputfile ".\test\data\$language\hello_free.asm" -expected ".\test\ref\$action\$language\hello_free.txt"
test\bin\run_test.ps1 -json -name hello_spaced-asm -action $action -language $language -inputfile ".\test\data\$language\hello_spaced.asm" -expected ".\test\ref\$action\$language\hello_spaced.txt"

# ASM Z-80
Set-Variable -Name language "ASM-Z-80"
test\bin\run_test.ps1 -json -name intro-asm -action $action -language $language -inputfile ".\test\data\$language\intro.asm" -expected ".\test\ref\$action\$language\intro.txt"
test\bin\run_test.ps1 -json -name hardmess-asm -action $action -language $language -inputfile ".\test\data\$language\hardmess.asm" -expected ".\test\ref\$action\$language\hardmess.txt"
test\bin\run_test.ps1 -json -name shftspr-asm -action $action -language $language -inputfile ".\test\data\$language\shftspr.asm" -expected ".\test\ref\$action\$language\shftspr.txt"
test\bin\run_test.ps1 -json -name sincos-asm -action $action -language $language -inputfile ".\test\data\$language\sincos.asm" -expected ".\test\ref\$action\$language\sincos.txt"

# ASM 8086
Set-Variable -Name language "ASM-8086"
test\bin\run_test.ps1 -json -name BISTRS-asm -action $action -language $language -inputfile ".\test\data\$language\BISTRS.asm" -expected ".\test\ref\$action\$language\BISTRS.txt"
test\bin\run_test.ps1 -json -name DSKCOM-asm -action $action -language $language -inputfile ".\test\data\$language\DSKCOM.asm" -expected ".\test\ref\$action\$language\DSKCOM.txt"
test\bin\run_test.ps1 -json -name FIVEO-asm -action $action -language $language -inputfile ".\test\data\$language\FIVEO.asm" -expected ".\test\ref\$action\$language\FIVEO.txt"
test\bin\run_test.ps1 -json -name GWEVAL-asm -action $action -language $language -inputfile ".\test\data\$language\GWEVAL.asm" -expected ".\test\ref\$action\$language\GWEVAL.txt"
test\bin\run_test.ps1 -json -name NEXT86-asm -action $action -language $language -inputfile ".\test\data\$language\NEXT86.asm" -expected ".\test\ref\$action\$language\NEXT86.txt"

# $language
Set-Variable -Name language "ASM-80386"
test\bin\run_test.ps1 -json -name reverser-asm -action $action -language $language -inputfile ".\test\data\$language\reverser.asm" -expected ".\test\ref\$action\$language\reverser.txt"
test\bin\run_test.ps1 -json -name crc16-asm -action $action -language $language -inputfile ".\test\data\$language\crc16.asm" -expected ".\test\ref\$action\$language\crc16.txt"
test\bin\run_test.ps1 -json -name mat_get_element-asm -action $action -language $language -inputfile ".\test\data\$language\mat_get_element.asm" -expected ".\test\ref\$action\$language\mat_get_element.txt"

# ASM-80486

# ASM PDP-8
Set-Variable -Name language "ASM-PDP-8"
test\bin\run_test.ps1 -json -name io-asm -action $action -language $language -inputfile ".\test\data\$language\io.asm" -expected ".\test\ref\$action\$language\io.txt"
test\bin\run_test.ps1 -json -name life-asm -action $action -language $language -inputfile ".\test\data\$language\life.asm" -expected ".\test\ref\$action\$language\life.txt"
test\bin\run_test.ps1 -json -name sum-asm -action $action -language $language -inputfile ".\test\data\$language\sum.asm" -expected ".\test\ref\$action\$language\sum.txt"

# ASM PDP-11
Set-Variable -Name language "ASM-PDP-11"
test\bin\run_test.ps1 -json -name print-asm -action $action -language $language -inputfile ".\test\data\$language\print.asm" -expected ".\test\ref\$action\$language\print.txt"
test\bin\run_test.ps1 -json -name k11tsx-asm -action $action -language $language -inputfile ".\test\data\$language\k11tsx.mac" -expected ".\test\ref\$action\$language\k11tsx.txt"
test\bin\run_test.ps1 -json -name krtser-asm -action $action -language $language -inputfile ".\test\data\$language\krtser.mac" -expected ".\test\ref\$action\$language\krtser.txt"
test\bin\run_test.ps1 -json -name krtsub-asm -action $action -language $language -inputfile ".\test\data\$language\krtsub.mac" -expected ".\test\ref\$action\$language\krtsub.txt"

# Ada 83
Set-Variable -Name language "Ada-83"
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action $action -language $language -inputfile ".\test\data\$language\adabkend.adb" -expected ".\test\ref\$action\$language\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-83 -action $action -language $language -inputfile ".\test\data\$language\adabkend.ads" -expected ".\test\ref\$action\$language\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-83 -action $action -language $language -inputfile ".\test\data\$language\signup.adb" -expected ".\test\ref\$action\$language\signup-adb.txt"

# Ada 95
Set-Variable -Name language "Ada-95"
test\bin\run_test.ps1 -json -name philosophers-ads-95 -action $action -language $language -inputfile ".\test\data\$language\philosophers.ads" -expected ".\test\ref\$action\$language\philosophers-ads.txt"

# Ada-2005

# Ada-2012

# Awk
Set-Variable -Name language "Awk"
test\bin\run_test.ps1 -json -name funstack-awk -action $action -language $language -inputfile ".\test\data\$language\funstack.awk" -expected ".\test\ref\$action\$language\funstack.txt"
test\bin\run_test.ps1 -json -name awkaster-awk -action $action -language $language -inputfile ".\test\data\$language\awkaster.awk" -expected ".\test\ref\$action\$language\awkaster.txt"

# BASIC
Set-Variable -Name language "BASIC"
test\bin\run_test.ps1 -json -name values-bas -action $action -language $language -inputfile ".\test\data\$language\values.bas" -expected ".\test\ref\$action\$language\values.txt"
test\bin\run_test.ps1 -json -name simple-bas -action $action -language $language -inputfile ".\test\data\$language\simple.bas" -expected ".\test\ref\$action\$language\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot-bas -action $action -language $language -inputfile ".\test\data\$language\3dplot.bas" -expected ".\test\ref\$action\$language\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action $action -language $language -inputfile ".\test\data\$language\batnum.bas" -expected ".\test\ref\$action\$language\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action $action -language $language -inputfile ".\test\data\$language\life.bas" -expected ".\test\ref\$action\$language\life.txt"
test\bin\run_test.ps1 -json -name income-bas -action $action -language $language -inputfile ".\test\data\$language\income.bas" -expected ".\test\ref\$action\$language\income.txt"
test\bin\run_test.ps1 -json -name rockt2-bas -action $action -language $language -inputfile ".\test\data\$language\rockt2.bas" -expected ".\test\ref\$action\$language\rockt2.txt"

# BASIC-80
Set-Variable -Name language "BASIC-80"
test\bin\run_test.ps1 -json -name doctor-mbasic -action $action -language $language -inputfile ".\test\data\BASIC\doctor.bas" -expected ".\test\ref\$action\$language\doctor.txt"
test\bin\run_test.ps1 -json -name airinput-mbasic -action $action -language $language -inputfile ".\test\data\BASIC\AIRINPUT.bas" -expected ".\test\ref\$action\$language\AIRINPUT.txt"
test\bin\run_test.ps1 -json -name backgamm-mbasic -action $action -language $language -inputfile ".\test\data\BASIC\BACKGAMM.bas" -expected ".\test\ref\$action\$language\BACKGAMM.txt"
test\bin\run_test.ps1 -json -name planes-mbasic -action $action -language $language -inputfile ".\test\data\BASIC\PLANES.bas" -expected ".\test\ref\$action\$language\PLANES.txt"

# BASICA
Set-Variable -Name language "BASICA"
test\bin\run_test.ps1 -json -name sea-creature-basica -action $action -language $language -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\$action\$language\sea_creature.txt"
test\bin\run_test.ps1 -json -name lanturn-basica -action $action -language $language -inputfile ".\test\data\BASIC\lanturn.bas" -expected ".\test\ref\$action\$language\lanturn.txt"
test\bin\run_test.ps1 -json -name gw3d-basica -action $action -language $language -inputfile ".\test\data\BASIC\GW3D.bas" -expected ".\test\ref\$action\$language\GW3D.txt"

# C-78
Set-Variable -Name language "C-78"
test\bin\run_test.ps1 -json -name diamond-c-78 -action $action -language $language -inputfile ".\test\data\$language\diamond.c" -expected ".\test\ref\$action\$language\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test-c-78 -action $action -language $language -inputfile ".\test\data\$language\prime_test.c" -expected ".\test\ref\$action\$language\prime_test.txt"
test\bin\run_test.ps1 -json -name values-c-78 -action $action -language $language -inputfile ".\test\data\$language\values.c" -expected ".\test\ref\$action\$language\values.txt"
test\bin\run_test.ps1 -json -name j_interpreter-c-78 -action $action -language $language -inputfile ".\test\data\$language\j_interpreter.c" -expected ".\test\ref\$action\$language\j_interpreter.txt"
test\bin\run_test.ps1 -json -name zero_line-c-78 -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.c" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"
test\bin\run_test.ps1 -json -name charlotte-c-78 -action $action -language $language -inputfile ".\test\data\$language\charlotte.c" -expected ".\test\ref\$action\$language\charlotte.txt"

# C-89

# C-99
Set-Variable -Name language "C-99"
test\bin\run_test.ps1 -json -name decl-c-99 -action $action -language $language -inputfile ".\test\data\$language\c-decl.c" -expected ".\test\ref\$action\$language\c-decl-c.txt"
test\bin\run_test.ps1 -json -name parser-h-99 -action $action -language $language -inputfile ".\test\data\$language\parser.h" -expected ".\test\ref\$action\$language\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c-99 -action $action -language $language -inputfile ".\test\data\$language\parser.c" -expected ".\test\ref\$action\$language\parser-c.txt"

# CBASIC
Set-Variable -Name language "CBASIC"
test\bin\run_test.ps1 -json -name fibo -action $action -language $language -inputfile ".\test\data\$language\fibo.bas" -expected ".\test\ref\$action\$language\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action $action -language $language -inputfile ".\test\data\$language\DEMOGRAF.BAS" -expected ".\test\ref\$action\$language\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action $action -language $language -inputfile ".\test\data\$language\GRAPHR.BAS" -expected ".\test\ref\$action\$language\GRAPHR.txt"

# COBOL-68
Set-Variable -Name language "COBOL-68"
test\bin\run_test.ps1 -json -name mccracken1-68 -action $action -language $language -inputfile ".\test\data\$language\mccracken1.cob" -expected ".\test\ref\$action\$language\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action $action -language $language -inputfile ".\test\data\$language\mccracken2.cob" -expected ".\test\ref\$action\$language\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action $action -language $language -inputfile ".\test\data\$language\mccracken3.cob" -expected ".\test\ref\$action\$language\mccracken3.txt"
test\bin\run_test.ps1 -json -name prog1-68 -action $action -language $language -inputfile ".\test\data\$language\PROG1.COB" -expected ".\test\ref\$action\$language\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-68 -action $action -language $language -inputfile ".\test\data\$language\PROG2.COB" -expected ".\test\ref\$action\$language\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-68 -action $action -language $language -inputfile ".\test\data\$language\PROG2A.COB" -expected ".\test\ref\$action\$language\PROG2A.txt"

# COBOL-74

# COBOL-85
Set-Variable -Name language "COBOL-85"
test\bin\run_test.ps1 -json -name prog3-85 -action $action -language $language -inputfile ".\test\data\$language\PROG3.COB" -expected ".\test\ref\$action\$language\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-85 -action $action -language $language -inputfile ".\test\data\$language\PROG4.COB" -expected ".\test\ref\$action\$language\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-85 -action $action -language $language -inputfile ".\test\data\$language\PROG5.COB" -expected ".\test\ref\$action\$language\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-85 -action $action -language $language -inputfile ".\test\data\$language\PROG6.COB" -expected ".\test\ref\$action\$language\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-85 -action $action -language $language -inputfile ".\test\data\$language\PROG12-2.COB" -expected ".\test\ref\$action\$language\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-85 -action $action -language $language -inputfile ".\test\data\$language\PROG13-3.COB" -expected ".\test\ref\$action\$language\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-85 -action $action -language $language -inputfile ".\test\data\$language\PROG14-2.COB" -expected ".\test\ref\$action\$language\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-85 -action $action -language $language -inputfile ".\test\data\$language\PROG15-4.COB" -expected ".\test\ref\$action\$language\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-85 -action $action -language $language -tabsize 4 -inputfile ".\test\data\$language\UNLDDBCU2.COB" -expected ".\test\ref\$action\$language\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-85 -action $action -language $language -inputfile ".\test\data\$language\P010.cob" -expected ".\test\ref\$action\$language\P010.txt"

# COBOL-2002
Set-Variable -Name language "COBOL-2002"
test\bin\run_test.ps1 -json -name report-card-cob2002 -action $action -language $language -tabsize 4 -inputfile ".\test\data\$language\ReportCard.cob" -expected ".\test\ref\$action\$language\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2002 -action $action -language $language -tabsize 4 -inputfile ".\test\data\$language\Person.cob" -expected ".\test\ref\$action\$language\Person.txt"
test\bin\run_test.ps1 -json -name report-cob2002 -action $action -language $language -tabsize 4 -inputfile ".\test\data\$language\Report.cob" -expected ".\test\ref\$action\$language\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action $action -language $language -inputfile ".\test\data\$language\AdventOfCode.cob" -expected ".\test\ref\$action\$language\AdventOfCode.txt"
test\bin\run_test.ps1 -json -name P010-wide-2002 -action $action -language $language -inputfile ".\test\data\$language\P010-wide.COB" -wide -expected ".\test\ref\$action\$language\P010-wide.txt"

# COBOL-2014

# COBOL-2014 with AcuCobol extensions

# COBOL-2014 with IBM extensions

# COBOL-2014 with GNU extensions
Set-Variable -Name language "COBOL-2014-GNU"
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action $action -language $language -tabsize 4 -inputfile ".\test\data\$language\SalesReport.cob" -expected ".\test\ref\$action\$language\SalesReport.txt"

# C++
Set-Variable -Name language "Cplusplus"
test\bin\run_test.ps1 -json -name checkers-cpp -action $action -language $language -inputfile ".\test\data\$language\checkers.cpp" -expected ".\test\ref\$action\$language\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb-cpp -action $action -language $language -inputfile ".\test\data\$language\hrdb.cpp" -expected ".\test\ref\$action\$language\hrdb.txt"
test\bin\run_test.ps1 -json -name date-h-cpp -action $action -language $language -inputfile ".\test\data\$language\date.h" -expected ".\test\ref\$action\$language\date_h.txt"
test\bin\run_test.ps1 -json -name date-cpp -action $action -language $language -inputfile ".\test\data\$language\date.cpp" -expected ".\test\ref\$action\$language\date_cpp.txt"
test\bin\run_test.ps1 -json -name inherit-cpp -action $action -language $language -inputfile ".\test\data\$language\inherit.cpp" -expected ".\test\ref\$action\$language\inherit.txt"
test\bin\run_test.ps1 -json -name zero_line-cpp -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.cpp" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# C#
Set-Variable -Name language "Csharp"
test\bin\run_test.ps1 -json -name calculator1-cs -action $action -language $language -inputfile ".\test\data\$language\calculator1.cs" -expected ".\test\ref\$action\$language\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2-cs -action $action -language $language -inputfile ".\test\data\$language\calculator2.cs" -expected ".\test\ref\$action\$language\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3-cs -action $action -language $language -inputfile ".\test\data\$language\calculator3.cs" -expected ".\test\ref\$action\$language\calculator3.txt"
test\bin\run_test.ps1 -json -name zero_line-cs -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.cs" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# CoffeeScript
Set-Variable -Name language "CoffeeScript"
test\bin\run_test.ps1 -json -name provider-coffee -action $action -language $language -inputfile ".\test\data\$language\provider.coffee" -expected ".\test\ref\$action\$language\provider.txt"
test\bin\run_test.ps1 -json -name resque-coffee -action $action -language $language -inputfile ".\test\data\$language\resque.coffee" -expected ".\test\ref\$action\$language\resque.txt"
test\bin\run_test.ps1 -json -name world-map-coffee -action $action -language $language -inputfile ".\test\data\$language\world_map.coffee" -expected ".\test\ref\$action\$language\world_map.txt"

# D
Set-Variable -Name language "D"
test\bin\run_test.ps1 -json -name regex-d -action $action -language $language -inputfile ".\test\data\d\regex.d" -expected ".\test\ref\$action\$language\regex.txt"
test\bin\run_test.ps1 -json -name halffloat-d -action $action -language $language -inputfile ".\test\data\d\halffloat.d" -expected ".\test\ref\$action\$language\halffloat.txt"
test\bin\run_test.ps1 -json -name wc-d -action $action -language $language -inputfile ".\test\data\d\wc.d" -expected ".\test\ref\$action\$language\wc.txt"

# Dart
Set-Variable -Name language "Dart"
test\bin\run_test.ps1 -json -name anagram-dart -action $action -language $language -inputfile ".\test\data\$language\anagram.dart" -expected ".\test\ref\$action\$language\anagram.txt"
test\bin\run_test.ps1 -json -name note_client-dart -action $action -language $language -inputfile ".\test\data\$language\note_client.dart" -expected ".\test\ref\$action\$language\note_client.txt"
test\bin\run_test.ps1 -json -name web_app-dart -action $action -language $language -inputfile ".\test\data\$language\web_app.dart" -expected ".\test\ref\$action\$language\web_app.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.dart" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# dbase II
Set-Variable -Name language "dbase-ii"
test\bin\run_test.ps1 -json -name sample-dbii -action $action -language $language -inputfile ".\test\data\$language\sample.prg" -expected ".\test\ref\$action\$language\sample.txt"
test\bin\run_test.ps1 -json -name addm-dbii -action $action -language $language -inputfile ".\test\data\$language\ADDM.PRG" -expected ".\test\ref\$action\$language\ADDM.txt"
test\bin\run_test.ps1 -json -name changedm-dbii -action $action -language $language -inputfile ".\test\data\$language\CHANGEDM.PRG" -expected ".\test\ref\$action\$language\CHANGEDM.txt"
test\bin\run_test.ps1 -json -name dater-dbii -action $action -language $language -inputfile ".\test\data\$language\DATER.PRG" -expected ".\test\ref\$action\$language\DATER.txt"
test\bin\run_test.ps1 -json -name updatedm-dbii -action $action -language $language -inputfile ".\test\data\$language\UPDATEDM.PRG" -expected ".\test\ref\$action\$language\UPDATEDM.txt"
test\bin\run_test.ps1 -json -name viewdm-dbii -action $action -language $language -inputfile ".\test\data\$language\VIEWDM.PRG" -expected ".\test\ref\$action\$language\VIEWDM.txt"
test\bin\run_test.ps1 -json -name emain-dbii -action $action -language $language -inputfile ".\test\data\$language\EMAIN.PRG" -expected ".\test\ref\$action\$language\EMAIN.txt"
test\bin\run_test.ps1 -json -name emp-entr-dbii -action $action -language $language -inputfile ".\test\data\$language\EMP-ENTR.PRG" -expected ".\test\ref\$action\$language\EMP-ENTR.txt"
test\bin\run_test.ps1 -json -name emp-rprt-dbii -action $action -language $language -inputfile ".\test\data\$language\EMP-RPRT.PRG" -expected ".\test\ref\$action\$language\EMP-RPRT.txt"
test\bin\run_test.ps1 -json -name emp-term-dbii -action $action -language $language -inputfile ".\test\data\$language\EMP-TERM.PRG" -expected ".\test\ref\$action\$language\EMP-TERM.txt"
test\bin\run_test.ps1 -json -name emp-upd-dbii -action $action -language $language -inputfile ".\test\data\$language\EMP-UPD.PRG" -expected ".\test\ref\$action\$language\EMP-UPD.txt"
test\bin\run_test.ps1 -json -name invmaint-dbii -action $action -language $language -inputfile ".\test\data\$language\INVMAINT.PRG" -expected ".\test\ref\$action\$language\INVMAINT.txt"
test\bin\run_test.ps1 -json -name invquan-dbii -action $action -language $language -inputfile ".\test\data\$language\INVQUAN.PRG" -expected ".\test\ref\$action\$language\INVQUAN.txt"
test\bin\run_test.ps1 -json -name invread-dbii -action $action -language $language -inputfile ".\test\data\$language\INVREAD.PRG" -expected ".\test\ref\$action\$language\INVREAD.txt"
test\bin\run_test.ps1 -json -name invrprt-dbii -action $action -language $language -inputfile ".\test\data\$language\INVRPRT.PRG" -expected ".\test\ref\$action\$language\INVRPRT.txt"

# dBase III
Set-Variable -Name language "dbase-III"
test\bin\run_test.ps1 -json -name fdate-dbiii -action $action -language $language -inputfile ".\test\data\$language\FDATE.PRG" -expected ".\test\ref\$action\$language\FDATE.txt"
test\bin\run_test.ps1 -json -name library-dbiii -action $action -language $language -inputfile ".\test\data\$language\LIBRARY.PRG" -expected ".\test\ref\$action\$language\LIBRARY.txt"
test\bin\run_test.ps1 -json -name dp_sort-dbiii -action $action -language $language -inputfile ".\test\data\$language\DP_SORT.PRG" -expected ".\test\ref\$action\$language\DP_SORT.txt"

# Delphi
Set-Variable -Name language "Delphi"
test\bin\run_test.ps1 -json -name FmMain-dfm-delphi -action $action -language $language -inputfile ".\test\data\$language\FmMain.dfm" -expected ".\test\ref\$action\$language\FmMain-dfm.txt"
test\bin\run_test.ps1 -json -name FmMain-pas-delphi -action $action -language $language -inputfile ".\test\data\$language\FmMain.pas" -expected ".\test\ref\$action\$language\FmMain-pas.txt"
test\bin\run_test.ps1 -json -name UCalc-delphi -action $action -language $language -inputfile ".\test\data\$language\UCalc.pas" -expected ".\test\ref\$action\$language\UCalc.txt"
test\bin\run_test.ps1 -json -name UChessBoardCmp-delphi -action $action -language $language -inputfile ".\test\data\$language\UChessBoardCmp.pas" -expected ".\test\ref\$action\$language\UChessBoardCmp.txt"
test\bin\run_test.ps1 -json -name UPlatform-delphi -action $action -language $language -inputfile ".\test\data\$language\UPlatform.pas" -expected ".\test\ref\$action\$language\UPlatform.txt"

# Dibol
$language= "Dibol"
test\bin\run_test.ps1 -json -name bottles-dibol -action $action -language $language -inputfile ".\test\data\$language\bottles.dbl" -expected ".\test\ref\$action\$language\bottles.txt"

# Eiffel
Set-Variable -Name language "Eiffel"
test\bin\run_test.ps1 -json -name hello-eiffel -action $action -language $language -inputfile ".\test\data\$language\hello.e" -expected ".\test\ref\$action\$language\hello.txt"
test\bin\run_test.ps1 -json -name bakery-eiffel -action $action -language $language -inputfile ".\test\data\$language\bakery.e" -expected ".\test\ref\$action\$language\bakery.txt"
test\bin\run_test.ps1 -json -name reverse-eiffel -action $action -language $language -inputfile ".\test\data\$language\reverse.e" -expected ".\test\ref\$action\$language\reverse.txt"
test\bin\run_test.ps1 -json -name bottles-eiffel -action $action -language $language -inputfile ".\test\data\$language\bottles_of_beer.e" -expected ".\test\ref\$action\$language\bottles_of_beer.txt"

# Erlang
Set-Variable -Name language "Erlang"
test\bin\run_test.ps1 -json -name armstrong-erlang -action $action -language $language -inputfile ".\test\data\$language\armstrong.erl" -expected ".\test\ref\$action\$language\armstrong.txt"
test\bin\run_test.ps1 -json -name list_comprehension-erlang -action $action -language $language -inputfile ".\test\data\$language\list_comprehension.erl" -expected ".\test\ref\$action\$language\list_comprehension.txt"
test\bin\run_test.ps1 -json -name send_receive-erlang -action $action -language $language -inputfile ".\test\data\$language\send_receive.erl" -expected ".\test\ref\$action\$language\send_receive.txt"

# FORTRAN-66
Set-Variable -Name language "FORTRAN-66"
test\bin\run_test.ps1 -json -name hello-ftn66 -action $action -language $language -inputfile ".\test\data\$language\HELLO.FOR" -expected ".\test\ref\$action\$language\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action $action -language $language -inputfile ".\test\data\$language\HERON.FOR" -expected ".\test\ref\$action\$language\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action $action -language $language -inputfile ".\test\data\$language\HERON2.FOR" -expected ".\test\ref\$action\$language\HERON2.txt"
test\bin\run_test.ps1 -json -name heron-wide-ftn66 -action $action -language $language -inputfile ".\test\data\$language\HERON-wide.FOR" -wide -expected ".\test\ref\$action\$language\HERON-wide.txt"

# FORTRAN-77
Set-Variable -Name language "FORTRAN-77"
test\bin\run_test.ps1 -json -name hello-ftn77 -action $action -language $language -inputfile ".\test\data\$language\HELLO.F77" -expected ".\test\ref\$action\$language\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action $action -language $language -inputfile ".\test\data\$language\COMPLEX.F77" -expected ".\test\ref\$action\$language\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action $action -language $language -inputfile ".\test\data\$language\EUCLID.F77" -expected ".\test\ref\$action\$language\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action $action -language $language -inputfile ".\test\data\$language\HERON.F77" -expected ".\test\ref\$action\$language\HERON.txt"

# Fortran-90
Set-Variable -Name language "Fortran-90"
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action $action -language $language -inputfile ".\test\data\$language\cylinder.f90" -expected ".\test\ref\$action\$language\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action $action -language $language -inputfile ".\test\data\$language\gauss.f90" -expected ".\test\ref\$action\$language\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action $action -language $language -inputfile ".\test\data\$language\hello.f90" -expected ".\test\ref\$action\$language\hello.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn90 -action $action -language $language -inputfile ".\test\data\$language\temp_sub.for" -expected ".\test\ref\$action\$language\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn90 -action $action -language $language -inputfile ".\test\data\$language\temp_func.for" -expected ".\test\ref\$action\$language\temp_func.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn90 -action $action -language $language -inputfile ".\test\data\$language\Wikibooks-ex2.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex2.txt"

# Fortran-95
Set-Variable -Name language "Fortran-95"
test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action $action -language $language -inputfile ".\test\data\$language\ibm-ex1.f95" -expected ".\test\ref\$action\$language\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action $action -language $language -inputfile ".\test\data\$language\Wikibooks-ex1.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex1.txt"

# Fortran-2003
Set-Variable -Name language "Fortran-2003"
test\bin\run_test.ps1 -json -name average-ftn2003 -action $action -language $language -inputfile ".\test\data\$language\average.f90" -expected ".\test\ref\$action\$language\average.txt"
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action $action -language $language -inputfile ".\test\data\$language\geo4060.for" -expected ".\test\ref\$action\$language\geo4060.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn2003 -action $action -language $language -inputfile ".\test\data\$language\Wikibooks-ex3.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn2003 -action $action -language $language -inputfile ".\test\data\$language\Wikibooks-ex4.f95" -expected ".\test\ref\$action\$language\Wikibooks-ex4.txt"

# Fortran-2008

# F#
Set-Variable -Name language "Fsharp"
test\bin\run_test.ps1 -json -name samples-fs -action $action -language $language -inputfile ".\test\data\$language\samples.fs" -expected ".\test\ref\$action\$language\samples.txt"
test\bin\run_test.ps1 -json -name antwalk-fs -action $action -language $language -inputfile ".\test\data\$language\ant_walk.fs" -expected ".\test\ref\$action\$language\ant_walk.txt"

# Go
Set-Variable -Name language "Go"
test\bin\run_test.ps1 -json -name find-cli-go -action $action -language $language -inputfile ".\test\data\$language\find-cli.go" -expected ".\test\ref\$action\$language\find-cli.txt"
test\bin\run_test.ps1 -json -name zero_line-go -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.go" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# Groovy
Set-Variable -Name language "Groovy"
test\bin\run_test.ps1 -json -name koan05-$language -action $action -language $language -inputfile ".\test\data\$language\koan05.groovy" -expected ".\test\ref\$action\$language\koan05.txt"
test\bin\run_test.ps1 -json -name koan13-$language -action $action -language $language -inputfile ".\test\data\$language\koan13.groovy" -expected ".\test\ref\$action\$language\koan13.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.groovy" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# Haskell
Set-Variable -Name language "Haskell"
test\bin\run_test.ps1 -json -name calendar-hs -action $action -language $language -inputfile ".\test\data\$language\calendar.hs" -expected ".\test\ref\$action\$language\calendar.txt"
test\bin\run_test.ps1 -json -name todo-hs -action $action -language $language -inputfile ".\test\data\$language\todo.hs" -expected ".\test\ref\$action\$language\todo.txt"
test\bin\run_test.ps1 -json -name rna-hs -action $action -language $language -inputfile ".\test\data\$language\todo.hs" -expected ".\test\ref\$action\$language\rna.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.hs" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# HTML
Set-Variable -Name language "HTML"
test\bin\run_test.ps1 -json -name knuth-html -action $action -language $language -inputfile ".\test\data\$language\knuth.html" -expected ".\test\ref\$action\$language\knuth.txt"
test\bin\run_test.ps1 -json -name developer-html -action $action -language $language -inputfile ".\test\data\$language\developer-css.html" -expected ".\test\ref\$action\$language\developer-css.txt"
test\bin\run_test.ps1 -json -name codestat-html -action $action -language $language -inputfile ".\test\data\$language\codestat-css-javascript.html" -expected ".\test\ref\$action\$language\codestat-css-$language.txt"

# Intercal
Set-Variable -Name language "Intercal"
test\bin\run_test.ps1 -json -name fizzbuzz-ical -action $action -language $language -inputfile ".\test\data\$language\fizzbuzz.intercal" -expected ".\test\ref\$action\$language\fizzbuzz.txt"

# Java
Set-Variable -Name language "Java"
test\bin\run_test.ps1 -json -name prime_test-java -action $action -language $language -inputfile ".\test\data\$language\prime_test.java" -expected ".\test\ref\$action\$language\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-java -action $action -language $language -inputfile ".\test\data\$language\palindrome.java" -expected ".\test\ref\$action\$language\palindrome.txt"
test\bin\run_test.ps1 -json -name binary-search-java -action $action -language $language -inputfile ".\test\data\$language\binary_search.java" -expected ".\test\ref\$action\$language\binary_search.txt"
test\bin\run_test.ps1 -json -name ObjectServer-java -action $action -language $language -inputfile ".\test\data\$language\ObjectServer.java" -expected ".\test\ref\$action\$language\ObjectServer.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.java" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# JavaScript
Set-Variable -Name language "JavaScript"
test\bin\run_test.ps1 -json -name values-js -action $action -language $language -inputfile ".\test\data\$language\values.js" -expected ".\test\ref\$action\$language\values.txt"
test\bin\run_test.ps1 -json -name codestat-js -action $action -language $language -inputfile ".\test\data\$language\codestat.js" -expected ".\test\ref\$action\$language\codestat.txt"
test\bin\run_test.ps1 -json -name bing-js -action $action -language $language -inputfile ".\test\data\$language\bing.js" -expected ".\test\ref\$action\$language\bing.txt"
test\bin\run_test.ps1 -json -name calc_prime-js -action $action -language $language -inputfile ".\test\data\$language\calc_prime.js" -expected ".\test\ref\$action\$language\calc_prime.txt"
test\bin\run_test.ps1 -json -name backtick-js -action $action -language $language -inputfile ".\test\data\$language\backtick.js" -expected ".\test\ref\$action\$language\backtick.txt"

# Julia
Set-Variable -Name language "Julia"
test\bin\run_test.ps1 -json -name dsp-jl -action $action -language $language -inputfile ".\test\data\$language\dsp.jl" -expected ".\test\ref\$action\$language\dsp.txt"
test\bin\run_test.ps1 -json -name container-jl -action $action -language $language -inputfile ".\test\data\$language\container.jl" -expected ".\test\ref\$action\$language\container.txt"
test\bin\run_test.ps1 -json -name microbiome-jl -action $action -language $language -inputfile ".\test\data\$language\microbiome.jl" -expected ".\test\ref\$action\$language\microbiome.txt"
test\bin\run_test.ps1 -json -name periodograms-jl -action $action -language $language -inputfile ".\test\data\$language\periodograms.jl" -expected ".\test\ref\$action\$language\periodograms.txt"

# Kotlin
Set-Variable -Name language "Kotlin"
test\bin\run_test.ps1 -json -name qksms-kt -action $action -language $language -inputfile ".\test\data\$language\qksms.kt" -expected ".\test\ref\$action\$language\qksms.txt"
test\bin\run_test.ps1 -json -name render-kt -action $action -language $language -inputfile ".\test\data\$language\render.kt" -expected ".\test\ref\$action\$language\render.txt"

# Lua
Set-Variable -Name language "Lua"
test\bin\run_test.ps1 -json -name dissector-lua -action $action -language $language -inputfile ".\test\data\$language\dissector.lua" -expected ".\test\ref\$action\$language\dissector.txt"
test\bin\run_test.ps1 -json -name dissector2-lua -action $action -language $language -inputfile ".\test\data\$language\dissector2.lua" -expected ".\test\ref\$action\$language\dissector2.txt"
test\bin\run_test.ps1 -json -name dsl-lua -action $action -language $language -inputfile ".\test\data\$language\dsl.lua" -expected ".\test\ref\$action\$language\dsl.txt"
test\bin\run_test.ps1 -json -name markov-lua -action $action -language $language -inputfile ".\test\data\$language\markov.lua" -expected ".\test\ref\$action\$language\markov.txt"
test\bin\run_test.ps1 -json -name zero_line-lua -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.lua" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# Matlab
$language= "Matlab"
test\bin\run_test.ps1 -json -name transpose-$language -action $action -language $language -inputfile ".\test\data\$language\transpose.m" -expected ".\test\ref\$action\$language\transpose.txt"
test\bin\run_test.ps1 -json -name choose-$language -action $action -language $language -inputfile ".\test\data\$language\choose.m" -expected ".\test\ref\$action\$language\choose.txt"
test\bin\run_test.ps1 -json -name fitnormal-$language -action $action -language $language -inputfile ".\test\data\$language\fitnormal.m" -expected ".\test\ref\$action\$language\fitnormal.txt"
test\bin\run_test.ps1 -json -name test_fitnormal-$language -action $action -language $language -inputfile ".\test\data\$language\test_fitnormal.m" -expected ".\test\ref\$action\$language\test_fitnormal.txt"

# Modula-2
Set-Variable -Name language "Modula-2"
test\bin\run_test.ps1 -json -name C64ToIBM-mod2 -action $action -language $language -inputfile ".\test\data\$language\C64ToIBM.mod" -expected ".\test\ref\$action\$language\C64ToIBM.txt"
test\bin\run_test.ps1 -json -name CaseDemo-mod2 -action $action -language $language -inputfile ".\test\data\$language\CaseDemo.mod" -expected ".\test\ref\$action\$language\CaseDemo.txt"
test\bin\run_test.ps1 -json -name game_def-mod2 -action $action -language $language -inputfile ".\test\data\$language\game.def" -expected ".\test\ref\$action\$language\game_def.txt"
test\bin\run_test.ps1 -json -name game-mod2 -action $action -language $language -inputfile ".\test\data\$language\game.mod" -expected ".\test\ref\$action\$language\game.txt"
test\bin\run_test.ps1 -json -name LoopDemo-mod2 -action $action -language $language -inputfile ".\test\data\$language\LoopDemo.mod" -expected ".\test\ref\$action\$language\LoopDemo.txt"
test\bin\run_test.ps1 -json -name PigLatin-mod2 -action $action -language $language -inputfile ".\test\data\$language\PigLatin.mod" -expected ".\test\ref\$action\$language\PigLatin.txt"
test\bin\run_test.ps1 -json -name TempConv-mod2 -action $action -language $language -inputfile ".\test\data\$language\TempConv.mod" -expected ".\test\ref\$action\$language\TempConv.txt"

# Objective-C
Set-Variable -Name language "Objective-C"
test\bin\run_test.ps1 -json -name hello-objc -action $action -language $language -inputfile ".\test\data\$language\HelloWorld.objc" -expected ".\test\ref\$action\$language\HelloWorld.txt"
test\bin\run_test.ps1 -json -name qrmath-objc -action $action -language $language -inputfile ".\test\data\$language\QRMath.h" -expected ".\test\ref\$action\$language\QRMath.txt"
test\bin\run_test.ps1 -json -name qrencoder-objc -action $action -language $language -inputfile ".\test\data\$language\QREncoder.m" -expected ".\test\ref\$action\$language\QREncoder.txt"
test\bin\run_test.ps1 -json -name jsonkit_h-objc -action $action -language $language -inputfile ".\test\data\$language\JSONKit.h" -expected ".\test\ref\$action\$language\JSONKit_h.txt"
test\bin\run_test.ps1 -json -name jsonkit_m-objc -action $action -language $language -inputfile ".\test\data\$language\JSONKit.m" -expected ".\test\ref\$action\$language\JSONKit_m.txt"

# OCaml
Set-Variable -Name language "OCaml"
test\bin\run_test.ps1 -json -name antwalk-ocaml -action $action -language $language -inputfile ".\test\data\$language\ant_walk.ml" -expected ".\test\ref\$action\$language\ant_walk.txt"

# Octave
Set-Variable -Name language "Octave"
test\bin\run_test.ps1 -json -name transpose-$language -action $action -language $language -inputfile ".\test\data\$language\transpose.m" -expected ".\test\ref\$action\$language\transpose.txt"
test\bin\run_test.ps1 -json -name choose-$language -action $action -language $language -inputfile ".\test\data\$language\choose.m" -expected ".\test\ref\$action\$language\choose.txt"
test\bin\run_test.ps1 -json -name fitnormal-$language -action $action -language $language -inputfile ".\test\data\$language\fitnormal.m" -expected ".\test\ref\$action\$language\fitnormal.txt"
test\bin\run_test.ps1 -json -name test_fitnormal-$language -action $action -language $language -inputfile ".\test\data\$language\test_fitnormal.m" -expected ".\test\ref\$action\$language\test_fitnormal.txt"
test\bin\run_test.ps1 -json -name tshow-$language -action $action -language $language -inputfile ".\test\data\$language\tshow.m" -expected ".\test\ref\$action\$language\tshow.txt"
test\bin\run_test.ps1 -json -name ex7_pca-$language -action $action -language $language -inputfile ".\test\data\$language\ex7_pca.m" -expected ".\test\ref\$action\$language\ex7_pca.txt"

# Pascal
Set-Variable -Name language "Pascal"
test\bin\run_test.ps1 -json -name label_declare-pas -action $action -language $language -inputfile ".\test\data\$language\LabelDeclaration.pas" -expected ".\test\ref\$action\$language\LabelDeclaration.txt"
test\bin\run_test.ps1 -json -name firework-pas -action $action -language $language -inputfile ".\test\data\$language\FIREWORK.PAS" -expected ".\test\ref\$action\$language\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello-pas -action $action -language $language -inputfile ".\test\data\$language\HELLO.PAS" -expected ".\test\ref\$action\$language\HELLO.txt"
test\bin\run_test.ps1 -json -name rose-pas -action $action -language $language -inputfile ".\test\data\$language\ROSE.PAS" -expected ".\test\ref\$action\$language\ROSE.txt"
test\bin\run_test.ps1 -json -name spider-pas -action $action -language $language -inputfile ".\test\data\$language\SPIDER.PAS" -expected ".\test\ref\$action\$language\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16-pas -action $action -language $language -inputfile ".\test\data\$language\TPC16.PAS" -expected ".\test\ref\$action\$language\TPC16.txt"

# Perl
Set-Variable -Name language "Perl"
test\bin\run_test.ps1 -json -name perligata-perl -action $action -language $language -inputfile ".\test\data\$language\Perligata.pm" -expected ".\test\ref\$action\$language\Perligata.txt"
test\bin\run_test.ps1 -json -name physics-perl -action $action -language $language -inputfile ".\test\data\$language\Physics.pm" -expected ".\test\ref\$action\$language\Physics.txt"
test\bin\run_test.ps1 -json -name zero_line-perl -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.pl" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# PL/1
Set-Variable -Name language "PL1"
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action $action -language $language -inputfile ".\test\data\$language\BPGPLI.pl1" -expected ".\test\ref\$action\$language\BPGPLI.txt"
test\bin\run_test.ps1 -json -name checkdt-pl1 -action $action -language $language -inputfile ".\test\data\$language\CHECKDT.pl1" -expected ".\test\ref\$action\$language\CHECKDT.txt"
test\bin\run_test.ps1 -json -name crtpln3-pl1 -action $action -language $language -inputfile ".\test\data\$language\CRTPLN3.pl1" -expected ".\test\ref\$action\$language\CRTPLN3.txt"
test\bin\run_test.ps1 -json -name mainfact-pl1 -action $action -language $language -inputfile ".\test\data\$language\MAINFACT.pl1" -expected ".\test\ref\$action\$language\MAINFACT.txt"
test\bin\run_test.ps1 -json -name example-pl1 -action $action -language $language -inputfile ".\test\data\$language\example.pl1" -expected ".\test\ref\$action\$language\example.txt"
test\bin\run_test.ps1 -json -name digrams-pl1 -action $action -language $language -inputfile ".\test\data\$language\digrams.pl1" -expected ".\test\ref\$action\$language\digrams.txt"

# Prolog
Set-Variable -Name language "Prolog"
test\bin\run_test.ps1 -json -name family-main-$language -action $action -language $language -inputfile ".\test\data\$language\family-main.pl" -expected ".\test\ref\$action\$language\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb-$language -action $action -language $language -inputfile ".\test\data\$language\family-kb.pl" -expected ".\test\ref\$action\$language\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu-$language -action $action -language $language -inputfile ".\test\data\$language\family-menu.pl" -expected ".\test\ref\$action\$language\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries-$language -action $action -language $language -inputfile ".\test\data\$language\family-queries.pl" -expected ".\test\ref\$action\$language\family-queries.txt"
test\bin\run_test.ps1 -json -name web-server-hello-$language -action $action -language $language -inputfile ".\test\data\$language\web-server-hello.pl" -expected ".\test\ref\$action\$language\web-server-hello.txt"
test\bin\run_test.ps1 -json -name web-server-params-$language -action $action -language $language -inputfile ".\test\data\$language\web-server-params.pl" -expected ".\test\ref\$action\$language\web-server-params.txt"

# Python
Set-Variable -Name language "Python"
test\bin\run_test.ps1 -json -name drone-3d-$language -action $action -language $language -inputfile ".\test\data\$language\drone_3d_trajectory_following.py" -expected ".\test\ref\$action\$language\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor-$language -action $action -language $language -inputfile ".\test\data\$language\Quadrotor.py" -expected ".\test\ref\$action\$language\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory-$language -action $action -language $language -inputfile ".\test\data\$language\TrajectoryGenerator.py" -expected ".\test\ref\$action\$language\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-$language -action $action -language $language -inputfile ".\test\data\$language\values.py" -expected ".\test\ref\$action\$language\values.txt"
test\bin\run_test.ps1 -json -name examiner-$language -action $action -language $language -inputfile ".\test\data\$language\Examiner.py" -expected ".\test\ref\$action\$language\Examiner.txt"
test\bin\run_test.ps1 -json -name authorized-view-$language -action $action -language $language -inputfile ".\test\data\$language\authorized_view.py" -expected ".\test\ref\$action\$language\authorized_view.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.py" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# R
Set-Variable -Name language "R"
test\bin\run_test.ps1 -json -name ETM-540-01-r -action $action -language $language -inputfile ".\test\data\$language\ETM-540-01.R" -expected ".\test\ref\$action\$language\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02-r -action $action -language $language -inputfile ".\test\data\$language\ETM-540-02.R" -expected ".\test\ref\$action\$language\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03-r -action $action -language $language -inputfile ".\test\data\$language\ETM-540-03.R" -expected ".\test\ref\$action\$language\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04-r -action $action -language $language -inputfile ".\test\data\$language\ETM-540-04.R" -expected ".\test\ref\$action\$language\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05-r -action $action -language $language -inputfile ".\test\data\$language\ETM-540-05.R" -expected ".\test\ref\$action\$language\ETM-540-05.txt"
test\bin\run_test.ps1 -json -name basketball-r -action $action -language $language -inputfile ".\test\data\$language\basketball.R" -expected ".\test\ref\$action\$language\basketball.txt"
test\bin\run_test.ps1 -json -name render-r -action $action -language $language -inputfile ".\test\data\$language\render.R" -expected ".\test\ref\$action\$language\render.txt"
test\bin\run_test.ps1 -json -name zero_line-r -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.R" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# Ruby
Set-Variable -Name language "Ruby"
test\bin\run_test.ps1 -json -name basic-$language -action $action -language $language -inputfile ".\test\data\$language\basic.rb" -expected ".\test\ref\$action\$language\basic.txt"
test\bin\run_test.ps1 -json -name constants-$language -action $action -language $language -inputfile ".\test\data\$language\constants.rb" -expected ".\test\ref\$action\$language\constants.txt"
test\bin\run_test.ps1 -json -name exceptions-$language -action $action -language $language -inputfile ".\test\data\$language\exceptions.rb" -expected ".\test\ref\$action\$language\exceptions.txt"
test\bin\run_test.ps1 -json -name expressions-$language -action $action -language $language -inputfile ".\test\data\$language\expressions.rb" -expected ".\test\ref\$action\$language\expressions.txt"
test\bin\run_test.ps1 -json -name functions-$language -action $action -language $language -inputfile ".\test\data\$language\functions.rb" -expected ".\test\ref\$action\$language\functions.txt"
test\bin\run_test.ps1 -json -name io-$language -action $action -language $language -inputfile ".\test\data\$language\io.rb" -expected ".\test\ref\$action\$language\io.txt"
test\bin\run_test.ps1 -json -name modifiers-$language -action $action -language $language -inputfile ".\test\data\$language\modifiers.rb" -expected ".\test\ref\$action\$language\modifiers.txt"
test\bin\run_test.ps1 -json -name operators-$language -action $action -language $language -inputfile ".\test\data\$language\operators.rb" -expected ".\test\ref\$action\$language\operators.txt"
test\bin\run_test.ps1 -json -name statements-$language -action $action -language $language -inputfile ".\test\data\$language\statements.rb" -expected ".\test\ref\$action\$language\statements.txt"
test\bin\run_test.ps1 -json -name tokenbuilders-$language -action $action -language $language -inputfile ".\test\data\$language\tokenbuilders.rb" -expected ".\test\ref\$action\$language\tokenbuilders.txt"
test\bin\run_test.ps1 -json -name tokenizers-$language -action $action -language $language -inputfile ".\test\data\$language\tokenizers.rb" -expected ".\test\ref\$action\$language\tokenizers.txt"
test\bin\run_test.ps1 -json -name tokens-$language -action $action -language $language -inputfile ".\test\data\$language\tokens.rb" -expected ".\test\ref\$action\$language\tokens.txt"
test\bin\run_test.ps1 -json -name webhook-$language -action $action -language $language -inputfile ".\test\data\$language\webhook.rb" -expected ".\test\ref\$action\$language\webhook.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.rb" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# Rust
Set-Variable -Name language "Rust"
test\bin\run_test.ps1 -json -name literals-$language -action $action -language $language -inputfile ".\test\data\$language\literals.rs" -expected ".\test\ref\$action\$language\literals.txt"
test\bin\run_test.ps1 -json -name dom-$language -action $action -language $language -inputfile ".\test\data\$language\dom.rs" -expected ".\test\ref\$action\$language\dom.txt"
test\bin\run_test.ps1 -json -name html-$language -action $action -language $language -inputfile ".\test\data\$language\html.rs" -expected ".\test\ref\$action\$language\html.txt"
test\bin\run_test.ps1 -json -name geometry-$language -action $action -language $language -inputfile ".\test\data\$language\geometry.rs" -expected ".\test\ref\$action\$language\geometry.txt"
test\bin\run_test.ps1 -json -name scene-$language -action $action -language $language -inputfile ".\test\data\$language\scene.rs" -expected ".\test\ref\$action\$language\scene.txt"
test\bin\run_test.ps1 -json -name comments-1-$language -action $action -language $language -inputfile ".\test\data\$language\nested_comments_1.rs" -expected ".\test\ref\$action\$language\nested_comments_1.txt"
test\bin\run_test.ps1 -json -name comments-2-$language -action $action -language $language -inputfile ".\test\data\$language\nested_comments_2.rs" -expected ".\test\ref\$action\$language\nested_comments_2.txt"
test\bin\run_test.ps1 -json -name comments-3-$language -action $action -language $language -inputfile ".\test\data\$language\nested_comments_3.rs" -expected ".\test\ref\$action\$language\nested_comments_3.txt"
test\bin\run_test.ps1 -json -name raw-string-1-$language -action $action -language $language -inputfile ".\test\data\$language\raw_string_1.rs" -expected ".\test\ref\$action\$language\raw_string_1.txt"
test\bin\run_test.ps1 -json -name raw-string-2-$language -action $action -language $language -inputfile ".\test\data\$language\raw_string_2.rs" -expected ".\test\ref\$action\$language\raw_string_2.txt"
test\bin\run_test.ps1 -json -name attributes-1-$language -action $action -language $language -inputfile ".\test\data\$language\attributes_1.rs" -expected ".\test\ref\$action\$language\attributes_1.txt"
test\bin\run_test.ps1 -json -name chip8-$language -action $action -language $language -inputfile ".\test\data\$language\chip8.rs" -expected ".\test\ref\$action\$language\chip8.txt"
test\bin\run_test.ps1 -json -name chip8-display-$language -action $action -language $language -inputfile ".\test\data\$language\chip8-display.rs" -expected ".\test\ref\$action\$language\chip8-display.txt"
test\bin\run_test.ps1 -json -name chip8-instructions-$language -action $action -language $language -inputfile ".\test\data\$language\chip8-instructions.rs" -expected ".\test\ref\$action\$language\chip8-instructions.txt"
test\bin\run_test.ps1 -json -name chip8-main-$language -action $action -language $language -inputfile ".\test\data\$language\chip8-main.rs" -expected ".\test\ref\$action\$language\chip8-main.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.rs" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# Scala
Set-Variable -Name language "Scala"
test\bin\run_test.ps1 -json -name hello-$language -action $action -language $language -inputfile ".\test\data\$language\hello.$language" -expected ".\test\ref\$action\$language\hello.txt"
test\bin\run_test.ps1 -json -name larger-$language -action $action -language $language -inputfile ".\test\data\$language\larger.$language" -expected ".\test\ref\$action\$language\larger.txt"
test\bin\run_test.ps1 -json -name random-$language -action $action -language $language -inputfile ".\test\data\$language\random.$language" -expected ".\test\ref\$action\$language\random.txt"
test\bin\run_test.ps1 -json -name variables-$language -action $action -language $language -inputfile ".\test\data\$language\variables.$language" -expected ".\test\ref\$action\$language\variables.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.$language" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# SQL
Set-Variable -Name language "SQL"
test\bin\run_test.ps1 -json -name microsoft-sql -action $action -language $language -inputfile ".\test\data\$language\microsoft.sql" -expected ".\test\ref\$action\$language\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql -action $action -language $language -inputfile ".\test\data\$language\table.sql" -expected ".\test\ref\$action\$language\table.txt"

# T-SQL
Set-Variable -Name language "T-SQL"
test\bin\run_test.ps1 -json -name brackets-tsql -action $action -language $language -inputfile ".\test\data\$language\brackets.sql" -expected ".\test\ref\$action\$language\brackets.txt"

# Swift
Set-Variable -Name language "Swift"
test\bin\run_test.ps1 -json -name AppDelegate-$language -action $action -language $language -inputfile ".\test\data\$language\AppDelegate.$language" -expected ".\test\ref\$action\$language\AppDelegate.txt"
test\bin\run_test.ps1 -json -name Meal-$language -action $action -language $language -inputfile ".\test\data\$language\Meal.$language" -expected ".\test\ref\$action\$language\Meal.txt"
test\bin\run_test.ps1 -json -name MealTableViewCell-$language -action $action -language $language -inputfile ".\test\data\$language\MealTableViewCell.$language" -expected ".\test\ref\$action\$language\MealTableViewCell.txt"
test\bin\run_test.ps1 -json -name MealTableViewController-$language -action $action -language $language -inputfile ".\test\data\$language\MealTableViewController.$language" -expected ".\test\ref\$action\$language\MealTableViewController.txt"
test\bin\run_test.ps1 -json -name MealViewController-$language -action $action -language $language -inputfile ".\test\data\$language\MealViewController.$language" -expected ".\test\ref\$action\$language\MealViewController.txt"
test\bin\run_test.ps1 -json -name RatingControl-$language -action $action -language $language -inputfile ".\test\data\$language\RatingControl.$language" -expected ".\test\ref\$action\$language\RatingControl.txt"
test\bin\run_test.ps1 -json -name URLExtensions-$language -action $action -language $language -inputfile ".\test\data\$language\URLExtensions.$language" -expected ".\test\ref\$action\$language\URLExtensions.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -language $language -inputfile ".\test\data\$language\zero_line_hello_world.$language" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"

# TypeScript
Set-Variable -Name language "TypeScript"
test\bin\run_test.ps1 -json -name TimeReporter-ts -action $action -language $language -inputfile ".\test\data\$language\TimeReporter.ts" -expected ".\test\ref\$action\$language\TimeReporter.txt"
test\bin\run_test.ps1 -json -name ImageBoard-ts -action $action -language $language -inputfile ".\test\data\$language\ImageBoard.ts" -expected ".\test\ref\$action\$language\ImageBoard.txt"

# Visual Basic 6
Set-Variable -Name language "VisualBasic-6"
test\bin\run_test.ps1 -json -name spider-vb6 -action $action -language $language -inputfile ".\test\data\$language\spider.bas" -expected ".\test\ref\$action\$language\spider.txt"
test\bin\run_test.ps1 -json -name azure_metadata-vb6 -action $action -language $language -inputfile ".\test\data\$language\azure_metadata.bas" -expected ".\test\ref\$action\$language\azure_metadata.txt"
test\bin\run_test.ps1 -json -name diffie_hellman-vb6 -action $action -language $language -inputfile ".\test\data\$language\diffie_hellman.bas" -expected ".\test\ref\$action\$language\diffie_hellman.txt"

# Visual Basic .NET
Set-Variable -Name language "VisualBasic-NET"
test\bin\run_test.ps1 -json -name word-processor-vbnet -action $action -language $language -inputfile ".\test\data\$language\WordProcessor.bas" -expected ".\test\ref\$action\$language\WordProcessor.txt"

# Polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c-78 -action $action -language C-78 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\C-78\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action $action -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action $action -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\Pascal\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-python -action $action -language Python -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action\Python\polyglot-py-rb.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-ruby -action $action -language Ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action\Ruby\polyglot-py-rb.txt"
