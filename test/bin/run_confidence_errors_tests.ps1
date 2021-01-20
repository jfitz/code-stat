Set-StrictMode -Version Latest

Set-Variable -Name action "confidence"

# generic
test\bin\run_test.ps1 -json -name adabkend-adb -action $action -errors -language generic -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\$action-errors\generic\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action $action -errors -language generic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\$action-errors\generic\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action $action -errors -language generic -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\$action-errors\generic\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action $action -errors -language generic -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\$action-errors\generic\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action $action -errors -language generic -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\$action-errors\generic\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action $action -errors -language generic -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\$action-errors\generic\heron.txt"
test\bin\run_test.ps1 -json -name bing -action $action -errors -language generic -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\$action-errors\generic\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action $action -errors -language generic -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\$action-errors\generic\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action $action -errors -language generic -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\$action-errors\generic\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action $action -errors -language generic -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\$action-errors\generic\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action $action -errors -language generic -inputfile ".\test\data\SQL\microsoft.sql" -expected ".\test\ref\$action-errors\generic\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action $action -errors -language generic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\$action-errors\generic\spider.txt"

# generic with comments
test\bin\run_test.ps1 -json -name adabkend-adb -action $action -errors -language generic -comment ada -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\$action-errors\generic-comments\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name rockt2 -action $action -errors -language generic -comment basic -inputfile ".\test\data\BASIC\rockt2.bas" -expected ".\test\ref\$action-errors\generic-comments\rockt2.txt"
test\bin\run_test.ps1 -json -name parser -action $action -errors -language generic -comment c -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\$action-errors\generic-comments\parser.txt"
test\bin\run_test.ps1 -json -name PROG13-3 -action $action -errors -language generic -comment cobol -inputfile ".\test\data\COBOL-85\PROG13-3.cob" -expected ".\test\ref\$action-errors\generic-comments\prog13-3.txt"
test\bin\run_test.ps1 -json -name calculator3 -action $action -errors -language generic -comment cpp -inputfile ".\test\data\Csharp\calculator3.cs" -expected ".\test\ref\$action-errors\generic-comments\calculator3.txt"
test\bin\run_test.ps1 -json -name heron -action $action -errors -language generic -comment fortran -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\$action-errors\generic-comments\heron.txt"
test\bin\run_test.ps1 -json -name bing -action $action -errors -language generic -comment hash -inputfile ".\test\data\JavaScript\bing.js" -expected ".\test\ref\$action-errors\generic-comments\bing.txt"
test\bin\run_test.ps1 -json -name family-main -action $action -errors -language generic -comment percent -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\$action-errors\generic-comments\family-main.txt"
test\bin\run_test.ps1 -json -name ETM-540-05 -action $action -errors -language generic -comment hash -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\$action-errors\generic-comments\etm-540-05.txt"
test\bin\run_test.ps1 -json -name basic -action $action -errors -language generic -comment hash -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\$action-errors\generic-comments\basic.txt"
test\bin\run_test.ps1 -json -name microsoft -action $action -errors -language generic -comment ada -inputfile ".\test\data\SQL\microsoft.sql" -expected ".\test\ref\$action-errors\generic-comments\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action $action -errors -language generic -comment basic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\$action-errors\generic-comments\spider.txt"

# generic assembly

# ASM IBM-360
Set-Variable -Name language "ASM-360"
test\bin\run_test.ps1 -json -name subroutine1-asm -action $action -errors -language $language -inputfile ".\test\data\$language\subroutine1.asm" -expected ".\test\ref\$action-errors\$language\subroutine1.txt"
test\bin\run_test.ps1 -json -name subroutine1_a-asm -action $action -errors -language $language -inputfile ".\test\data\$language\subroutine1_a.asm" -expected ".\test\ref\$action-errors\$language\subroutine1_a.txt"
test\bin\run_test.ps1 -json -name subroutine2-asm -action $action -errors -language $language -inputfile ".\test\data\$language\subroutine2.asm" -expected ".\test\ref\$action-errors\$language\subroutine2.txt"
test\bin\run_test.ps1 -json -name subroutine2_a-asm -action $action -errors -language $language -inputfile ".\test\data\$language\subroutine2_a.asm" -expected ".\test\ref\$action-errors\$language\subroutine2_a.txt"
test\bin\run_test.ps1 -json -name spitbol-asm -action $action -errors -language $language -inputfile ".\test\data\$language\spitbol.asm" -expected ".\test\ref\$action-errors\$language\spitbol.txt"

# ASM IBM-370
Set-Variable -Name language "ASM-370"
test\bin\run_test.ps1 -json -name asm370a1-asm -action $action -errors -language $language -inputfile ".\test\data\$language\asm370a1.asm" -expected ".\test\ref\$action-errors\$language\asm370a1.txt"

# ASM IBM-390
Set-Variable -Name language "ASM-390"
test\bin\run_test.ps1 -json -name osint-asm -action $action -errors -language $language -inputfile ".\test\data\$language\osint.asm" -expected ".\test\ref\$action-errors\$language\osint.txt"

# ASM IBM-system-z

# ASM 1802
Set-Variable -Name language "ASM-1802"
test\bin\run_test.ps1 -json -name test_alu-asm -action $action -errors -language $language -inputfile ".\test\data\$language\test_alu.asm" -expected ".\test\ref\$action-errors\$language\test_alu.txt"

# ASM 6502
Set-Variable -Name language "ASM-6502"
test\bin\run_test.ps1 -json -name sweet16-asm -action $action -errors -language $language -inputfile ".\test\data\$language\sweet16.asm" -expected ".\test\ref\$action-errors\$language\sweet16.txt"

# ASM 6800
Set-Variable -Name language "ASM-6800"
test\bin\run_test.ps1 -json -name ET3400-S6-asm -action $action -errors -language $language -inputfile ".\test\data\$language\ET3400-S6.asm" -expected ".\test\ref\$action-errors\$language\ET3400-S6.txt"

# ASM 68000
Set-Variable -Name language "ASM-68000"
test\bin\run_test.ps1 -json -name basic68k-asm -action $action -errors -language $language -inputfile ".\test\data\$language\basic68k.asm" -expected ".\test\ref\$action-errors\$language\basic68k.txt"
test\bin\run_test.ps1 -json -name sprites-asm -action $action -errors -language $language -inputfile ".\test\data\$language\sprites.asm" -expected ".\test\ref\$action-errors\$language\sprites.txt"

# ASM 8080
Set-Variable -Name language "ASM-8080"
test\bin\run_test.ps1 -json -name i8080core-asm -action $action -errors -language $language -inputfile ".\test\data\$language\i8080core.asm" -expected ".\test\ref\$action-errors\$language\i8080core.txt"
test\bin\run_test.ps1 -json -name microcosm-asm -action $action -errors -language $language -inputfile ".\test\data\$language\microcosm.asm" -expected ".\test\ref\$action-errors\$language\microcosm.txt"
test\bin\run_test.ps1 -json -name hello_free-asm -action $action -errors -language $language -inputfile ".\test\data\$language\hello_free.asm" -expected ".\test\ref\$action-errors\$language\hello_free.txt"
test\bin\run_test.ps1 -json -name hello_spaced-asm -action $action -errors -language $language -inputfile ".\test\data\$language\hello_spaced.asm" -expected ".\test\ref\$action-errors\$language\hello_spaced.txt"

# ASM Z-80
Set-Variable -Name language "ASM-Z-80"
test\bin\run_test.ps1 -json -name intro-asm -action $action -errors -language $language -inputfile ".\test\data\$language\intro.asm" -expected ".\test\ref\$action-errors\$language\intro.txt"
test\bin\run_test.ps1 -json -name hardmess-asm -action $action -errors -language $language -inputfile ".\test\data\$language\hardmess.asm" -expected ".\test\ref\$action-errors\$language\hardmess.txt"
test\bin\run_test.ps1 -json -name shftspr-asm -action $action -errors -language $language -inputfile ".\test\data\$language\shftspr.asm" -expected ".\test\ref\$action-errors\$language\shftspr.txt"
test\bin\run_test.ps1 -json -name sincos-asm -action $action -errors -language $language -inputfile ".\test\data\$language\sincos.asm" -expected ".\test\ref\$action-errors\$language\sincos.txt"

# ASM 8086
Set-Variable -Name language "ASM-8086"
test\bin\run_test.ps1 -json -name BISTRS-asm -action $action -errors -language $language -inputfile ".\test\data\$language\BISTRS.asm" -expected ".\test\ref\$action-errors\$language\BISTRS.txt"
test\bin\run_test.ps1 -json -name DSKCOM-asm -action $action -errors -language $language -inputfile ".\test\data\$language\DSKCOM.asm" -expected ".\test\ref\$action-errors\$language\DSKCOM.txt"
test\bin\run_test.ps1 -json -name FIVEO-asm -action $action -errors -language $language -inputfile ".\test\data\$language\FIVEO.asm" -expected ".\test\ref\$action-errors\$language\FIVEO.txt"
test\bin\run_test.ps1 -json -name GWEVAL-asm -action $action -errors -language $language -inputfile ".\test\data\$language\GWEVAL.asm" -expected ".\test\ref\$action-errors\$language\GWEVAL.txt"
test\bin\run_test.ps1 -json -name NEXT86-asm -action $action -errors -language $language -inputfile ".\test\data\$language\NEXT86.asm" -expected ".\test\ref\$action-errors\$language\NEXT86.txt"

# $language
Set-Variable -Name language "ASM-80386"
test\bin\run_test.ps1 -json -name reverser-asm -action $action -errors -language $language -inputfile ".\test\data\$language\reverser.asm" -expected ".\test\ref\$action-errors\$language\reverser.txt"
test\bin\run_test.ps1 -json -name crc16-asm -action $action -errors -language $language -inputfile ".\test\data\$language\crc16.asm" -expected ".\test\ref\$action-errors\$language\crc16.txt"
test\bin\run_test.ps1 -json -name mat_get_element-asm -action $action -errors -language $language -inputfile ".\test\data\$language\mat_get_element.asm" -expected ".\test\ref\$action-errors\$language\mat_get_element.txt"

# ASM-80486

# ASM PDP-8
Set-Variable -Name language "ASM-PDP-8"
test\bin\run_test.ps1 -json -name io-asm -action $action -errors -language $language -inputfile ".\test\data\$language\io.asm" -expected ".\test\ref\$action-errors\$language\io.txt"
test\bin\run_test.ps1 -json -name life-asm -action $action -errors -language $language -inputfile ".\test\data\$language\life.asm" -expected ".\test\ref\$action-errors\$language\life.txt"
test\bin\run_test.ps1 -json -name sum-asm -action $action -errors -language $language -inputfile ".\test\data\$language\sum.asm" -expected ".\test\ref\$action-errors\$language\sum.txt"

# ASM PDP-11
Set-Variable -Name language "ASM-PDP-11"
test\bin\run_test.ps1 -json -name print-asm -action $action -errors -language $language -inputfile ".\test\data\$language\print.asm" -expected ".\test\ref\$action-errors\$language\print.txt"
test\bin\run_test.ps1 -json -name k11tsx-asm -action $action -errors -language $language -inputfile ".\test\data\$language\k11tsx.mac" -expected ".\test\ref\$action-errors\$language\k11tsx.txt"
test\bin\run_test.ps1 -json -name krtser-asm -action $action -errors -language $language -inputfile ".\test\data\$language\krtser.mac" -expected ".\test\ref\$action-errors\$language\krtser.txt"
test\bin\run_test.ps1 -json -name krtsub-asm -action $action -errors -language $language -inputfile ".\test\data\$language\krtsub.mac" -expected ".\test\ref\$action-errors\$language\krtsub.txt"

# Ada 83
Set-Variable -Name language "Ada-83"
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action $action -errors -language $language -inputfile ".\test\data\$language\adabkend.adb" -expected ".\test\ref\$action-errors\$language\adabkend-adb.txt"
test\bin\run_test.ps1 -json -name adabkend-ads-83 -action $action -errors -language $language -inputfile ".\test\data\$language\adabkend.ads" -expected ".\test\ref\$action-errors\$language\adabkend-ads.txt"
test\bin\run_test.ps1 -json -name signup-adb-83 -action $action -errors -language $language -inputfile ".\test\data\$language\signup.adb" -expected ".\test\ref\$action-errors\$language\signup-adb.txt"

# Ada 95
Set-Variable -Name language "Ada-95"
test\bin\run_test.ps1 -json -name philosophers-ads-95 -action $action -errors -language $language -inputfile ".\test\data\$language\philosophers.ads" -expected ".\test\ref\$action-errors\$language\philosophers-ads.txt"

# Ada-2005

# Ada-2012

# Awk
Set-Variable -Name language "Awk"
test\bin\run_test.ps1 -json -name funstack-awk -action $action -errors -language $language -inputfile ".\test\data\$language\funstack.awk" -expected ".\test\ref\$action-errors\$language\funstack.txt"
test\bin\run_test.ps1 -json -name awkaster-awk -action $action -errors -language $language -inputfile ".\test\data\$language\awkaster.awk" -expected ".\test\ref\$action-errors\$language\awkaster.txt"

# BASIC
Set-Variable -Name language "BASIC"
test\bin\run_test.ps1 -json -name values-bas -action $action -errors -language $language -inputfile ".\test\data\$language\values.bas" -expected ".\test\ref\$action-errors\$language\values.txt"
test\bin\run_test.ps1 -json -name simple-bas -action $action -errors -language $language -inputfile ".\test\data\$language\simple.bas" -expected ".\test\ref\$action-errors\$language\simple.txt"
test\bin\run_test.ps1 -json -name 3dplot-bas -action $action -errors -language $language -inputfile ".\test\data\$language\3dplot.bas" -expected ".\test\ref\$action-errors\$language\3dplot.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action $action -errors -language $language -inputfile ".\test\data\$language\batnum.bas" -expected ".\test\ref\$action-errors\$language\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action $action -errors -language $language -inputfile ".\test\data\$language\life.bas" -expected ".\test\ref\$action-errors\$language\life.txt"
test\bin\run_test.ps1 -json -name income-bas -action $action -errors -language $language -inputfile ".\test\data\$language\income.bas" -expected ".\test\ref\$action-errors\$language\income.txt"
test\bin\run_test.ps1 -json -name rockt2-bas -action $action -errors -language $language -inputfile ".\test\data\$language\rockt2.bas" -expected ".\test\ref\$action-errors\$language\rockt2.txt"

# BASIC-80
Set-Variable -Name language "BASIC-80"
test\bin\run_test.ps1 -json -name doctor-mbasic -action $action -errors -language $language -inputfile ".\test\data\BASIC\doctor.bas" -expected ".\test\ref\$action-errors\$language\doctor.txt"
test\bin\run_test.ps1 -json -name airinput-mbasic -action $action -errors -language $language -inputfile ".\test\data\BASIC\AIRINPUT.bas" -expected ".\test\ref\$action-errors\$language\AIRINPUT.txt"
test\bin\run_test.ps1 -json -name backgamm-mbasic -action $action -errors -language $language -inputfile ".\test\data\BASIC\BACKGAMM.bas" -expected ".\test\ref\$action-errors\$language\BACKGAMM.txt"
test\bin\run_test.ps1 -json -name planes-mbasic -action $action -errors -language $language -inputfile ".\test\data\BASIC\PLANES.bas" -expected ".\test\ref\$action-errors\$language\PLANES.txt"

# BASICA
Set-Variable -Name language "BASICA"
test\bin\run_test.ps1 -json -name sea-creature-basica -action $action -errors -language $language -inputfile ".\test\data\BASIC\sea_creature.bas" -expected ".\test\ref\$action-errors\$language\sea_creature.txt"
test\bin\run_test.ps1 -json -name lanturn-basica -action $action -errors -language $language -inputfile ".\test\data\BASIC\lanturn.bas" -expected ".\test\ref\$action-errors\$language\lanturn.txt"
test\bin\run_test.ps1 -json -name gw3d-basica -action $action -errors -language $language -inputfile ".\test\data\BASIC\GW3D.bas" -expected ".\test\ref\$action-errors\$language\GW3D.txt"

# C-78
Set-Variable -Name language "C-78"
test\bin\run_test.ps1 -json -name diamond-$language -action $action -errors -language $language -inputfile ".\test\data\$language\diamond.c" -expected ".\test\ref\$action-errors\$language\diamond.txt"
test\bin\run_test.ps1 -json -name prime_test-$language -action $action -errors -language $language -inputfile ".\test\data\$language\prime_test.c" -expected ".\test\ref\$action-errors\$language\prime_test.txt"
test\bin\run_test.ps1 -json -name values-$language -action $action -errors -language $language -inputfile ".\test\data\$language\values.c" -expected ".\test\ref\$action-errors\$language\values.txt"
test\bin\run_test.ps1 -json -name j_interpreter-$language -action $action -errors -language $language -inputfile ".\test\data\$language\j_interpreter.c" -expected ".\test\ref\$action-errors\$language\j_interpreter.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.c" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# C-89

# C-99
Set-Variable -Name language "C-99"
test\bin\run_test.ps1 -json -name decl-c-99 -action $action -errors -language $language -inputfile ".\test\data\$language\c-decl.c" -expected ".\test\ref\$action-errors\$language\c-decl-c.txt"
test\bin\run_test.ps1 -json -name parser-h-99 -action $action -errors -language $language -inputfile ".\test\data\$language\parser.h" -expected ".\test\ref\$action-errors\$language\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c-99 -action $action -errors -language $language -inputfile ".\test\data\$language\parser.c" -expected ".\test\ref\$action-errors\$language\parser-c.txt"

# CBASIC
Set-Variable -Name language "CBASIC"
test\bin\run_test.ps1 -json -name fibo -action $action -errors -language $language -inputfile ".\test\data\$language\fibo.bas" -expected ".\test\ref\$action-errors\$language\fibo.txt"
test\bin\run_test.ps1 -json -name demograf -action $action -errors -language $language -inputfile ".\test\data\$language\DEMOGRAF.BAS" -expected ".\test\ref\$action-errors\$language\DEMOGRAF.txt"
test\bin\run_test.ps1 -json -name graphr -action $action -errors -language $language -inputfile ".\test\data\$language\GRAPHR.BAS" -expected ".\test\ref\$action-errors\$language\GRAPHR.txt"

# COBOL-68
Set-Variable -Name language "COBOL-68"
test\bin\run_test.ps1 -json -name mccracken1-68 -action $action -errors -language $language -inputfile ".\test\data\$language\mccracken1.cob" -expected ".\test\ref\$action-errors\$language\mccracken1.txt"
test\bin\run_test.ps1 -json -name mccracken2-68 -action $action -errors -language $language -inputfile ".\test\data\$language\mccracken2.cob" -expected ".\test\ref\$action-errors\$language\mccracken2.txt"
test\bin\run_test.ps1 -json -name mccracken3-68 -action $action -errors -language $language -inputfile ".\test\data\$language\mccracken3.cob" -expected ".\test\ref\$action-errors\$language\mccracken3.txt"
test\bin\run_test.ps1 -json -name prog1-68 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG1.COB" -expected ".\test\ref\$action-errors\$language\PROG1.txt"
test\bin\run_test.ps1 -json -name prog2-68 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG2.COB" -expected ".\test\ref\$action-errors\$language\PROG2.txt"
test\bin\run_test.ps1 -json -name prog2A-68 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG2A.COB" -expected ".\test\ref\$action-errors\$language\PROG2A.txt"

# COBOL-74

# COBOL-85
Set-Variable -Name language "COBOL-85"
test\bin\run_test.ps1 -json -name prog3-85 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG3.COB" -expected ".\test\ref\$action-errors\$language\PROG3.txt"
test\bin\run_test.ps1 -json -name prog4-85 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG4.COB" -expected ".\test\ref\$action-errors\$language\PROG4.txt"
test\bin\run_test.ps1 -json -name prog5-85 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG5.COB" -expected ".\test\ref\$action-errors\$language\PROG5.txt"
test\bin\run_test.ps1 -json -name prog6-85 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG6.COB" -expected ".\test\ref\$action-errors\$language\PROG6.txt"
test\bin\run_test.ps1 -json -name prog12-2-85 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG12-2.COB" -expected ".\test\ref\$action-errors\$language\PROG12-2.txt"
test\bin\run_test.ps1 -json -name prog13-3-85 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG13-3.COB" -expected ".\test\ref\$action-errors\$language\PROG13-3.txt"
test\bin\run_test.ps1 -json -name prog14-2-85 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG14-2.COB" -expected ".\test\ref\$action-errors\$language\PROG14-2.txt"
test\bin\run_test.ps1 -json -name prog15-4-85 -action $action -errors -language $language -inputfile ".\test\data\$language\PROG15-4.COB" -expected ".\test\ref\$action-errors\$language\PROG15-4.txt"
test\bin\run_test.ps1 -json -name exec1-85 -action $action -errors -language $language -tabsize 4 -inputfile ".\test\data\$language\UNLDDBCU2.COB" -expected ".\test\ref\$action-errors\$language\UNLDDBCU2.txt"
test\bin\run_test.ps1 -json -name P010-85 -action $action -errors -language $language -inputfile ".\test\data\$language\P010.cob" -expected ".\test\ref\$action-errors\$language\P010.txt"

# COBOL-2002
Set-Variable -Name language "COBOL-2002"
test\bin\run_test.ps1 -json -name report-card-cob2002 -action $action -errors -language $language -tabsize 4 -inputfile ".\test\data\$language\ReportCard.cob" -expected ".\test\ref\$action-errors\$language\ReportCard.txt"
test\bin\run_test.ps1 -json -name person-cob2002 -action $action -errors -language $language -tabsize 4 -inputfile ".\test\data\$language\Person.cob" -expected ".\test\ref\$action-errors\$language\Person.txt"
test\bin\run_test.ps1 -json -name report-cob2002 -action $action -errors -language $language -tabsize 4 -inputfile ".\test\data\$language\Report.cob" -expected ".\test\ref\$action-errors\$language\Report.txt"
test\bin\run_test.ps1 -json -name advent-of-code-cob2002 -action $action -errors -language $language -inputfile ".\test\data\$language\AdventOfCode.cob" -expected ".\test\ref\$action-errors\$language\AdventOfCode.txt"
test\bin\run_test.ps1 -json -name P010-wide-2002 -action $action -errors -language $language -inputfile ".\test\data\$language\P010-wide.COB" -wide -expected ".\test\ref\$action-errors\$language\P010-wide.txt"

# COBOL-2014

# COBOL-2014 with AcuCobol extensions

# COBOL-2014 with IBM extensions

# COBOL-2014 with GNU extensions
Set-Variable -Name language "COBOL-2014-GNU"
test\bin\run_test.ps1 -json -name sales-report-cob2014-GNU -action $action -errors -language $language -tabsize 4 -inputfile ".\test\data\$language\SalesReport.cob" -expected ".\test\ref\$action-errors\$language\SalesReport.txt"

# C++
Set-Variable -Name language "Cplusplus"
test\bin\run_test.ps1 -json -name checkers-cpp -action $action -errors -language $language -inputfile ".\test\data\$language\checkers.cpp" -expected ".\test\ref\$action-errors\$language\checkers.txt"
test\bin\run_test.ps1 -json -name hrdb-cpp -action $action -errors -language $language -inputfile ".\test\data\$language\hrdb.cpp" -expected ".\test\ref\$action-errors\$language\hrdb.txt"
test\bin\run_test.ps1 -json -name date-h-cpp -action $action -errors -language $language -inputfile ".\test\data\$language\date.h" -expected ".\test\ref\$action-errors\$language\date_h.txt"
test\bin\run_test.ps1 -json -name date-cpp -action $action -errors -language $language -inputfile ".\test\data\$language\date.cpp" -expected ".\test\ref\$action-errors\$language\date_cpp.txt"
test\bin\run_test.ps1 -json -name inherit-cpp -action $action -errors -language $language -inputfile ".\test\data\$language\inherit.cpp" -expected ".\test\ref\$action-errors\$language\inherit.txt"
test\bin\run_test.ps1 -json -name zero_line-cpp -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.cpp" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# C#
Set-Variable -Name language "Csharp"
test\bin\run_test.ps1 -json -name calculator1-cs -action $action -errors -language $language -inputfile ".\test\data\$language\calculator1.cs" -expected ".\test\ref\$action-errors\$language\calculator1.txt"
test\bin\run_test.ps1 -json -name calculator2-cs -action $action -errors -language $language -inputfile ".\test\data\$language\calculator2.cs" -expected ".\test\ref\$action-errors\$language\calculator2.txt"
test\bin\run_test.ps1 -json -name calculator3-cs -action $action -errors -language $language -inputfile ".\test\data\$language\calculator3.cs" -expected ".\test\ref\$action-errors\$language\calculator3.txt"
test\bin\run_test.ps1 -json -name zero_line-cs -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.cs" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# CoffeeScript
Set-Variable -Name language "CoffeeScript"
test\bin\run_test.ps1 -json -name provider-coffee -action $action -errors -language $language -inputfile ".\test\data\$language\provider.coffee" -expected ".\test\ref\$action-errors\$language\provider.txt"
test\bin\run_test.ps1 -json -name resque-coffee -action $action -errors -language $language -inputfile ".\test\data\$language\resque.coffee" -expected ".\test\ref\$action-errors\$language\resque.txt"
test\bin\run_test.ps1 -json -name world-map-coffee -action $action -errors -language $language -inputfile ".\test\data\$language\world_map.coffee" -expected ".\test\ref\$action-errors\$language\world_map.txt"

# D
Set-Variable -Name language "D"
test\bin\run_test.ps1 -json -name regex-d -action $action -errors -language $language -inputfile ".\test\data\d\regex.d" -expected ".\test\ref\$action-errors\$language\regex.txt"
test\bin\run_test.ps1 -json -name halffloat-d -action $action -errors -language $language -inputfile ".\test\data\d\halffloat.d" -expected ".\test\ref\$action-errors\$language\halffloat.txt"
test\bin\run_test.ps1 -json -name wc-d -action $action -errors -language $language -inputfile ".\test\data\d\wc.d" -expected ".\test\ref\$action-errors\$language\wc.txt"

# Dart
Set-Variable -Name language "Dart"
test\bin\run_test.ps1 -json -name anagram-dart -action $action -errors -language $language -inputfile ".\test\data\$language\anagram.dart" -expected ".\test\ref\$action-errors\$language\anagram.txt"
test\bin\run_test.ps1 -json -name note_client-dart -action $action -errors -language $language -inputfile ".\test\data\$language\note_client.dart" -expected ".\test\ref\$action-errors\$language\note_client.txt"
test\bin\run_test.ps1 -json -name web_app-dart -action $action -errors -language $language -inputfile ".\test\data\$language\web_app.dart" -expected ".\test\ref\$action-errors\$language\web_app.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.dart" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# dbase II
Set-Variable -Name language "dbase-ii"
test\bin\run_test.ps1 -json -name sample-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\sample.prg" -expected ".\test\ref\$action-errors\$language\sample.txt"
test\bin\run_test.ps1 -json -name addm-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\ADDM.PRG" -expected ".\test\ref\$action-errors\$language\ADDM.txt"
test\bin\run_test.ps1 -json -name changedm-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\CHANGEDM.PRG" -expected ".\test\ref\$action-errors\$language\CHANGEDM.txt"
test\bin\run_test.ps1 -json -name dater-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\DATER.PRG" -expected ".\test\ref\$action-errors\$language\DATER.txt"
test\bin\run_test.ps1 -json -name updatedm-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\UPDATEDM.PRG" -expected ".\test\ref\$action-errors\$language\UPDATEDM.txt"
test\bin\run_test.ps1 -json -name viewdm-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\VIEWDM.PRG" -expected ".\test\ref\$action-errors\$language\VIEWDM.txt"
test\bin\run_test.ps1 -json -name emain-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\EMAIN.PRG" -expected ".\test\ref\$action-errors\$language\EMAIN.txt"
test\bin\run_test.ps1 -json -name emp-entr-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\EMP-ENTR.PRG" -expected ".\test\ref\$action-errors\$language\EMP-ENTR.txt"
test\bin\run_test.ps1 -json -name emp-rprt-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\EMP-RPRT.PRG" -expected ".\test\ref\$action-errors\$language\EMP-RPRT.txt"
test\bin\run_test.ps1 -json -name emp-term-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\EMP-TERM.PRG" -expected ".\test\ref\$action-errors\$language\EMP-TERM.txt"
test\bin\run_test.ps1 -json -name emp-upd-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\EMP-UPD.PRG" -expected ".\test\ref\$action-errors\$language\EMP-UPD.txt"
test\bin\run_test.ps1 -json -name invmaint-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\INVMAINT.PRG" -expected ".\test\ref\$action-errors\$language\INVMAINT.txt"
test\bin\run_test.ps1 -json -name invquan-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\INVQUAN.PRG" -expected ".\test\ref\$action-errors\$language\INVQUAN.txt"
test\bin\run_test.ps1 -json -name invread-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\INVREAD.PRG" -expected ".\test\ref\$action-errors\$language\INVREAD.txt"
test\bin\run_test.ps1 -json -name invrprt-dbii -action $action -errors -language $language -inputfile ".\test\data\$language\INVRPRT.PRG" -expected ".\test\ref\$action-errors\$language\INVRPRT.txt"

# dBase III
Set-Variable -Name language "dbase-III"
test\bin\run_test.ps1 -json -name fdate-dbiii -action $action -errors -language $language -inputfile ".\test\data\$language\FDATE.PRG" -expected ".\test\ref\$action-errors\$language\FDATE.txt"
test\bin\run_test.ps1 -json -name library-dbiii -action $action -errors -language $language -inputfile ".\test\data\$language\LIBRARY.PRG" -expected ".\test\ref\$action-errors\$language\LIBRARY.txt"
test\bin\run_test.ps1 -json -name dp_sort-dbiii -action $action -errors -language $language -inputfile ".\test\data\$language\DP_SORT.PRG" -expected ".\test\ref\$action-errors\$language\DP_SORT.txt"

# Delphi
Set-Variable -Name language "Delphi"
test\bin\run_test.ps1 -json -name FmMain-dfm-delphi -action $action -errors -language $language -inputfile ".\test\data\$language\FmMain.dfm" -expected ".\test\ref\$action-errors\$language\FmMain-dfm.txt"
test\bin\run_test.ps1 -json -name FmMain-pas-delphi -action $action -errors -language $language -inputfile ".\test\data\$language\FmMain.pas" -expected ".\test\ref\$action-errors\$language\FmMain-pas.txt"
test\bin\run_test.ps1 -json -name UCalc-delphi -action $action -errors -language $language -inputfile ".\test\data\$language\UCalc.pas" -expected ".\test\ref\$action-errors\$language\UCalc.txt"
test\bin\run_test.ps1 -json -name UChessBoardCmp-delphi -action $action -errors -language $language -inputfile ".\test\data\$language\UChessBoardCmp.pas" -expected ".\test\ref\$action-errors\$language\UChessBoardCmp.txt"
test\bin\run_test.ps1 -json -name UPlatform-delphi -action $action -errors -language $language -inputfile ".\test\data\$language\UPlatform.pas" -expected ".\test\ref\$action-errors\$language\UPlatform.txt"

# Dibol
$language= "Dibol"
test\bin\run_test.ps1 -json -name bottles-dibol -action $action -errors -language $language -inputfile ".\test\data\$language\bottles.dbl" -expected ".\test\ref\$action-errors\$language\bottles.txt"

# Eiffel
Set-Variable -Name language "Eiffel"
test\bin\run_test.ps1 -json -name hello-eiffel -action $action -errors -language $language -inputfile ".\test\data\$language\hello.e" -expected ".\test\ref\$action-errors\$language\hello.txt"
test\bin\run_test.ps1 -json -name bakery-eiffel -action $action -errors -language $language -inputfile ".\test\data\$language\bakery.e" -expected ".\test\ref\$action-errors\$language\bakery.txt"
test\bin\run_test.ps1 -json -name reverse-eiffel -action $action -errors -language $language -inputfile ".\test\data\$language\reverse.e" -expected ".\test\ref\$action-errors\$language\reverse.txt"
test\bin\run_test.ps1 -json -name bottles-eiffel -action $action -errors -language $language -inputfile ".\test\data\$language\bottles_of_beer.e" -expected ".\test\ref\$action-errors\$language\bottles_of_beer.txt"

# Erlang
Set-Variable -Name language "Erlang"
test\bin\run_test.ps1 -json -name armstrong-erlang -action $action -errors -language $language -inputfile ".\test\data\$language\armstrong.erl" -expected ".\test\ref\$action-errors\$language\armstrong.txt"
test\bin\run_test.ps1 -json -name list_comprehension-erlang -action $action -errors -language $language -inputfile ".\test\data\$language\list_comprehension.erl" -expected ".\test\ref\$action-errors\$language\list_comprehension.txt"
test\bin\run_test.ps1 -json -name send_receive-erlang -action $action -errors -language $language -inputfile ".\test\data\$language\send_receive.erl" -expected ".\test\ref\$action-errors\$language\send_receive.txt"

# FORTRAN-66
Set-Variable -Name language "FORTRAN-66"
test\bin\run_test.ps1 -json -name hello-ftn66 -action $action -errors -language $language -inputfile ".\test\data\$language\HELLO.FOR" -expected ".\test\ref\$action-errors\$language\HELLO.txt"
test\bin\run_test.ps1 -json -name heron-ftn66 -action $action -errors -language $language -inputfile ".\test\data\$language\HERON.FOR" -expected ".\test\ref\$action-errors\$language\HERON.txt"
test\bin\run_test.ps1 -json -name heron2-ftn66 -action $action -errors -language $language -inputfile ".\test\data\$language\HERON2.FOR" -expected ".\test\ref\$action-errors\$language\HERON2.txt"
test\bin\run_test.ps1 -json -name heron-wide-ftn66 -action $action -errors -language $language -inputfile ".\test\data\$language\HERON-wide.FOR" -wide -expected ".\test\ref\$action-errors\$language\HERON-wide.txt"

# FORTRAN-77
Set-Variable -Name language "FORTRAN-77"
test\bin\run_test.ps1 -json -name hello-ftn77 -action $action -errors -language $language -inputfile ".\test\data\$language\HELLO.F77" -expected ".\test\ref\$action-errors\$language\HELLO.txt"
test\bin\run_test.ps1 -json -name complex-ftn77 -action $action -errors -language $language -inputfile ".\test\data\$language\COMPLEX.F77" -expected ".\test\ref\$action-errors\$language\COMPLEX.txt"
test\bin\run_test.ps1 -json -name euclid-ftn77 -action $action -errors -language $language -inputfile ".\test\data\$language\EUCLID.F77" -expected ".\test\ref\$action-errors\$language\EUCLID.txt"
test\bin\run_test.ps1 -json -name heron-ftn77 -action $action -errors -language $language -inputfile ".\test\data\$language\HERON.F77" -expected ".\test\ref\$action-errors\$language\HERON.txt"

# Fortran-90
Set-Variable -Name language "Fortran-90"
test\bin\run_test.ps1 -json -name cylinder-ftn90 -action $action -errors -language $language -inputfile ".\test\data\$language\cylinder.f90" -expected ".\test\ref\$action-errors\$language\cylinder.txt"
test\bin\run_test.ps1 -json -name gauss-ftn90 -action $action -errors -language $language -inputfile ".\test\data\$language\gauss.f90" -expected ".\test\ref\$action-errors\$language\gauss.txt"
test\bin\run_test.ps1 -json -name hello-ftn90 -action $action -errors -language $language -inputfile ".\test\data\$language\hello.f90" -expected ".\test\ref\$action-errors\$language\hello.txt"
test\bin\run_test.ps1 -json -name temp-sub-ftn90 -action $action -errors -language $language -inputfile ".\test\data\$language\temp_sub.for" -expected ".\test\ref\$action-errors\$language\temp_sub.txt"
test\bin\run_test.ps1 -json -name temp-func-ftn90 -action $action -errors -language $language -inputfile ".\test\data\$language\temp_func.for" -expected ".\test\ref\$action-errors\$language\temp_func.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex2-ftn90 -action $action -errors -language $language -inputfile ".\test\data\$language\Wikibooks-ex2.f95" -expected ".\test\ref\$action-errors\$language\Wikibooks-ex2.txt"

# Fortran-95
Set-Variable -Name language "Fortran-95"
test\bin\run_test.ps1 -json -name ibm-ex1-ftn95 -action $action -errors -language $language -inputfile ".\test\data\$language\ibm-ex1.f95" -expected ".\test\ref\$action-errors\$language\ibm-ex1.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex1-ftn95 -action $action -errors -language $language -inputfile ".\test\data\$language\Wikibooks-ex1.f95" -expected ".\test\ref\$action-errors\$language\Wikibooks-ex1.txt"

# Fortran-2003
Set-Variable -Name language "Fortran-2003"
test\bin\run_test.ps1 -json -name average-ftn2003 -action $action -errors -language $language -inputfile ".\test\data\$language\average.f90" -expected ".\test\ref\$action-errors\$language\average.txt"
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action $action -errors -language $language -inputfile ".\test\data\$language\geo4060.for" -expected ".\test\ref\$action-errors\$language\geo4060.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex3-ftn2003 -action $action -errors -language $language -inputfile ".\test\data\$language\Wikibooks-ex3.f95" -expected ".\test\ref\$action-errors\$language\Wikibooks-ex3.txt"
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn2003 -action $action -errors -language $language -inputfile ".\test\data\$language\Wikibooks-ex4.f95" -expected ".\test\ref\$action-errors\$language\Wikibooks-ex4.txt"

# Fortran-2008

# F#
Set-Variable -Name language "Fsharp"
test\bin\run_test.ps1 -json -name samples-fs -action $action -errors -language $language -inputfile ".\test\data\$language\samples.fs" -expected ".\test\ref\$action-errors\$language\samples.txt"
test\bin\run_test.ps1 -json -name antwalk-fs -action $action -errors -language $language -inputfile ".\test\data\$language\ant_walk.fs" -expected ".\test\ref\$action-errors\$language\ant_walk.txt"

# Go
Set-Variable -Name language "Go"
test\bin\run_test.ps1 -json -name find-cli-$language -action $action -errors -language $language -inputfile ".\test\data\$language\find-cli.go" -expected ".\test\ref\$action-errors\$language\find-cli.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.go" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# Groovy
Set-Variable -Name language "Groovy"
test\bin\run_test.ps1 -json -name koan05-$language -action $action -errors -language $language -inputfile ".\test\data\$language\koan05.groovy" -expected ".\test\ref\$action-errors\$language\koan05.txt"
test\bin\run_test.ps1 -json -name koan13-$language -action $action -errors -language $language -inputfile ".\test\data\$language\koan13.groovy" -expected ".\test\ref\$action-errors\$language\koan13.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.groovy" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# Haskell
Set-Variable -Name language "Haskell"
test\bin\run_test.ps1 -json -name calendar-hs -action $action -errors -language $language -inputfile ".\test\data\$language\calendar.hs" -expected ".\test\ref\$action-errors\$language\calendar.txt"
test\bin\run_test.ps1 -json -name todo-hs -action $action -errors -language $language -inputfile ".\test\data\$language\todo.hs" -expected ".\test\ref\$action-errors\$language\todo.txt"
test\bin\run_test.ps1 -json -name rna-hs -action $action -errors -language $language -inputfile ".\test\data\$language\todo.hs" -expected ".\test\ref\$action-errors\$language\rna.txt"
test\bin\run_test.ps1 -json -name zero_line-hs -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.hs" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# HTML
Set-Variable -Name language "HTML"
test\bin\run_test.ps1 -json -name knuth-html -action $action -errors -language $language -inputfile ".\test\data\$language\knuth.html" -expected ".\test\ref\$action-errors\$language\knuth.txt"
test\bin\run_test.ps1 -json -name developer-html -action $action -errors -language $language -inputfile ".\test\data\$language\developer-css.html" -expected ".\test\ref\$action-errors\$language\developer-css.txt"
test\bin\run_test.ps1 -json -name codestat-html -action $action -errors -language $language -inputfile ".\test\data\$language\codestat-css-javascript.html" -expected ".\test\ref\$action-errors\$language\codestat-css-$language.txt"

# Java
Set-Variable -Name language "Java"
test\bin\run_test.ps1 -json -name prime_test-$language -action $action -errors -language $language -inputfile ".\test\data\$language\prime_test.java" -expected ".\test\ref\$action-errors\$language\prime_test.txt"
test\bin\run_test.ps1 -json -name palindrome-$language -action $action -errors -language $language -inputfile ".\test\data\$language\palindrome.java" -expected ".\test\ref\$action-errors\$language\palindrome.txt"
test\bin\run_test.ps1 -json -name binary-search-$language -action $action -errors -language $language -inputfile ".\test\data\$language\binary_search.java" -expected ".\test\ref\$action-errors\$language\binary_search.txt"
test\bin\run_test.ps1 -json -name ObjectServer-$language -action $action -errors -language $language -inputfile ".\test\data\$language\ObjectServer.java" -expected ".\test\ref\$action-errors\$language\ObjectServer.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.java" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# JavaScript
Set-Variable -Name language "JavaScript"
test\bin\run_test.ps1 -json -name values-js -action $action -errors -language $language -inputfile ".\test\data\$language\values.js" -expected ".\test\ref\$action-errors\$language\values.txt"
test\bin\run_test.ps1 -json -name codestat-js -action $action -errors -language $language -inputfile ".\test\data\$language\codestat.js" -expected ".\test\ref\$action-errors\$language\codestat.txt"
test\bin\run_test.ps1 -json -name bing-js -action $action -errors -language $language -inputfile ".\test\data\$language\bing.js" -expected ".\test\ref\$action-errors\$language\bing.txt"
test\bin\run_test.ps1 -json -name calc_prime-js -action $action -errors -language $language -inputfile ".\test\data\$language\calc_prime.js" -expected ".\test\ref\$action-errors\$language\calc_prime.txt"
test\bin\run_test.ps1 -json -name backtick-js -action $action -errors -language $language -inputfile ".\test\data\$language\backtick.js" -expected ".\test\ref\$action-errors\$language\backtick.txt"

# Julia
Set-Variable -Name language "Julia"
test\bin\run_test.ps1 -json -name dsp-jl -action $action -errors -language $language -inputfile ".\test\data\$language\dsp.jl" -expected ".\test\ref\$action-errors\$language\dsp.txt"
test\bin\run_test.ps1 -json -name container-jl -action $action -errors -language $language -inputfile ".\test\data\$language\container.jl" -expected ".\test\ref\$action-errors\$language\container.txt"
test\bin\run_test.ps1 -json -name microbiome-jl -action $action -errors -language $language -inputfile ".\test\data\$language\microbiome.jl" -expected ".\test\ref\$action-errors\$language\microbiome.txt"
test\bin\run_test.ps1 -json -name periodograms-jl -action $action -errors -language $language -inputfile ".\test\data\$language\periodograms.jl" -expected ".\test\ref\$action-errors\$language\periodograms.txt"

# Kotlin
Set-Variable -Name language "Kotlin"
test\bin\run_test.ps1 -json -name qksms-kt -action $action -errors -language $language -inputfile ".\test\data\$language\qksms.kt" -expected ".\test\ref\$action-errors\$language\qksms.txt"
test\bin\run_test.ps1 -json -name render-kt -action $action -errors -language $language -inputfile ".\test\data\$language\render.kt" -expected ".\test\ref\$action-errors\$language\render.txt"

# Lua
Set-Variable -Name language "Lua"
test\bin\run_test.ps1 -json -name dissector-$language -action $action -errors -language $language -inputfile ".\test\data\$language\dissector.lua" -expected ".\test\ref\$action-errors\$language\dissector.txt"
test\bin\run_test.ps1 -json -name dissector2-$language -action $action -errors -language $language -inputfile ".\test\data\$language\dissector2.lua" -expected ".\test\ref\$action-errors\$language\dissector2.txt"
test\bin\run_test.ps1 -json -name dsl-$language -action $action -errors -language $language -inputfile ".\test\data\$language\dsl.lua" -expected ".\test\ref\$action-errors\$language\dsl.txt"
test\bin\run_test.ps1 -json -name markov-$language -action $action -errors -language $language -inputfile ".\test\data\$language\markov.lua" -expected ".\test\ref\$action-errors\$language\markov.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.lua" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# Matlab
$language= "Matlab"
test\bin\run_test.ps1 -json -name transpose-$language -action $action -errors -language $language -inputfile ".\test\data\$language\transpose.m" -expected ".\test\ref\$action-errors\$language\transpose.txt"
test\bin\run_test.ps1 -json -name choose-$language -action $action -errors -language $language -inputfile ".\test\data\$language\choose.m" -expected ".\test\ref\$action-errors\$language\choose.txt"
test\bin\run_test.ps1 -json -name fitnormal-$language -action $action -errors -language $language -inputfile ".\test\data\$language\fitnormal.m" -expected ".\test\ref\$action-errors\$language\fitnormal.txt"
test\bin\run_test.ps1 -json -name test_fitnormal-$language -action $action -errors -language $language -inputfile ".\test\data\$language\test_fitnormal.m" -expected ".\test\ref\$action-errors\$language\test_fitnormal.txt"

# Modula-2
Set-Variable -Name language "Modula-2"
test\bin\run_test.ps1 -json -name C64ToIBM-mod2 -action $action -errors -language $language -inputfile ".\test\data\$language\C64ToIBM.mod" -expected ".\test\ref\$action-errors\$language\C64ToIBM.txt"
test\bin\run_test.ps1 -json -name CaseDemo-mod2 -action $action -errors -language $language -inputfile ".\test\data\$language\CaseDemo.mod" -expected ".\test\ref\$action-errors\$language\CaseDemo.txt"
test\bin\run_test.ps1 -json -name game_def-mod2 -action $action -errors -language $language -inputfile ".\test\data\$language\game.def" -expected ".\test\ref\$action-errors\$language\game_def.txt"
test\bin\run_test.ps1 -json -name game-mod2 -action $action -errors -language $language -inputfile ".\test\data\$language\game.mod" -expected ".\test\ref\$action-errors\$language\game.txt"
test\bin\run_test.ps1 -json -name LoopDemo-mod2 -action $action -errors -language $language -inputfile ".\test\data\$language\LoopDemo.mod" -expected ".\test\ref\$action-errors\$language\LoopDemo.txt"
test\bin\run_test.ps1 -json -name PigLatin-mod2 -action $action -errors -language $language -inputfile ".\test\data\$language\PigLatin.mod" -expected ".\test\ref\$action-errors\$language\PigLatin.txt"
test\bin\run_test.ps1 -json -name TempConv-mod2 -action $action -errors -language $language -inputfile ".\test\data\$language\TempConv.mod" -expected ".\test\ref\$action-errors\$language\TempConv.txt"

# Objective-C
Set-Variable -Name language "Objective-C"
test\bin\run_test.ps1 -json -name hello-objc -action $action -errors -language $language -inputfile ".\test\data\$language\HelloWorld.objc" -expected ".\test\ref\$action-errors\$language\HelloWorld.txt"
test\bin\run_test.ps1 -json -name qrmath-objc -action $action -errors -language $language -inputfile ".\test\data\$language\QRMath.h" -expected ".\test\ref\$action-errors\$language\QRMath.txt"
test\bin\run_test.ps1 -json -name qrencoder-objc -action $action -errors -language $language -inputfile ".\test\data\$language\QREncoder.m" -expected ".\test\ref\$action-errors\$language\QREncoder.txt"
test\bin\run_test.ps1 -json -name jsonkit_h-objc -action $action -errors -language $language -inputfile ".\test\data\$language\JSONKit.h" -expected ".\test\ref\$action-errors\$language\JSONKit_h.txt"
test\bin\run_test.ps1 -json -name jsonkit_m-objc -action $action -errors -language $language -inputfile ".\test\data\$language\JSONKit.m" -expected ".\test\ref\$action-errors\$language\JSONKit_m.txt"

# OCaml
Set-Variable -Name language "OCaml"
test\bin\run_test.ps1 -json -name antwalk-ocaml -action $action -errors -language $language -inputfile ".\test\data\$language\ant_walk.ml" -expected ".\test\ref\$action-errors\$language\ant_walk.txt"

# Octave
Set-Variable -Name language "Octave"
test\bin\run_test.ps1 -json -name transpose-$language -action $action -errors -language $language -inputfile ".\test\data\$language\transpose.m" -expected ".\test\ref\$action-errors\$language\transpose.txt"
test\bin\run_test.ps1 -json -name choose-$language -action $action -errors -language $language -inputfile ".\test\data\$language\choose.m" -expected ".\test\ref\$action-errors\$language\choose.txt"
test\bin\run_test.ps1 -json -name fitnormal-$language -action $action -errors -language $language -inputfile ".\test\data\$language\fitnormal.m" -expected ".\test\ref\$action-errors\$language\fitnormal.txt"
test\bin\run_test.ps1 -json -name test_fitnormal-$language -action $action -errors -language $language -inputfile ".\test\data\$language\test_fitnormal.m" -expected ".\test\ref\$action-errors\$language\test_fitnormal.txt"
test\bin\run_test.ps1 -json -name tshow-$language -action $action -errors -language $language -inputfile ".\test\data\$language\tshow.m" -expected ".\test\ref\$action-errors\$language\tshow.txt"
test\bin\run_test.ps1 -json -name ex7_pca-$language -action $action -errors -language $language -inputfile ".\test\data\$language\ex7_pca.m" -expected ".\test\ref\$action-errors\$language\ex7_pca.txt"

# Pascal
Set-Variable -Name language "Pascal"
test\bin\run_test.ps1 -json -name label_declare-pas -action $action -errors -language $language -inputfile ".\test\data\$language\LabelDeclaration.pas" -expected ".\test\ref\$action-errors\$language\LabelDeclaration.txt"
test\bin\run_test.ps1 -json -name firework-pas -action $action -errors -language $language -inputfile ".\test\data\$language\FIREWORK.PAS" -expected ".\test\ref\$action-errors\$language\FIREWORK.txt"
test\bin\run_test.ps1 -json -name hello-pas -action $action -errors -language $language -inputfile ".\test\data\$language\HELLO.PAS" -expected ".\test\ref\$action-errors\$language\HELLO.txt"
test\bin\run_test.ps1 -json -name rose-pas -action $action -errors -language $language -inputfile ".\test\data\$language\ROSE.PAS" -expected ".\test\ref\$action-errors\$language\ROSE.txt"
test\bin\run_test.ps1 -json -name spider-pas -action $action -errors -language $language -inputfile ".\test\data\$language\SPIDER.PAS" -expected ".\test\ref\$action-errors\$language\SPIDER.txt"
test\bin\run_test.ps1 -json -name tpc16-pas -action $action -errors -language $language -inputfile ".\test\data\$language\TPC16.PAS" -expected ".\test\ref\$action-errors\$language\TPC16.txt"

# Perl
Set-Variable -Name language "Perl"
test\bin\run_test.ps1 -json -name perligata-$language -action $action -errors -language $language -inputfile ".\test\data\$language\Perligata.pm" -expected ".\test\ref\$action-errors\$language\Perligata.txt"
test\bin\run_test.ps1 -json -name physics-$language -action $action -errors -language $language -inputfile ".\test\data\$language\Physics.pm" -expected ".\test\ref\$action-errors\$language\Physics.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.pl" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# PL/1
Set-Variable -Name language "PL1"
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action $action -errors -language $language -inputfile ".\test\data\$language\BPGPLI.pl1" -expected ".\test\ref\$action-errors\$language\BPGPLI.txt"
test\bin\run_test.ps1 -json -name checkdt-pl1 -action $action -errors -language $language -inputfile ".\test\data\$language\CHECKDT.pl1" -expected ".\test\ref\$action-errors\$language\CHECKDT.txt"
test\bin\run_test.ps1 -json -name crtpln3-pl1 -action $action -errors -language $language -inputfile ".\test\data\$language\CRTPLN3.pl1" -expected ".\test\ref\$action-errors\$language\CRTPLN3.txt"
test\bin\run_test.ps1 -json -name mainfact-pl1 -action $action -errors -language $language -inputfile ".\test\data\$language\MAINFACT.pl1" -expected ".\test\ref\$action-errors\$language\MAINFACT.txt"
test\bin\run_test.ps1 -json -name example-pl1 -action $action -errors -language $language -inputfile ".\test\data\$language\example.pl1" -expected ".\test\ref\$action-errors\$language\example.txt"
test\bin\run_test.ps1 -json -name digrams-pl1 -action $action -errors -language $language -inputfile ".\test\data\$language\digrams.pl1" -expected ".\test\ref\$action-errors\$language\digrams.txt"

# Prolog
Set-Variable -Name language "Prolog"
test\bin\run_test.ps1 -json -name family-main-$language -action $action -errors -language $language -inputfile ".\test\data\$language\family-main.pl" -expected ".\test\ref\$action-errors\$language\family-main.txt"
test\bin\run_test.ps1 -json -name family-kb-$language -action $action -errors -language $language -inputfile ".\test\data\$language\family-kb.pl" -expected ".\test\ref\$action-errors\$language\family-kb.txt"
test\bin\run_test.ps1 -json -name family-menu-$language -action $action -errors -language $language -inputfile ".\test\data\$language\family-menu.pl" -expected ".\test\ref\$action-errors\$language\family-menu.txt"
test\bin\run_test.ps1 -json -name family-queries-$language -action $action -errors -language $language -inputfile ".\test\data\$language\family-queries.pl" -expected ".\test\ref\$action-errors\$language\family-queries.txt"
test\bin\run_test.ps1 -json -name web-server-hello-$language -action $action -errors -language $language -inputfile ".\test\data\$language\web-server-hello.pl" -expected ".\test\ref\$action-errors\$language\web-server-hello.txt"
test\bin\run_test.ps1 -json -name web-server-params-$language -action $action -errors -language $language -inputfile ".\test\data\$language\web-server-params.pl" -expected ".\test\ref\$action-errors\$language\web-server-params.txt"

# Python
Set-Variable -Name language "Python"
test\bin\run_test.ps1 -json -name drone-3d-$language -action $action -errors -language $language -inputfile ".\test\data\$language\drone_3d_trajectory_following.py" -expected ".\test\ref\$action-errors\$language\drone_3d_trajectory_following.txt"
test\bin\run_test.ps1 -json -name quadrotor-$language -action $action -errors -language $language -inputfile ".\test\data\$language\Quadrotor.py" -expected ".\test\ref\$action-errors\$language\Quadrotor.txt"
test\bin\run_test.ps1 -json -name trajectory-$language -action $action -errors -language $language -inputfile ".\test\data\$language\TrajectoryGenerator.py" -expected ".\test\ref\$action-errors\$language\TrajectoryGenerator.txt"
test\bin\run_test.ps1 -json -name values-$language -action $action -errors -language $language -inputfile ".\test\data\$language\values.py" -expected ".\test\ref\$action-errors\$language\values.txt"
test\bin\run_test.ps1 -json -name examiner-$language -action $action -errors -language $language -inputfile ".\test\data\$language\Examiner.py" -expected ".\test\ref\$action-errors\$language\Examiner.txt"
test\bin\run_test.ps1 -json -name authorized-view-$language -action $action -errors -language $language -inputfile ".\test\data\$language\authorized_view.py" -expected ".\test\ref\$action-errors\$language\authorized_view.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.py" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# R
Set-Variable -Name language "R"
test\bin\run_test.ps1 -json -name ETM-540-01-r -action $action -errors -language $language -inputfile ".\test\data\$language\ETM-540-01.R" -expected ".\test\ref\$action-errors\$language\ETM-540-01.txt"
test\bin\run_test.ps1 -json -name ETM-540-02-r -action $action -errors -language $language -inputfile ".\test\data\$language\ETM-540-02.R" -expected ".\test\ref\$action-errors\$language\ETM-540-02.txt"
test\bin\run_test.ps1 -json -name ETM-540-03-r -action $action -errors -language $language -inputfile ".\test\data\$language\ETM-540-03.R" -expected ".\test\ref\$action-errors\$language\ETM-540-03.txt"
test\bin\run_test.ps1 -json -name ETM-540-04-r -action $action -errors -language $language -inputfile ".\test\data\$language\ETM-540-04.R" -expected ".\test\ref\$action-errors\$language\ETM-540-04.txt"
test\bin\run_test.ps1 -json -name ETM-540-05-r -action $action -errors -language $language -inputfile ".\test\data\$language\ETM-540-05.R" -expected ".\test\ref\$action-errors\$language\ETM-540-05.txt"
test\bin\run_test.ps1 -json -name basketball-r -action $action -errors -language $language -inputfile ".\test\data\$language\basketball.R" -expected ".\test\ref\$action-errors\$language\basketball.txt"
test\bin\run_test.ps1 -json -name render-r -action $action -errors -language $language -inputfile ".\test\data\$language\render.R" -expected ".\test\ref\$action-errors\$language\render.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.R" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# Ruby
Set-Variable -Name language "Ruby"
test\bin\run_test.ps1 -json -name basic-$language -action $action -errors -language $language -inputfile ".\test\data\$language\basic.rb" -expected ".\test\ref\$action-errors\$language\basic.txt"
test\bin\run_test.ps1 -json -name constants-$language -action $action -errors -language $language -inputfile ".\test\data\$language\constants.rb" -expected ".\test\ref\$action-errors\$language\constants.txt"
test\bin\run_test.ps1 -json -name exceptions-$language -action $action -errors -language $language -inputfile ".\test\data\$language\exceptions.rb" -expected ".\test\ref\$action-errors\$language\exceptions.txt"
test\bin\run_test.ps1 -json -name expressions-$language -action $action -errors -language $language -inputfile ".\test\data\$language\expressions.rb" -expected ".\test\ref\$action-errors\$language\expressions.txt"
test\bin\run_test.ps1 -json -name functions-$language -action $action -errors -language $language -inputfile ".\test\data\$language\functions.rb" -expected ".\test\ref\$action-errors\$language\functions.txt"
test\bin\run_test.ps1 -json -name io-$language -action $action -errors -language $language -inputfile ".\test\data\$language\io.rb" -expected ".\test\ref\$action-errors\$language\io.txt"
test\bin\run_test.ps1 -json -name modifiers-$language -action $action -errors -language $language -inputfile ".\test\data\$language\modifiers.rb" -expected ".\test\ref\$action-errors\$language\modifiers.txt"
test\bin\run_test.ps1 -json -name operators-$language -action $action -errors -language $language -inputfile ".\test\data\$language\operators.rb" -expected ".\test\ref\$action-errors\$language\operators.txt"
test\bin\run_test.ps1 -json -name statements-$language -action $action -errors -language $language -inputfile ".\test\data\$language\statements.rb" -expected ".\test\ref\$action-errors\$language\statements.txt"
test\bin\run_test.ps1 -json -name tokenbuilders-$language -action $action -errors -language $language -inputfile ".\test\data\$language\tokenbuilders.rb" -expected ".\test\ref\$action-errors\$language\tokenbuilders.txt"
test\bin\run_test.ps1 -json -name tokenizers-$language -action $action -errors -language $language -inputfile ".\test\data\$language\tokenizers.rb" -expected ".\test\ref\$action-errors\$language\tokenizers.txt"
test\bin\run_test.ps1 -json -name tokens-$language -action $action -errors -language $language -inputfile ".\test\data\$language\tokens.rb" -expected ".\test\ref\$action-errors\$language\tokens.txt"
test\bin\run_test.ps1 -json -name webhook-$language -action $action -errors -language $language -inputfile ".\test\data\$language\webhook.rb" -expected ".\test\ref\$action-errors\$language\webhook.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.rb" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# Rust
Set-Variable -Name language "Rust"
test\bin\run_test.ps1 -json -name literals-$language -action $action -errors -language $language -inputfile ".\test\data\$language\literals.rs" -expected ".\test\ref\$action-errors\$language\literals.txt"
test\bin\run_test.ps1 -json -name dom-$language -action $action -errors -language $language -inputfile ".\test\data\$language\dom.rs" -expected ".\test\ref\$action-errors\$language\dom.txt"
test\bin\run_test.ps1 -json -name html-$language -action $action -errors -language $language -inputfile ".\test\data\$language\html.rs" -expected ".\test\ref\$action-errors\$language\html.txt"
test\bin\run_test.ps1 -json -name geometry-$language -action $action -errors -language $language -inputfile ".\test\data\$language\geometry.rs" -expected ".\test\ref\$action-errors\$language\geometry.txt"
test\bin\run_test.ps1 -json -name scene-$language -action $action -errors -language $language -inputfile ".\test\data\$language\scene.rs" -expected ".\test\ref\$action-errors\$language\scene.txt"
test\bin\run_test.ps1 -json -name comments-1-$language -action $action -errors -language $language -inputfile ".\test\data\$language\nested_comments_1.rs" -expected ".\test\ref\$action-errors\$language\nested_comments_1.txt"
test\bin\run_test.ps1 -json -name comments-2-$language -action $action -errors -language $language -inputfile ".\test\data\$language\nested_comments_2.rs" -expected ".\test\ref\$action-errors\$language\nested_comments_2.txt"
test\bin\run_test.ps1 -json -name comments-3-$language -action $action -errors -language $language -inputfile ".\test\data\$language\nested_comments_3.rs" -expected ".\test\ref\$action-errors\$language\nested_comments_3.txt"
test\bin\run_test.ps1 -json -name raw-string-1-$language -action $action -errors -language $language -inputfile ".\test\data\$language\raw_string_1.rs" -expected ".\test\ref\$action-errors\$language\raw_string_1.txt"
test\bin\run_test.ps1 -json -name raw-string-2-$language -action $action -errors -language $language -inputfile ".\test\data\$language\raw_string_2.rs" -expected ".\test\ref\$action-errors\$language\raw_string_2.txt"
test\bin\run_test.ps1 -json -name attributes-1-$language -action $action -errors -language $language -inputfile ".\test\data\$language\attributes_1.rs" -expected ".\test\ref\$action-errors\$language\attributes_1.txt"
test\bin\run_test.ps1 -json -name chip8-$language -action $action -errors -language $language -inputfile ".\test\data\$language\chip8.rs" -expected ".\test\ref\$action-errors\$language\chip8.txt"
test\bin\run_test.ps1 -json -name chip8-display-$language -action $action -errors -language $language -inputfile ".\test\data\$language\chip8-display.rs" -expected ".\test\ref\$action-errors\$language\chip8-display.txt"
test\bin\run_test.ps1 -json -name chip8-instructions-$language -action $action -errors -language $language -inputfile ".\test\data\$language\chip8-instructions.rs" -expected ".\test\ref\$action-errors\$language\chip8-instructions.txt"
test\bin\run_test.ps1 -json -name chip8-main-$language -action $action -errors -language $language -inputfile ".\test\data\$language\chip8-main.rs" -expected ".\test\ref\$action-errors\$language\chip8-main.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.rs" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# Scala
Set-Variable -Name language "Scala"
test\bin\run_test.ps1 -json -name hello-$language -action $action -errors -language $language -inputfile ".\test\data\$language\hello.$language" -expected ".\test\ref\$action-errors\$language\hello.txt"
test\bin\run_test.ps1 -json -name larger-$language -action $action -errors -language $language -inputfile ".\test\data\$language\larger.$language" -expected ".\test\ref\$action-errors\$language\larger.txt"
test\bin\run_test.ps1 -json -name random-$language -action $action -errors -language $language -inputfile ".\test\data\$language\random.$language" -expected ".\test\ref\$action-errors\$language\random.txt"
test\bin\run_test.ps1 -json -name variables-$language -action $action -errors -language $language -inputfile ".\test\data\$language\variables.$language" -expected ".\test\ref\$action-errors\$language\variables.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.scala" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# SQL
Set-Variable -Name language "SQL"
test\bin\run_test.ps1 -json -name microsoft-sql -action $action -errors -language $language -inputfile ".\test\data\$language\microsoft.sql" -expected ".\test\ref\$action-errors\$language\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql -action $action -errors -language $language -inputfile ".\test\data\$language\table.sql" -expected ".\test\ref\$action-errors\$language\table.txt"

# T-SQL
Set-Variable -Name language "T-SQL"
test\bin\run_test.ps1 -json -name brackets-tsql -action $action -errors -language $language -inputfile ".\test\data\$language\brackets.sql" -expected ".\test\ref\$action-errors\$language\brackets.txt"

# Swift
Set-Variable -Name language "Swift"
test\bin\run_test.ps1 -json -name AppDelegate -action $action -errors -language $language -inputfile ".\test\data\$language\AppDelegate.$language" -expected ".\test\ref\$action-errors\$language\AppDelegate.txt"
test\bin\run_test.ps1 -json -name Meal -action $action -errors -language $language -inputfile ".\test\data\$language\Meal.$language" -expected ".\test\ref\$action-errors\$language\Meal.txt"
test\bin\run_test.ps1 -json -name MealTableViewCell -action $action -errors -language $language -inputfile ".\test\data\$language\MealTableViewCell.$language" -expected ".\test\ref\$action-errors\$language\MealTableViewCell.txt"
test\bin\run_test.ps1 -json -name MealTableViewController -action $action -errors -language $language -inputfile ".\test\data\$language\MealTableViewController.$language" -expected ".\test\ref\$action-errors\$language\MealTableViewController.txt"
test\bin\run_test.ps1 -json -name MealViewController -action $action -errors -language $language -inputfile ".\test\data\$language\MealViewController.$language" -expected ".\test\ref\$action-errors\$language\MealViewController.txt"
test\bin\run_test.ps1 -json -name RatingControl -action $action -errors -language $language -inputfile ".\test\data\$language\RatingControl.$language" -expected ".\test\ref\$action-errors\$language\RatingControl.txt"
test\bin\run_test.ps1 -json -name URLExtensions -action $action -errors -language $language -inputfile ".\test\data\$language\URLExtensions.$language" -expected ".\test\ref\$action-errors\$language\URLExtensions.txt"
test\bin\run_test.ps1 -json -name zero_line-$language -action $action -errors -language $language -inputfile ".\test\data\$language\zero_line_hello_world.swift" -expected ".\test\ref\$action-errors\$language\zero_line_hello_world.txt"

# TypeScript
Set-Variable -Name language "TypeScript"
test\bin\run_test.ps1 -json -name TimeReporter-ts -action $action -errors -language $language -inputfile ".\test\data\$language\TimeReporter.ts" -expected ".\test\ref\$action-errors\$language\TimeReporter.txt"
test\bin\run_test.ps1 -json -name ImageBoard-ts -action $action -errors -language $language -inputfile ".\test\data\$language\ImageBoard.ts" -expected ".\test\ref\$action-errors\$language\ImageBoard.txt"

# Visual Basic 6
Set-Variable -Name language "VisualBasic-6"
test\bin\run_test.ps1 -json -name spider-vb6 -action $action -errors -language $language -inputfile ".\test\data\$language\spider.bas" -expected ".\test\ref\$action-errors\$language\spider.txt"
test\bin\run_test.ps1 -json -name azure_metadata-vb6 -action $action -errors -language $language -inputfile ".\test\data\$language\azure_metadata.bas" -expected ".\test\ref\$action-errors\$language\azure_metadata.txt"
test\bin\run_test.ps1 -json -name diffie_hellman-vb6 -action $action -errors -language $language -inputfile ".\test\data\$language\diffie_hellman.bas" -expected ".\test\ref\$action-errors\$language\diffie_hellman.txt"

# Visual Basic .NET
Set-Variable -Name language "VisualBasic-NET"
test\bin\run_test.ps1 -json -name word-processor-vbnet -action $action -errors -language $language -inputfile ".\test\data\$language\WordProcessor.bas" -expected ".\test\ref\$action-errors\$language\WordProcessor.txt"

# Polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c-78 -action $action -errors -language C-78 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action-errors\C-78\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action $action -errors -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action-errors\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action $action -errors -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action-errors\Pascal\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-python -action $action -errors -language Python -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action-errors\Python\polyglot-py-rb.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-ruby -action $action -errors -language Ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action-errors\Ruby\polyglot-py-rb.txt"
