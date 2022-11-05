Set-StrictMode -Version Latest

Set-Variable -Name valid_actions ("tokens", "statistics", "confidence", "confidence-errors")

if (-Not ($valid_actions.Contains($action))) {
    throw "Invalid or missing action"
}

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
test\bin\run_test.ps1 -json -name microsoft -action $action -language generic -inputfile ".\test\data\T-SQL\microsoft.sql" -expected ".\test\ref\$action\generic\microsoft.txt"
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
test\bin\run_test.ps1 -json -name microsoft -action $action -language generic -comment ada -inputfile ".\test\data\T-SQL\microsoft.sql" -expected ".\test\ref\$action\generic-comments\microsoft.txt"
test\bin\run_test.ps1 -json -name spider -action $action -language generic -comment basic -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\$action\generic-comments\spider.txt"

# generic assembly

# Assembly for IBM, DEC, Intel, and more
test\bin\run_action_tests_language.ps1 -language "Assembly" -variants ("ASM-360", "ASM-370", "ASM-390", "ASM-1802", "ASM-6502", "ASM-6800", "ASM-68000", "ASM-8080", "ASM-Z-80", "ASM-8086", "ASM-80386", "ASM-PDP-8", "ASM-PDP-11")

# Ada 83, Ada 95
test\bin\run_action_tests_language.ps1 -language "Ada" -variants ("Ada-83", "Ada-95")

# Awk
test\bin\run_action_tests_language.ps1 -language "Awk" -variants ("Awk")

# BASIC
test\bin\run_action_tests_language.ps1 -language "BASIC" -variants ("BASIC", "BASIC-80", "BASICA")

# C and Objective-C
test\bin\run_action_tests_language.ps1 -language "C" -variants ("C-78", "C-89", "C-99", "Objective-C")

# CBASIC
test\bin\run_action_tests_language.ps1 -language "CBASIC" -variants ("CBASIC")

# COBOL
test\bin\run_action_tests_language.ps1 -language "COBOL" -variants ("COBOL-68", "COBOL-74", "COBOL-85", "COBOL-2014", "COBOL-2014-ACU", "COBOL-2014-GNU", "COBOL-2014-IBM")

# C++
test\bin\run_action_tests_language.ps1 -language "Cplusplus" -variants ("Cplusplus")

# C#
test\bin\run_action_tests_language.ps1 -language "Csharp" -variants ("Csharp")

# D
test\bin\run_action_tests_language.ps1 -language "D" -variants ("D")

# Dart
test\bin\run_action_tests_language.ps1 -language "Dart" -variants ("Dart")

# dbase
test\bin\run_action_tests_language.ps1 -language "dbase" -variants ("dbase-ii", "dbase-iii")

# Dibol
test\bin\run_action_tests_language.ps1 -language "Dibol" -variants ("Dibol")

# Eiffel
test\bin\run_action_tests_language.ps1 -language "Eiffel" -variants ("Eiffel")

# Erlang
test\bin\run_action_tests_language.ps1 -language "Erlang" -variants ("Erlang")

# Flowmatic
test\bin\run_action_tests_language.ps1 -language "Flowmatic" -variants ("Flowmatic")

# FORTRAN
test\bin\run_action_tests_language.ps1 -language "FORTRAN" -variants ("FORTRAN-66", "FORTRAN-77", "Fortran-90", "Fortran-95", "Fortran-2003")

# F#
test\bin\run_action_tests_language.ps1 -language "Fsharp" -variants ("Fsharp")

# Go
test\bin\run_action_tests_language.ps1 -language "Go" -variants ("Go")

# Groovy
test\bin\run_action_tests_language.ps1 -language "Groovy" -variants ("Groovy")

# Haskell
test\bin\run_action_tests_language.ps1 -language "Haskell" -variants ("Haskell")

# HTML
test\bin\run_action_tests_language.ps1 -language "HTML" -variants ("HTML")

# Intercal
test\bin\run_action_tests_language.ps1 -language "Intercal" -variants ("Intercal")

# Java
test\bin\run_action_tests_language.ps1 -language "Java" -variants ("Java")

# JavaScript and TypeScript and CoffeeScript
test\bin\run_action_tests_language.ps1 -language "JavaScript" -variants ("JavaScript", "TypeScript", "CoffeeScript")

# Julia
test\bin\run_action_tests_language.ps1 -language "Julia" -variants ("Julia")

# Kotlin
test\bin\run_action_tests_language.ps1 -language "Kotlin" -variants ("Kotlin")

# Latino
test\bin\run_action_tests_language.ps1 -language "Latino" -variants ("Latino")

# Lua
test\bin\run_action_tests_language.ps1 -language "Lua" -variants ("Lua")

# Matlab and Octave
test\bin\run_action_tests_language.ps1 -language "Matlab" -variants ("Matlab", "Octave")

# OCaml
test\bin\run_action_tests_language.ps1 -language "ML" -variants ("OCaml")

# Pascal and Delphi and Modula-2
test\bin\run_action_tests_language.ps1 -language "Pascal" -variants ("Pascal", "Delphi", "Modula-2")

# Perl
test\bin\run_action_tests_language.ps1 -language "Perl" -variants ("Perl")

# PL/1 and PL/M
test\bin\run_action_tests_language.ps1 -language "PL1" -variants ("PL1", "PLM")

# Prolog
test\bin\run_action_tests_language.ps1 -language "Prolog" -variants ("Prolog")

# Python
test\bin\run_action_tests_language.ps1 -language "Python" -variants ("Python")

# R
test\bin\run_action_tests_language.ps1 -language "R" -variants ("R")

# Ruby
test\bin\run_action_tests_language.ps1 -language "Ruby" -variants ("Ruby")

# Rust
test\bin\run_action_tests_language.ps1 -language "Rust" -variants ("Rust")

# Scala
test\bin\run_action_tests_language.ps1 -language "Scala" -variants ("Scala")

# SQL, PL/SQL, and T-SQL
test\bin\run_action_tests_language.ps1 -language "SQL" -variants ("SQL", "PL-SQL", "T-SQL")

# Swift
test\bin\run_action_tests_language.ps1 -language "Swift" -variants ("Swift")

# Visual Basic
test\bin\run_action_tests_language.ps1 -language "VisualBasic" -variants ("VisualBasic-6", "VisualBasic-NET")

# Polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c-78 -action $action -language C-78 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\C-78\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-COBOL-85 -action $action -language COBOL-85 -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\COBOL-85\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-pascal -action $action -language Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\Pascal\polyglot.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-python -action $action -language Python -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action\Python\polyglot-py-rb.txt"
test\bin\run_test.ps1 -json -name polyglot-py-rb-ruby -action $action -language Ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action\Ruby\polyglot-py-rb.txt"
