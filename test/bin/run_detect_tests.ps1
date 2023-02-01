Set-StrictMode -Version Latest

Set-Variable -Name action "detect"

# generic assembly

# Assembly for IBM, DEC, Intel, and more
test\bin\run_detect_tests_language.ps1 -language "Assembly"

# Ada 83, Ada 95
test\bin\run_detect_tests_language.ps1 -language "Ada"

# Awk
test\bin\run_detect_tests_language.ps1 -language "Awk"

# BASIC
test\bin\run_detect_tests_language.ps1 -language "BASIC"

# BBC BASIC
test\bin\run_detect_tests_language.ps1 -language "BBC-BASIC"

# C and Objective-C
test\bin\run_detect_tests_language.ps1 -language "C"

# CBASIC
test\bin\run_detect_tests_language.ps1 -language "CBASIC"

# COBOL
test\bin\run_detect_tests_language.ps1 -language "COBOL"

# C++
test\bin\run_detect_tests_language.ps1 -language "Cplusplus"

# C#
test\bin\run_detect_tests_language.ps1 -language "Csharp"

# D
test\bin\run_detect_tests_language.ps1 -language "D"

# Dart
test\bin\run_detect_tests_language.ps1 -language "Dart"

# dbase
test\bin\run_detect_tests_language.ps1 -language "dbase"

# Dibol
test\bin\run_detect_tests_language.ps1 -language "Dibol"

# Eiffel
test\bin\run_detect_tests_language.ps1 -language "Eiffel"

# Erlang
test\bin\run_detect_tests_language.ps1 -language "Erlang"

# Flowmatic
test\bin\run_detect_tests_language.ps1 -language "Flowmatic"

# FORTRAN
test\bin\run_detect_tests_language.ps1 -language "FORTRAN"

# F#
test\bin\run_detect_tests_language.ps1 -language "Fsharp"

# Go
test\bin\run_detect_tests_language.ps1 -language "Go"

# Groovy
test\bin\run_detect_tests_language.ps1 -language "Groovy"

# Haskell
test\bin\run_detect_tests_language.ps1 -language "Haskell"

# HTML
test\bin\run_detect_tests_language.ps1 -language "HTML"

# Intercal
test\bin\run_detect_tests_language.ps1 -language "Intercal"

# Java
test\bin\run_detect_tests_language.ps1 -language "Java"

# JavaScript and TypeScript and CoffeeScript
test\bin\run_detect_tests_language.ps1 -language "JavaScript"

# Julia
test\bin\run_detect_tests_language.ps1 -language "Julia"

# Kotlin
test\bin\run_detect_tests_language.ps1 -language "Kotlin"

# Latino
test\bin\run_detect_tests_language.ps1 -language "Latino"

# Lua
test\bin\run_detect_tests_language.ps1 -language "Lua"

# Matlab and Octave
test\bin\run_detect_tests_language.ps1 -language "Matlab"

# OCaml
test\bin\run_detect_tests_language.ps1 -language "ML"

# Pascal and Delphi and Modula-2
test\bin\run_detect_tests_language.ps1 -language "Pascal"

# Perl
test\bin\run_detect_tests_language.ps1 -language "Perl"

# PL/1 and PL/M
test\bin\run_detect_tests_language.ps1 -language "PL1"

# Prolog
test\bin\run_detect_tests_language.ps1 -language "Prolog"

# Python
test\bin\run_detect_tests_language.ps1 -language "Python"

# R
test\bin\run_detect_tests_language.ps1 -language "R"

# Ruby
test\bin\run_detect_tests_language.ps1 -language "Ruby"

# Rust
test\bin\run_detect_tests_language.ps1 -language "Rust"

# Scala
test\bin\run_detect_tests_language.ps1 -language "Scala"

# SQL, PL/SQL, and T-SQL
test\bin\run_detect_tests_language.ps1 -language "SQL"

# Swift
test\bin\run_detect_tests_language.ps1 -language "Swift"

# Visual Basic
test\bin\run_detect_tests_language.ps1 -language "VisualBasic"

# polyglot programs
# test\bin\run_detect_test.ps1 -json -name polyglot -notiebreak -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\multiple\polyglot.txt"
# test\bin\run_detect_test.ps1 -json -name polyglot-languages -notiebreak -language C-78,Cplusplus,COBOL-68,COBOL-74,cobol-85,objective-c,Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\$action\multiple\polyglot-languages.txt"
# test\bin\run_detect_test.ps1 -json -name polyglot-py-rb -notiebreak -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action\multiple\polyglot-py-rb.txt"
# test\bin\run_detect_test.ps1 -json -name polyglot-py-rb-languages -notiebreak -language python,ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\$action\multiple\polyglot-py-rb-languages.txt"

# non-code files
# test\bin\run_detect_test.ps1 -json -name noncode-empty -inputfile ".\test\data\noncode\empty.txt" -expected ".\test\ref\$action\noncode\empty.txt"
# test\bin\run_detect_test.ps1 -json -name noncode-whitespace -inputfile ".\test\data\noncode\whitespace.txt" -expected ".\test\ref\$action\noncode\whitespace.txt"
# test\bin\run_detect_test.ps1 -json -name noncode-win-ini -inputfile ".\test\data\noncode\win.ini" -expected ".\test\ref\$action\noncode\win.txt"
# test\bin\run_detect_test.ps1 -json -name noncode-setutc -inputfile ".\test\data\noncode\setutc.reg" -expected ".\test\ref\$action\noncode\setutc.txt"
# test\bin\run_detect_test.ps1 -json -name noncode-diagwrn -inputfile ".\test\data\noncode\diagwrn.xml" -expected ".\test\ref\$action\noncode\diagwrn.txt"
# test\bin\run_detect_test.ps1 -json -name noncode-profile-xlsx -inputfile ".\test\data\noncode\profile.xlsx" -expected ".\test\ref\$action\noncode\profile.txt"
# test\bin\run_detect_test.ps1 -json -name noncode-resume-docx -inputfile ".\test\data\noncode\resume.docx" -expected ".\test\ref\$action\noncode\resume.txt"
# test\bin\run_detect_test.ps1 -json -name noncode-hh -inputfile ".\test\data\noncode\hh.exe" -expected ".\test\ref\$action\noncode\hh.txt"
