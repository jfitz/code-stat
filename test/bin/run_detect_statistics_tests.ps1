Set-StrictMode -Version Latest

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action statistics -language "ada-83 ada-95 basic c pl1-fixed swift" -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\detect-statistics\adabkend-adb.txt"

# BASIC
test\bin\run_test.ps1 -json -name values-bas -action statistics -language "BASIC CBASIC pascal python" -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\detect-statistics\values.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action statistics -language "BASIC cobol-68 fortran-66 c" -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\detect-statistics\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action statistics -language "BASIC BASICA ruby prolog" -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\detect-statistics\life.txt"

# C-99
test\bin\run_test.ps1 -json -name parser-h-99 -action statistics -language "C python swift" -inputfile ".\test\data\C-99\parser.h" -expected ".\test\ref\detect-statistics\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c-99 -action statistics -language "C C++ pascal" -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\detect-statistics\parser-c.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action statistics -language "CBASIC BASIC pascal" -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\detect-statistics\fibo.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken3-68 -action statistics -language "COBOL-68 cobol-85 fortran-77" -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\detect-statistics\mccracken3.txt"

# COBOL-74
test\bin\run_test.ps1 -json -name prog15-4-74 -action statistics -language "COBOL-74 pascal pl1-fixed" -inputfile ".\test\data\COBOL-74\PROG15-4.COB" -expected ".\test\ref\detect-statistics\PROG15-4.txt"

# COBOL-2002
test\bin\run_test.ps1 -json -name report-card-cob2002 -action statistics -language "COBOL-2002 java rust" -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\detect-statistics\ReportCard.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action statistics -language "Cplusplus visualbasic-6" -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\detect-statistics\checkers.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action statistics -language "Csharp Fsharp Java" -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\detect-statistics\calculator1.txt"

# FORTRAN-66
test\bin\run_test.ps1 -json -name heron2-ftn66 -action statistics -language "FORTRAN-66 cobol-68 ruby" -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\detect-statistics\HERON2.txt"

# FORTRAN-77
test\bin\run_test.ps1 -json -name heron-ftn77 -action statistics -language "FORTRAN-77 pascal sql-92" -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\detect-statistics\HERON.txt"

# Fortran-90
test\bin\run_test.ps1 -json -name average-ftn90 -action statistics -language "Fortran-90 r pl1-free" -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\detect-statistics\average.txt"

# Fortran-95
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn95 -action statistics -language "Fortran-95 swift html" -inputfile ".\test\data\Fortran-95\Wikibooks-ex4.f95" -expected ".\test\ref\detect-statistics\Wikibooks-ex4.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action statistics -language "Fortran-2003 go javascript" -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\detect-statistics\geo4060.txt"

# F#
test\bin\run_test.ps1 -json -name samples-fs -action statistics -language "Fsharp r basic" -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\detect-statistics\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action statistics -language "Go basic c swift" -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\detect-statistics\find-cli.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action statistics -language "HTML basic java" -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\detect-statistics\knuth.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action statistics -language "Java basic csharp fsharp" -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\detect-statistics\prime_test.txt"

# JavaScript
test\bin\run_test.ps1 -json -name codestat-js -action statistics -language "JavaScript html" -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\detect-statistics\codestat.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action statistics -language "Objective-C c cplusplus swift" -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\detect-statistics\HelloWorld.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework-pas -action statistics -language "Pascal fortran-77 basic ruby" -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect-statistics\FIREWORK.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action statistics -language "PL1-Fixed fortran-77 cobol-85" -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\detect-statistics\BPGPLI.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main-prolog -action statistics -language "Prolog basic ruby html" -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\detect-statistics\family-main.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d-python -action statistics -language "Python Java Rust Ruby" -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\detect-statistics\drone_3d_trajectory_following.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01-r -action statistics -language "R Fortran-77 basic" -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\detect-statistics\ETM-540-01.txt"

# Ruby
test\bin\run_test.ps1 -json -name basic-ruby -action statistics -language "Ruby basic rust sql-92" -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\detect-statistics\basic.txt"

# Rust
test\bin\run_test.ps1 -json -name dom-rust -action statistics -language "Rust Ruby Basic" -inputfile ".\test\data\Rust\dom.rs" -expected ".\test\ref\detect-statistics\dom.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action statistics -language "SQL-92 basic cobol-85" -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\detect-statistics\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action statistics -language "SQL-92 fortran-77 cbasic html" -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\detect-statistics\table.txt"

# Swift
test\bin\run_test.ps1 -json -name MealTableViewCell -action statistics -language "Swift basic sql-92" -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\detect-statistics\MealTableViewCell.txt"

# TypeScript
test\bin\run_test.ps1 -json -name TimeReporter-ts -action statistics -language "TypeScript JavaScript Java Basic" -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\detect-statistics\TimeReporter.txt"

# Visual Basic 6
test\bin\run_test.ps1 -json -name spider-vb6 -action statistics -language "VisualBasic-6 basic cbasic ruby" -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\detect-statistics\spider.txt"

# Visual Basic .NET
test\bin\run_test.ps1 -json -name word-processor-vbnet -action statistics -language "VisualBasic-NET basic cbasic ruby rust" -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\detect-statistics\WordProcessor.txt"

# polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c -action statistics -language "C COBOL-85 Pascal Fortran-66" -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect-statistics\polyglot.txt"

# non-code files
test\bin\run_test.ps1 -json -name noncode-empty -action statistics -inputfile ".\test\data\noncode\empty.txt" -expected ".\test\ref\detect-statistics\noncode\empty.txt"
test\bin\run_test.ps1 -json -name noncode-whitespace -action statistics -inputfile ".\test\data\noncode\whitespace.txt" -expected ".\test\ref\detect-statistics\noncode\whitespace.txt"
test\bin\run_test.ps1 -json -name noncode-win-ini -action statistics -inputfile ".\test\data\noncode\win.ini" -expected ".\test\ref\detect-statistics\noncode\win.txt"
test\bin\run_test.ps1 -json -name noncode-iis -action statistics -inputfile ".\test\data\noncode\iis.log" -expected ".\test\ref\detect-statistics\noncode\iis.txt"
test\bin\run_test.ps1 -json -name noncode-setutc -action statistics -inputfile ".\test\data\noncode\setutc.reg" -expected ".\test\ref\detect-statistics\noncode\setutc.txt"
test\bin\run_test.ps1 -json -name noncode-diagwrn -action statistics -inputfile ".\test\data\noncode\diagwrn.xml" -expected ".\test\ref\detect-statistics\noncode\diagwrn.txt"
test\bin\run_test.ps1 -json -name noncode-profile-xlsx -action statistics -inputfile ".\test\data\noncode\profile.xlsx" -expected ".\test\ref\detect-statistics\noncode\profile.txt"
test\bin\run_test.ps1 -json -name noncode-resume-docx -action statistics -inputfile ".\test\data\noncode\resume.docx" -expected ".\test\ref\detect-statistics\noncode\resume.txt"
test\bin\run_test.ps1 -json -name noncode-hh -action statistics -inputfile ".\test\data\noncode\hh.exe" -expected ".\test\ref\detect-statistics\noncode\hh.txt"
