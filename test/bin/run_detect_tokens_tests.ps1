Set-StrictMode -Version Latest

[int]$failures = 0

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action tokens -languages "ada-83 ada-95 basic c pl1-fixed swift" -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\detect-tokens\adabkend-adb.txt"

# BASIC
test\bin\run_test.ps1 -json -name values-bas -action tokens -languages "BASIC CBASIC pascal python" -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\detect-tokens\values.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action tokens -languages "BASIC cobol-68 fortran-66 c" -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\detect-tokens\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action tokens -languages "" -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\detect-tokens\life.txt"

# C
test\bin\run_test.ps1 -json -name parser-h -action tokens -languages "C python swift" -inputfile ".\test\data\C\parser.h" -expected ".\test\ref\detect-tokens\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c -action tokens -languages "C C++ pascal" -inputfile ".\test\data\C\parser.c" -expected ".\test\ref\detect-tokens\parser-c.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action tokens -languages "CBASIC BASIC pascal" -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\detect-tokens\fibo.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken3-68 -action tokens -languages "COBOL-68 cobol-85 fortran-77" -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\detect-tokens\mccracken3.txt"

# COBOL-74
test\bin\run_test.ps1 -json -name prog15-4-74 -action tokens -languages "COBOL-74 pascal pl1-fixed" -inputfile ".\test\data\COBOL-74\PROG15-4.COB" -expected ".\test\ref\detect-tokens\PROG15-4.txt"

# COBOL-2002
test\bin\run_test.ps1 -json -name report-card-cob2002 -action tokens -languages "COBOL-2002 java rust" -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\detect-tokens\ReportCard.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action tokens -languages "Cplusplus visualbasic-6" -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\detect-tokens\checkers.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action tokens -languages "Csharp Fsharp Java" -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\detect-tokens\calculator1.txt"

# FORTRAN-66
test\bin\run_test.ps1 -json -name heron2-ftn66 -action tokens -languages "FORTRAN-66 cobol-68 ruby" -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\detect-tokens\HERON2.txt"

# FORTRAN-77
test\bin\run_test.ps1 -json -name heron-ftn77 -action tokens -languages "FORTRAN-77 pascal sql-92" -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\detect-tokens\HERON.txt"

# Fortran-90
test\bin\run_test.ps1 -json -name average-ftn90 -action tokens -languages "Fortran-90 r pl1-free" -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\detect-tokens\average.txt"

# Fortran-95
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn95 -action tokens -languages "Fortran-95 swift html" -inputfile ".\test\data\Fortran-95\Wikibooks-ex4.f95" -expected ".\test\ref\detect-tokens\Wikibooks-ex4.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action tokens -languages "Fortran-2003 go javascript" -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\detect-tokens\geo4060.txt"

# F#
test\bin\run_test.ps1 -json -name samples-fs -action tokens -languages "Fsharp r basic" -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\detect-tokens\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action tokens -languages "Go basic c swift" -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\detect-tokens\find-cli.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action tokens -languages "HTML basic java" -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\detect-tokens\knuth.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action tokens -languages "Java basic csharp fsharp" -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\detect-tokens\prime_test.txt"

# JavaScript
test\bin\run_test.ps1 -json -name codestat-js -action tokens -languages "JavaScript html" -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\detect-tokens\codestat.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action tokens -languages "Objective-C c cplusplus swift" -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\detect-tokens\HelloWorld.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework-pas -action tokens -languages "Pascal fortran-77 basic ruby" -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect-tokens\FIREWORK.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action tokens -languages "PL1-Fixed fortran-77 cobol-85" -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\detect-tokens\BPGPLI.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main-prolog -action tokens -languages "Prolog basic ruby html" -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\detect-tokens\family-main.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d-python -action tokens -languages "Python Java Rust Ruby" -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\detect-tokens\drone_3d_trajectory_following.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01-r -action tokens -languages "R Fortran-77 basic" -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\detect-tokens\ETM-540-01.txt"

# Ruby
test\bin\run_test.ps1 -json -name basic-ruby -action tokens -languages "Ruby basic rust sql-92" -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\detect-tokens\basic.txt"

# Rust
test\bin\run_test.ps1 -json -name dom-rust -action tokens -languages "Rust Ruby Basic" -inputfile ".\test\data\Rust\dom.rs" -expected ".\test\ref\detect-tokens\dom.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action tokens -languages "SQL-92 basic cobol-85" -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\detect-tokens\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action tokens -languages "SQL-92 fortran-77 cbasic html" -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\detect-tokens\table.txt"

# Swift
test\bin\run_test.ps1 -json -name MealTableViewCell -action tokens -languages "Swift basic sql-92" -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\detect-tokens\MealTableViewCell.txt"

# TypeScript
test\bin\run_test.ps1 -json -name TimeReporter-ts -action tokens -languages "TypeScript JavaScript Java Basic" -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\detect-tokens\TimeReporter.txt"

# Visual Basic 6
test\bin\run_test.ps1 -json -name spider-vb6 -action tokens -languages "VisualBasic-6 basic cbasic ruby" -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\detect-tokens\spider.txt"

# Visual Basic .NET
test\bin\run_test.ps1 -json -name word-processor-vbnet -action tokens -languages "VisualBasic-NET basic cbasic ruby rust" -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\detect-tokens\WordProcessor.txt"

# polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c -action tokens -languages "C COBOL-85 Pascal Fortran-66" -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect-tokens\polyglot.txt"

Write-Output "Number of failures: $failures"
