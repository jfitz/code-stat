Set-StrictMode -Version Latest

[int]$failures = 0

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action confidence -languages "ada-83 ada-95 basic c pl1-fixed swift" -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\detect-confidence\adabkend-adb.txt"

# BASIC
test\bin\run_test.ps1 -json -name values-bas -action confidence -languages "BASIC CBASIC pascal python" -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\detect-confidence\values.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action confidence -languages "BASIC cobol-68 fortran-66 c" -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\detect-confidence\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action confidence -languages "" -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\detect-confidence\life.txt"

# C
test\bin\run_test.ps1 -json -name parser-h -action confidence -languages "C python swift" -inputfile ".\test\data\C\parser.h" -expected ".\test\ref\detect-confidence\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c -action confidence -languages "C C++ pascal" -inputfile ".\test\data\C\parser.c" -expected ".\test\ref\detect-confidence\parser-c.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action confidence -languages "CBASIC BASIC pascal" -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\detect-confidence\fibo.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken3-68 -action confidence -languages "COBOL-68 cobol-85 fortran-77" -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\detect-confidence\mccracken3.txt"

# COBOL-74
test\bin\run_test.ps1 -json -name prog15-4-74 -action confidence -languages "COBOL-74 pascal pl1-fixed" -inputfile ".\test\data\COBOL-74\PROG15-4.COB" -expected ".\test\ref\detect-confidence\PROG15-4.txt"

# COBOL-2002
test\bin\run_test.ps1 -json -name report-card-cob2002 -action confidence -languages "COBOL-2002 java rust" -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\detect-confidence\ReportCard.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action confidence -languages "Cplusplus visualbasic-6" -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\detect-confidence\checkers.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action confidence -languages "Csharp Fsharp Java" -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\detect-confidence\calculator1.txt"

# FORTRAN-66
test\bin\run_test.ps1 -json -name heron2-ftn66 -action confidence -languages "FORTRAN-66 cobol-68 ruby" -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\detect-confidence\HERON2.txt"

# FORTRAN-77
test\bin\run_test.ps1 -json -name heron-ftn77 -action confidence -languages "FORTRAN-77 pascal sql-92" -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\detect-confidence\HERON.txt"

# Fortran-90
test\bin\run_test.ps1 -json -name average-ftn90 -action confidence -languages "Fortran-90 r pl1-free" -inputfile ".\test\data\Fortran-90\average.f90" -expected ".\test\ref\detect-confidence\average.txt"

# Fortran-95
test\bin\run_test.ps1 -json -name Wikibooks-ex4-ftn95 -action confidence -languages "Fortran-95 swift html" -inputfile ".\test\data\Fortran-95\Wikibooks-ex4.f95" -expected ".\test\ref\detect-confidence\Wikibooks-ex4.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action confidence -languages "Fortran-2003 go javascript" -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\detect-confidence\geo4060.txt"

# F#
test\bin\run_test.ps1 -json -name samples-fs -action confidence -languages "Fsharp r basic" -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\detect-confidence\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action confidence -languages "Go basic c swift" -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\detect-confidence\find-cli.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action confidence -languages "HTML basic java" -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\detect-confidence\knuth.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action confidence -languages "Java basic csharp fsharp" -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\detect-confidence\prime_test.txt"

# JavaScript
test\bin\run_test.ps1 -json -name codestat-js -action confidence -languages "JavaScript html" -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\detect-confidence\codestat.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action confidence -languages "Objective-C c cplusplus swift" -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\detect-confidence\HelloWorld.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework-pas -action confidence -languages "Pascal fortran-77 basic ruby" -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect-confidence\FIREWORK.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action confidence -languages "PL1-Fixed fortran-77 cobol-85" -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\detect-confidence\BPGPLI.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main-prolog -action confidence -languages "Prolog basic ruby html" -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\detect-confidence\family-main.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d-python -action confidence -languages "Python Java Rust Ruby" -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\detect-confidence\drone_3d_trajectory_following.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01-r -action confidence -languages "R Fortran-77 basic" -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\detect-confidence\ETM-540-01.txt"

# Ruby
test\bin\run_test.ps1 -json -name basic-ruby -action confidence -languages "Ruby basic rust sql-92" -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\detect-confidence\basic.txt"

# Rust
test\bin\run_test.ps1 -json -name dom-rust -action confidence -languages "Rust Ruby Basic" -inputfile ".\test\data\Rust\dom.rs" -expected ".\test\ref\detect-confidence\dom.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action confidence -languages "SQL-92 basic cobol-85" -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\detect-confidence\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action confidence -languages "SQL-92 fortran-77 cbasic html" -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\detect-confidence\table.txt"

# Swift
test\bin\run_test.ps1 -json -name MealTableViewCell -action confidence -languages "Swift basic sql-92" -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\detect-confidence\MealTableViewCell.txt"

# TypeScript
test\bin\run_test.ps1 -json -name TimeReporter-ts -action confidence -languages "TypeScript JavaScript Java Basic" -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\detect-confidence\TimeReporter.txt"

# Visual Basic 6
test\bin\run_test.ps1 -json -name spider-vb6 -action confidence -languages "VisualBasic-6 basic cbasic ruby" -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\detect-confidence\spider.txt"

# Visual Basic .NET
test\bin\run_test.ps1 -json -name word-processor-vbnet -action confidence -languages "VisualBasic-NET basic cbasic ruby rust" -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\detect-confidence\WordProcessor.txt"

# polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c -action confidence -languages "C COBOL-85 Pascal Fortran-66" -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect-confidence\polyglot.txt"

Write-Output "Number of failures: $failures"
