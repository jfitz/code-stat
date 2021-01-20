Set-StrictMode -Version Latest

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action tokens -language "ada-83 ada-95 basic c pl1 swift" -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\detect-tokens\adabkend-adb.txt"

# BASIC
test\bin\run_test.ps1 -json -name values-bas -action tokens -language "BASIC CBASIC pascal python" -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\detect-tokens\values.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action tokens -language "BASIC cobol-68 fortran-66 c" -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\detect-tokens\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action tokens -language "BASIC BASICA ruby prolog" -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\detect-tokens\life.txt"

# C-99
test\bin\run_test.ps1 -json -name parser-h-99 -action tokens -language "C python swift" -inputfile ".\test\data\C-99\parser.h" -expected ".\test\ref\detect-tokens\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c-99 -action tokens -language "C C++ pascal" -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\detect-tokens\parser-c.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action tokens -language "CBASIC BASIC pascal" -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\detect-tokens\fibo.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken3-68 -action tokens -language "COBOL-68 cobol-85 fortran-77" -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\detect-tokens\mccracken3.txt"

# COBOL-85
test\bin\run_test.ps1 -json -name prog15-4-85 -action tokens -language "COBOL-85 pascal pl1" -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\detect-tokens\PROG15-4.txt"

# COBOL-2002
test\bin\run_test.ps1 -json -name report-card-cob2002 -action tokens -language "COBOL-2002 java rust" -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\detect-tokens\ReportCard.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action tokens -language "Cplusplus visualbasic-6" -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\detect-tokens\checkers.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action tokens -language "Csharp Fsharp Java" -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\detect-tokens\calculator1.txt"

# FORTRAN-66
test\bin\run_test.ps1 -json -name heron2-ftn66 -action tokens -language "FORTRAN-66 cobol-68 ruby" -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\detect-tokens\HERON2.txt"

# FORTRAN-77
test\bin\run_test.ps1 -json -name heron-ftn77 -action tokens -language "FORTRAN-77 pascal sql-92" -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\detect-tokens\HERON.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action tokens -language "Fortran-2003 go javascript" -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\detect-tokens\geo4060.txt"

# F#
test\bin\run_test.ps1 -json -name samples-fs -action tokens -language "Fsharp r basic" -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\detect-tokens\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action tokens -language "Go basic c swift" -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\detect-tokens\find-cli.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action tokens -language "HTML basic java" -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\detect-tokens\knuth.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action tokens -language "Java basic csharp fsharp" -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\detect-tokens\prime_test.txt"

# JavaScript
test\bin\run_test.ps1 -json -name codestat-js -action tokens -language "JavaScript html" -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\detect-tokens\codestat.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action tokens -language "Objective-C c cplusplus swift" -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\detect-tokens\HelloWorld.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework-pas -action tokens -language "Pascal fortran-77 basic ruby" -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect-tokens\FIREWORK.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action tokens -language "PL1 fortran-77 cobol-85" -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\detect-tokens\BPGPLI.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main-prolog -action tokens -language "Prolog basic ruby html" -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\detect-tokens\family-main.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d-python -action tokens -language "Python Java Rust Ruby" -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\detect-tokens\drone_3d_trajectory_following.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01-r -action tokens -language "R Fortran-77 basic" -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\detect-tokens\ETM-540-01.txt"

# Ruby
test\bin\run_test.ps1 -json -name basic-ruby -action tokens -language "Ruby basic rust sql-92" -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\detect-tokens\basic.txt"

# Rust
test\bin\run_test.ps1 -json -name dom-rust -action tokens -language "Rust Ruby Basic" -inputfile ".\test\data\Rust\dom.rs" -expected ".\test\ref\detect-tokens\dom.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action tokens -language "SQL-92 basic cobol-85" -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\detect-tokens\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action tokens -language "SQL-92 fortran-77 cbasic html" -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\detect-tokens\table.txt"

# Swift
test\bin\run_test.ps1 -json -name MealTableViewCell -action tokens -language "Swift basic sql-92" -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\detect-tokens\MealTableViewCell.txt"

# TypeScript
test\bin\run_test.ps1 -json -name TimeReporter-ts -action tokens -language "TypeScript JavaScript Java Basic" -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\detect-tokens\TimeReporter.txt"

# Visual Basic 6
test\bin\run_test.ps1 -json -name spider-vb6 -action tokens -language "VisualBasic-6 basic cbasic ruby" -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\detect-tokens\spider.txt"

# Visual Basic .NET
test\bin\run_test.ps1 -json -name word-processor-vbnet -action tokens -language "VisualBasic-NET basic cbasic ruby rust" -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\detect-tokens\WordProcessor.txt"

# polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c -action tokens -language "C COBOL-85 Pascal Fortran-66" -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect-tokens\polyglot.txt"

# non-code files
test\bin\run_test.ps1 -json -name noncode-empty -action tokens -inputfile ".\test\data\noncode\empty.txt" -expected ".\test\ref\detect-tokens\noncode\empty.txt"
test\bin\run_test.ps1 -json -name noncode-whitespace -action tokens -inputfile ".\test\data\noncode\whitespace.txt" -expected ".\test\ref\detect-tokens\noncode\whitespace.txt"
test\bin\run_test.ps1 -json -name noncode-win-ini -action tokens -inputfile ".\test\data\noncode\win.ini" -expected ".\test\ref\detect-tokens\noncode\win.txt"
test\bin\run_test.ps1 -json -name noncode-iis -action tokens -inputfile ".\test\data\noncode\iis.log" -expected ".\test\ref\detect-tokens\noncode\iis.txt"
test\bin\run_test.ps1 -json -name noncode-setutc -action tokens -inputfile ".\test\data\noncode\setutc.reg" -expected ".\test\ref\detect-tokens\noncode\setutc.txt"
test\bin\run_test.ps1 -json -name noncode-diagwrn -action tokens -inputfile ".\test\data\noncode\diagwrn.xml" -expected ".\test\ref\detect-tokens\noncode\diagwrn.txt"
test\bin\run_test.ps1 -json -name noncode-profile-xlsx -action tokens -inputfile ".\test\data\noncode\profile.xlsx" -expected ".\test\ref\detect-tokens\noncode\profile.txt"
test\bin\run_test.ps1 -json -name noncode-resume-docx -action tokens -inputfile ".\test\data\noncode\resume.docx" -expected ".\test\ref\detect-tokens\noncode\resume.txt"
test\bin\run_test.ps1 -json -name noncode-hh -action tokens -inputfile ".\test\data\noncode\hh.exe" -expected ".\test\ref\detect-tokens\noncode\hh.txt"
