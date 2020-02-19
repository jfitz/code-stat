Set-StrictMode -Version Latest

# Ada-83
test\bin\run_test.ps1 -json -name adabkend-adb-83 -action confidence -errors -language "ada-83 ada-95 basic c pl1-fixed swift" -inputfile ".\test\data\Ada-83\adabkend.adb" -expected ".\test\ref\detect-confidence-errors\adabkend-adb.txt"

# BASIC
test\bin\run_test.ps1 -json -name values-bas -action confidence -errors -language "BASIC CBASIC pascal python" -inputfile ".\test\data\BASIC\values.bas" -expected ".\test\ref\detect-confidence-errors\values.txt"
test\bin\run_test.ps1 -json -name batnum-bas -action confidence -errors -language "BASIC cobol-68 fortran-66 c" -inputfile ".\test\data\BASIC\batnum.bas" -expected ".\test\ref\detect-confidence-errors\batnum.txt"
test\bin\run_test.ps1 -json -name life-bas -action confidence -errors -language "BASIC BASICA ruby prolog" -inputfile ".\test\data\BASIC\life.bas" -expected ".\test\ref\detect-confidence-errors\life.txt"

# C-99
test\bin\run_test.ps1 -json -name parser-h-99 -action confidence -errors -language "C python swift" -inputfile ".\test\data\C-99\parser.h" -expected ".\test\ref\detect-confidence-errors\parser-h.txt"
test\bin\run_test.ps1 -json -name parser-c-99 -action confidence -errors -language "C C++ pascal" -inputfile ".\test\data\C-99\parser.c" -expected ".\test\ref\detect-confidence-errors\parser-c.txt"

# CBASIC
test\bin\run_test.ps1 -json -name fibo -action confidence -errors -language "CBASIC BASIC pascal" -inputfile ".\test\data\CBASIC\fibo.bas" -expected ".\test\ref\detect-confidence-errors\fibo.txt"

# COBOL-68
test\bin\run_test.ps1 -json -name mccracken3-68 -action confidence -errors -language "COBOL-68 cobol-85 fortran-77" -inputfile ".\test\data\COBOL-68\mccracken3.cob" -expected ".\test\ref\detect-confidence-errors\mccracken3.txt"

# COBOL-85
test\bin\run_test.ps1 -json -name prog15-4-85 -action confidence -errors -language "COBOL-85 pascal pl1-fixed" -inputfile ".\test\data\COBOL-85\PROG15-4.COB" -expected ".\test\ref\detect-confidence-errors\PROG15-4.txt"

# COBOL-2002
test\bin\run_test.ps1 -json -name report-card-cob2002 -action confidence -errors -language "COBOL-2002 java rust" -tabsize 4 -inputfile ".\test\data\COBOL-2002\ReportCard.cob" -expected ".\test\ref\detect-confidence-errors\ReportCard.txt"

# C++
test\bin\run_test.ps1 -json -name checkers-cpp -action confidence -errors -language "Cplusplus visualbasic-6" -inputfile ".\test\data\Cplusplus\checkers.cpp" -expected ".\test\ref\detect-confidence-errors\checkers.txt"

# C#
test\bin\run_test.ps1 -json -name calculator1-cs -action confidence -errors -language "Csharp Fsharp Java" -inputfile ".\test\data\Csharp\calculator1.cs" -expected ".\test\ref\detect-confidence-errors\calculator1.txt"

# FORTRAN-66
test\bin\run_test.ps1 -json -name heron2-ftn66 -action confidence -errors -language "FORTRAN-66 cobol-68 ruby" -inputfile ".\test\data\FORTRAN-66\HERON2.FOR" -expected ".\test\ref\detect-confidence-errors\HERON2.txt"

# FORTRAN-77
test\bin\run_test.ps1 -json -name heron-ftn77 -action confidence -errors -language "FORTRAN-77 pascal sql-92" -inputfile ".\test\data\FORTRAN-77\HERON.F77" -expected ".\test\ref\detect-confidence-errors\HERON.txt"

# Fortran-2003
test\bin\run_test.ps1 -json -name geo4060-ftn2003 -action confidence -errors -language "Fortran-2003 go javascript" -inputfile ".\test\data\Fortran-2003\geo4060.for" -expected ".\test\ref\detect-confidence-errors\geo4060.txt"

# F#
test\bin\run_test.ps1 -json -name samples-fs -action confidence -errors -language "Fsharp r basic" -inputfile ".\test\data\Fsharp\samples.fs" -expected ".\test\ref\detect-confidence-errors\samples.txt"

# Go
test\bin\run_test.ps1 -json -name find-cli-go -action confidence -errors -language "Go basic c swift" -inputfile ".\test\data\Go\find-cli.go" -expected ".\test\ref\detect-confidence-errors\find-cli.txt"

# HTML
test\bin\run_test.ps1 -json -name knuth-html -action confidence -errors -language "HTML basic java" -inputfile ".\test\data\HTML\knuth.html" -expected ".\test\ref\detect-confidence-errors\knuth.txt"

# Java
test\bin\run_test.ps1 -json -name prime_test-java -action confidence -errors -language "Java basic csharp fsharp" -inputfile ".\test\data\Java\prime_test.java" -expected ".\test\ref\detect-confidence-errors\prime_test.txt"

# JavaScript
test\bin\run_test.ps1 -json -name codestat-js -action confidence -errors -language "JavaScript html" -inputfile ".\test\data\JavaScript\codestat.js" -expected ".\test\ref\detect-confidence-errors\codestat.txt"

# Objective-C
test\bin\run_test.ps1 -json -name hello-objc -action confidence -errors -language "Objective-C c cplusplus swift" -inputfile ".\test\data\Objective-C\HelloWorld.objc" -expected ".\test\ref\detect-confidence-errors\HelloWorld.txt"

# Pascal
test\bin\run_test.ps1 -json -name firework-pas -action confidence -errors -language "Pascal fortran-77 basic ruby" -inputfile ".\test\data\Pascal\FIREWORK.PAS" -expected ".\test\ref\detect-confidence-errors\FIREWORK.txt"

# PL/1
test\bin\run_test.ps1 -json -name bpgpli-pl1 -action confidence -errors -language "PL1-Fixed fortran-77 cobol-85" -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\detect-confidence-errors\BPGPLI.txt"

# Prolog
test\bin\run_test.ps1 -json -name family-main-prolog -action confidence -errors -language "Prolog basic ruby html" -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\detect-confidence-errors\family-main.txt"

# Python
test\bin\run_test.ps1 -json -name drone-3d-python -action confidence -errors -language "Python Java Rust Ruby" -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\detect-confidence-errors\drone_3d_trajectory_following.txt"

# R
test\bin\run_test.ps1 -json -name ETM-540-01-r -action confidence -errors -language "R Fortran-77 basic" -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\detect-confidence-errors\ETM-540-01.txt"

# Ruby
test\bin\run_test.ps1 -json -name basic-ruby -action confidence -errors -language "Ruby basic rust sql-92" -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\detect-confidence-errors\basic.txt"

# Rust
test\bin\run_test.ps1 -json -name dom-rust -action confidence -errors -language "Rust Ruby Basic" -inputfile ".\test\data\Rust\dom.rs" -expected ".\test\ref\detect-confidence-errors\dom.txt"

# SQL-92
test\bin\run_test.ps1 -json -name microsoft-sql-92 -action confidence -errors -language "SQL-92 basic cobol-85" -inputfile ".\test\data\SQL-92\microsoft.sql" -expected ".\test\ref\detect-confidence-errors\microsoft.txt"
test\bin\run_test.ps1 -json -name table-sql-92 -action confidence -errors -language "SQL-92 fortran-77 cbasic html" -inputfile ".\test\data\SQL-92\table.sql" -expected ".\test\ref\detect-confidence-errors\table.txt"

# Swift
test\bin\run_test.ps1 -json -name MealTableViewCell -action confidence -errors -language "Swift basic sql-92" -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\detect-confidence-errors\MealTableViewCell.txt"

# TypeScript
test\bin\run_test.ps1 -json -name TimeReporter-ts -action confidence -errors -language "TypeScript JavaScript Java Basic" -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\detect-confidence-errors\TimeReporter.txt"

# Visual Basic 6
test\bin\run_test.ps1 -json -name spider-vb6 -action confidence -errors -language "VisualBasic-6 basic cbasic ruby" -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\detect-confidence-errors\spider.txt"

# Visual Basic .NET
test\bin\run_test.ps1 -json -name word-processor-vbnet -action confidence -errors -language "VisualBasic-NET basic cbasic ruby rust" -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\detect-confidence-errors\WordProcessor.txt"

# polyglot programs
test\bin\run_test.ps1 -json -name polyglot-c -action confidence -errors -language "C COBOL-85 Pascal Fortran-66" -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect-confidence-errors\polyglot.txt"

# non-code files
test\bin\run_test.ps1 -json -name noncode-empty -action confidence -errors -inputfile ".\test\data\noncode\empty.txt" -expected ".\test\ref\detect-confidence-errors\noncode\empty.txt"
test\bin\run_test.ps1 -json -name noncode-whitespace -action confidence -errors -inputfile ".\test\data\noncode\whitespace.txt" -expected ".\test\ref\detect-confidence-errors\noncode\whitespace.txt"
test\bin\run_test.ps1 -json -name noncode-win-ini -action confidence -errors -inputfile ".\test\data\noncode\win.ini" -expected ".\test\ref\detect-confidence-errors\noncode\win.txt"
test\bin\run_test.ps1 -json -name noncode-iis -action confidence -errors -inputfile ".\test\data\noncode\iis.log" -expected ".\test\ref\detect-confidence-errors\noncode\iis.txt"
test\bin\run_test.ps1 -json -name noncode-setutc -action confidence -errors -inputfile ".\test\data\noncode\setutc.reg" -expected ".\test\ref\detect-confidence-errors\noncode\setutc.txt"
test\bin\run_test.ps1 -json -name noncode-diagwrn -action confidence -errors -inputfile ".\test\data\noncode\diagwrn.xml" -expected ".\test\ref\detect-confidence-errors\noncode\diagwrn.txt"
test\bin\run_test.ps1 -json -name noncode-profile-xlsx -action confidence -errors -inputfile ".\test\data\noncode\profile.xlsx" -expected ".\test\ref\detect-confidence-errors\noncode\profile.txt"
test\bin\run_test.ps1 -json -name noncode-resume-docx -action confidence -errors -inputfile ".\test\data\noncode\resume.docx" -expected ".\test\ref\detect-confidence-errors\noncode\resume.txt"
test\bin\run_test.ps1 -json -name noncode-hh -action confidence -errors -inputfile ".\test\data\noncode\hh.exe" -expected ".\test\ref\detect-confidence-errors\noncode\hh.txt"
