Set-StrictMode -Version Latest

Set-Variable -Name action "detect"

# PL/1
test\bin\run_detect_test.ps1 -json -name bpgpli-pl1 -inputfile ".\test\data\PL1\BPGPLI.pl1" -expected ".\test\ref\detect\PL1\BPGPLI.txt"
test\bin\run_detect_test.ps1 -json -name checkdt-pl1 -inputfile ".\test\data\PL1\CHECKDT.pl1" -expected ".\test\ref\detect\PL1\CHECKDT.txt"
test\bin\run_detect_test.ps1 -json -name crtpln3-pl1 -inputfile ".\test\data\PL1\CRTPLN3.pl1" -expected ".\test\ref\detect\PL1\CRTPLN3.txt"
test\bin\run_detect_test.ps1 -json -name mainfact-pl1 -inputfile ".\test\data\PL1\MAINFACT.pl1" -expected ".\test\ref\detect\PL1\MAINFACT.txt"
test\bin\run_detect_test.ps1 -json -name example-pl1 -inputfile ".\test\data\PL1\example.pl1" -expected ".\test\ref\detect\PL1\example.txt"
test\bin\run_detect_test.ps1 -json -name digrams-pl1 -inputfile ".\test\data\PL1\digrams.pl1" -expected ".\test\ref\detect\PL1\digrams.txt"

# PL/M
Set-Variable -Name language "PLM"
test\bin\run_detect_test.ps1 -json -name example-plm -inputfile ".\test\data\$language\example.plm" -expected ".\test\ref\$action\$language\example.txt"

# Prolog
test\bin\run_detect_test.ps1 -json -name family-main -inputfile ".\test\data\Prolog\family-main.pl" -expected ".\test\ref\detect\Prolog\family-main.txt"
test\bin\run_detect_test.ps1 -json -name family-kb -inputfile ".\test\data\Prolog\family-kb.pl" -expected ".\test\ref\detect\Prolog\family-kb.txt"
test\bin\run_detect_test.ps1 -json -name family-menu -inputfile ".\test\data\Prolog\family-menu.pl" -expected ".\test\ref\detect\Prolog\family-menu.txt"
test\bin\run_detect_test.ps1 -json -name family-queries -inputfile ".\test\data\Prolog\family-queries.pl" -expected ".\test\ref\detect\Prolog\family-queries.txt"
test\bin\run_detect_test.ps1 -json -name web-server-hello-prolog -inputfile ".\test\data\Prolog\web-server-hello.pl" -expected ".\test\ref\detect\Prolog\web-server-hello.txt"
test\bin\run_detect_test.ps1 -json -name web-server-params-prolog -inputfile ".\test\data\Prolog\web-server-params.pl" -expected ".\test\ref\detect\Prolog\web-server-params.txt"

# Python
test\bin\run_detect_test.ps1 -json -name drone-3d -inputfile ".\test\data\Python\drone_3d_trajectory_following.py" -expected ".\test\ref\detect\Python\drone_3d_trajectory_following.txt"
test\bin\run_detect_test.ps1 -json -name quadrotor -inputfile ".\test\data\Python\Quadrotor.py" -expected ".\test\ref\detect\Python\Quadrotor.txt"
test\bin\run_detect_test.ps1 -json -name trajectory -inputfile ".\test\data\Python\TrajectoryGenerator.py" -expected ".\test\ref\detect\Python\TrajectoryGenerator.txt"
test\bin\run_detect_test.ps1 -json -name values-python -inputfile ".\test\data\Python\values.py" -expected ".\test\ref\detect\Python\values.txt"
test\bin\run_detect_test.ps1 -json -name examiner -inputfile ".\test\data\Python\Examiner.py" -expected ".\test\ref\detect\Python\Examiner.txt"
test\bin\run_detect_test.ps1 -json -name authorized-view-python -inputfile ".\test\data\Python\authorized_view.py" -expected ".\test\ref\detect\Python\authorized_view.txt"

# R
test\bin\run_detect_test.ps1 -json -name ETM-540-01 -inputfile ".\test\data\R\ETM-540-01.R" -expected ".\test\ref\detect\R\ETM-540-01.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-02 -inputfile ".\test\data\R\ETM-540-02.R" -expected ".\test\ref\detect\R\ETM-540-02.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-03 -inputfile ".\test\data\R\ETM-540-03.R" -expected ".\test\ref\detect\R\ETM-540-03.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-04 -inputfile ".\test\data\R\ETM-540-04.R" -expected ".\test\ref\detect\R\ETM-540-04.txt"
test\bin\run_detect_test.ps1 -json -name ETM-540-05 -inputfile ".\test\data\R\ETM-540-05.R" -expected ".\test\ref\detect\R\ETM-540-05.txt"
test\bin\run_detect_test.ps1 -json -name basketball-r -inputfile ".\test\data\R\basketball.R" -expected ".\test\ref\detect\R\basketball.txt"
test\bin\run_detect_test.ps1 -json -name render-r -inputfile ".\test\data\R\render.R" -expected ".\test\ref\detect\R\render.txt"

# Ruby
test\bin\run_detect_test.ps1 -json -name basic-ruby -inputfile ".\test\data\Ruby\basic.rb" -expected ".\test\ref\detect\Ruby\basic.txt"
test\bin\run_detect_test.ps1 -json -name constants-ruby -inputfile ".\test\data\Ruby\constants.rb" -expected ".\test\ref\detect\Ruby\constants.txt"
test\bin\run_detect_test.ps1 -json -name exceptions-ruby -inputfile ".\test\data\Ruby\exceptions.rb" -expected ".\test\ref\detect\Ruby\exceptions.txt"
test\bin\run_detect_test.ps1 -json -name expressions-ruby -inputfile ".\test\data\Ruby\expressions.rb" -expected ".\test\ref\detect\Ruby\expressions.txt"
test\bin\run_detect_test.ps1 -json -name functions-ruby -inputfile ".\test\data\Ruby\functions.rb" -expected ".\test\ref\detect\Ruby\functions.txt"
test\bin\run_detect_test.ps1 -json -name io-ruby -inputfile ".\test\data\Ruby\io.rb" -expected ".\test\ref\detect\Ruby\io.txt"
test\bin\run_detect_test.ps1 -json -name modifiers-ruby -inputfile ".\test\data\Ruby\modifiers.rb" -expected ".\test\ref\detect\Ruby\modifiers.txt"
test\bin\run_detect_test.ps1 -json -name operators-ruby -inputfile ".\test\data\Ruby\operators.rb" -expected ".\test\ref\detect\Ruby\operators.txt"
test\bin\run_detect_test.ps1 -json -name statements-ruby -inputfile ".\test\data\Ruby\statements.rb" -expected ".\test\ref\detect\Ruby\statements.txt"
test\bin\run_detect_test.ps1 -json -name tokenbuilders-ruby -inputfile ".\test\data\Ruby\tokenbuilders.rb" -expected ".\test\ref\detect\Ruby\tokenbuilders.txt"
test\bin\run_detect_test.ps1 -json -name tokenizers-ruby -inputfile ".\test\data\Ruby\tokenizers.rb" -expected ".\test\ref\detect\Ruby\tokenizers.txt"
test\bin\run_detect_test.ps1 -json -name tokens-ruby -inputfile ".\test\data\Ruby\tokens.rb" -expected ".\test\ref\detect\Ruby\tokens.txt"
test\bin\run_detect_test.ps1 -json -name webhook-ruby -inputfile ".\test\data\Ruby\webhook.rb" -expected ".\test\ref\detect\Ruby\webhook.txt"

# Rust
Set-Variable -Name language "Rust"
test\bin\run_detect_test.ps1 -json -name literals-$language -inputfile ".\test\data\$language\literals.rs" -expected ".\test\ref\$action\$language\literals.txt"
test\bin\run_detect_test.ps1 -json -name dom-$language -inputfile ".\test\data\$language\dom.rs" -expected ".\test\ref\$action\$language\dom.txt"
test\bin\run_detect_test.ps1 -json -name html-$language -inputfile ".\test\data\$language\html.rs" -expected ".\test\ref\$action\$language\html.txt"
test\bin\run_detect_test.ps1 -json -name geometry-$language -inputfile ".\test\data\$language\geometry.rs" -expected ".\test\ref\$action\$language\geometry.txt"
test\bin\run_detect_test.ps1 -json -name scene-$language -inputfile ".\test\data\$language\scene.rs" -expected ".\test\ref\$action\$language\scene.txt"
test\bin\run_detect_test.ps1 -json -name comments-1-$language -inputfile ".\test\data\$language\nested_comments_1.rs" -expected ".\test\ref\$action\$language\nested_comments_1.txt"
test\bin\run_detect_test.ps1 -json -name comments-2-$language -inputfile ".\test\data\$language\nested_comments_2.rs" -expected ".\test\ref\$action\$language\nested_comments_2.txt"
test\bin\run_detect_test.ps1 -json -name comments-3-$language -inputfile ".\test\data\$language\nested_comments_3.rs" -expected ".\test\ref\$action\$language\nested_comments_3.txt"
test\bin\run_detect_test.ps1 -json -name raw-string-1-$language -inputfile ".\test\data\$language\raw_string_1.rs" -expected ".\test\ref\$action\$language\raw_string_1.txt"
test\bin\run_detect_test.ps1 -json -name raw-string-2-$language -inputfile ".\test\data\$language\raw_string_2.rs" -expected ".\test\ref\$action\$language\raw_string_2.txt"
test\bin\run_detect_test.ps1 -json -name attributes-1-$language -inputfile ".\test\data\$language\attributes_1.rs" -expected ".\test\ref\$action\$language\attributes_1.txt"
test\bin\run_detect_test.ps1 -json -name chip8-$language -inputfile ".\test\data\$language\chip8.rs" -expected ".\test\ref\$action\$language\chip8.txt"
test\bin\run_detect_test.ps1 -json -name chip8-display-$language -inputfile ".\test\data\$language\chip8-display.rs" -expected ".\test\ref\$action\$language\chip8-display.txt"
test\bin\run_detect_test.ps1 -json -name chip8-instructions-$language -inputfile ".\test\data\$language\chip8-instructions.rs" -expected ".\test\ref\$action\$language\chip8-instructions.txt"
test\bin\run_detect_test.ps1 -json -name chip8-main-$language -inputfile ".\test\data\$language\chip8-main.rs" -expected ".\test\ref\$action\$language\chip8-main.txt"
test\bin\run_detect_test.ps1 -json -name weird-$language -inputfile ".\test\data\$language\weird.rs" -expected ".\test\ref\$action\$language\weird.txt"

# Scala
Set-Variable -Name language "Scala"
test\bin\run_detect_test.ps1 -json -name hello-$language -inputfile ".\test\data\$language\hello.scala" -expected ".\test\ref\$action\$language\hello.txt"
test\bin\run_detect_test.ps1 -json -name larger-$language -inputfile ".\test\data\$language\larger.scala" -expected ".\test\ref\$action\$language\larger.txt"
test\bin\run_detect_test.ps1 -json -name random-$language -inputfile ".\test\data\$language\random.scala" -expected ".\test\ref\$action\$language\random.txt"
test\bin\run_detect_test.ps1 -json -name variables-$language -inputfile ".\test\data\$language\variables.scala" -expected ".\test\ref\$action\$language\variables.txt"
test\bin\run_detect_test.ps1 -json -name zero_line-$language -inputfile ".\test\data\$language\zero_line_hello_world.scala" -expected ".\test\ref\$action\$language\zero_line_hello_world.txt"
test\bin\run_detect_test.ps1 -json -name v3-$language -inputfile ".\test\data\$language\v3_control.$language" -expected ".\test\ref\$action\$language\v3_control.txt"

# SQL
test\bin\run_detect_test.ps1 -json -name microsoft-sql -inputfile ".\test\data\SQL\microsoft.sql" -expected ".\test\ref\detect\SQL\microsoft.txt"
test\bin\run_detect_test.ps1 -json -name table-sql -inputfile ".\test\data\SQL\table.sql" -expected ".\test\ref\detect\SQL\table.txt"

# T-SQL
test\bin\run_detect_test.ps1 -json -name brackets-tsql -inputfile ".\test\data\T-SQL\brackets.sql" -expected ".\test\ref\detect\T-SQL\brackets.txt"

# Swift
test\bin\run_detect_test.ps1 -json -name AppDelegate -inputfile ".\test\data\Swift\AppDelegate.swift" -expected ".\test\ref\detect\Swift\AppDelegate.txt"
test\bin\run_detect_test.ps1 -json -name Meal -inputfile ".\test\data\Swift\Meal.swift" -expected ".\test\ref\detect\Swift\Meal.txt"
test\bin\run_detect_test.ps1 -json -name MealTableViewCell -inputfile ".\test\data\Swift\MealTableViewCell.swift" -expected ".\test\ref\detect\Swift\MealTableViewCell.txt"
test\bin\run_detect_test.ps1 -json -name MealTableViewController -inputfile ".\test\data\Swift\MealTableViewController.swift" -expected ".\test\ref\detect\Swift\MealTableViewController.txt"
test\bin\run_detect_test.ps1 -json -name MealViewController -inputfile ".\test\data\Swift\MealViewController.swift" -expected ".\test\ref\detect\Swift\MealViewController.txt"
test\bin\run_detect_test.ps1 -json -name RatingControl -inputfile ".\test\data\Swift\RatingControl.swift" -expected ".\test\ref\detect\Swift\RatingControl.txt"
test\bin\run_detect_test.ps1 -json -name URLExtensions -inputfile ".\test\data\Swift\URLExtensions.swift" -expected ".\test\ref\detect\Swift\URLExtensions.txt"

# TypeScript
test\bin\run_detect_test.ps1 -json -name TimeReporter-ts -inputfile ".\test\data\TypeScript\TimeReporter.ts" -expected ".\test\ref\detect\TypeScript\TimeReporter.txt"
test\bin\run_detect_test.ps1 -json -name ImageBoard-ts -inputfile ".\test\data\TypeScript\ImageBoard.ts" -expected ".\test\ref\detect\TypeScript\ImageBoard.txt"

# Visual Basic 6
test\bin\run_detect_test.ps1 -json -name spider-vb6 -inputfile ".\test\data\VisualBasic-6\spider.bas" -expected ".\test\ref\detect\VisualBasic-6\spider.txt"
test\bin\run_detect_test.ps1 -json -name azure_metadata-vb6 -inputfile ".\test\data\VisualBasic-6\azure_metadata.bas" -expected ".\test\ref\detect\VisualBasic-6\azure_metadata.txt"
test\bin\run_detect_test.ps1 -json -name diffie_hellman-vb6 -inputfile ".\test\data\VisualBasic-6\diffie_hellman.bas" -expected ".\test\ref\detect\VisualBasic-6\diffie_hellman.txt"

# Visual Basic .NET
test\bin\run_detect_test.ps1 -json -name word-processor-vbnet -inputfile ".\test\data\VisualBasic-NET\WordProcessor.bas" -expected ".\test\ref\detect\VisualBasic-NET\WordProcessor.txt"

# polyglot programs
test\bin\run_detect_test.ps1 -json -name polyglot -notiebreak -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect\multiple\polyglot.txt"
test\bin\run_detect_test.ps1 -json -name polyglot-languages -notiebreak -language C-78,Cplusplus,COBOL-68,COBOL-74,cobol-85,objective-c,Pascal -inputfile ".\test\data\multiple\polyglot.txt" -expected ".\test\ref\detect\multiple\polyglot-languages.txt"
test\bin\run_detect_test.ps1 -json -name polyglot-py-rb -notiebreak -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\detect\multiple\polyglot-py-rb.txt"
test\bin\run_detect_test.ps1 -json -name polyglot-py-rb-languages -notiebreak -language python,ruby -inputfile ".\test\data\multiple\polyglot-py-rb.txt" -expected ".\test\ref\detect\multiple\polyglot-py-rb-languages.txt"

# non-code files
test\bin\run_detect_test.ps1 -json -name noncode-empty -inputfile ".\test\data\noncode\empty.txt" -expected ".\test\ref\detect\noncode\empty.txt"
test\bin\run_detect_test.ps1 -json -name noncode-whitespace -inputfile ".\test\data\noncode\whitespace.txt" -expected ".\test\ref\detect\noncode\whitespace.txt"
test\bin\run_detect_test.ps1 -json -name noncode-win-ini -inputfile ".\test\data\noncode\win.ini" -expected ".\test\ref\detect\noncode\win.txt"
test\bin\run_detect_test.ps1 -json -name noncode-setutc -inputfile ".\test\data\noncode\setutc.reg" -expected ".\test\ref\detect\noncode\setutc.txt"
test\bin\run_detect_test.ps1 -json -name noncode-diagwrn -inputfile ".\test\data\noncode\diagwrn.xml" -expected ".\test\ref\detect\noncode\diagwrn.txt"
test\bin\run_detect_test.ps1 -json -name noncode-profile-xlsx -inputfile ".\test\data\noncode\profile.xlsx" -expected ".\test\ref\detect\noncode\profile.txt"
test\bin\run_detect_test.ps1 -json -name noncode-resume-docx -inputfile ".\test\data\noncode\resume.docx" -expected ".\test\ref\detect\noncode\resume.txt"
test\bin\run_detect_test.ps1 -json -name noncode-hh -inputfile ".\test\data\noncode\hh.exe" -expected ".\test\ref\detect\noncode\hh.txt"
