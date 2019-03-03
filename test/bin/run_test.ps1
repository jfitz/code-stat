Param
(
    [string]$name,
    [string]$language,
    [string]$languages,
    [string]$action,
    [string]$inputfile,
    [string]$expected,
    [switch]$json,
    [int32]$tabsize,
    [switch]$errors,
    [switch]$wide,
    [switch]$tiebreak
)

Write-Output "****** ****** ******"
Write-Output "Starting test $name..."
Write-Output ""

$testbed = "$env:USERPROFILE\Projects\CodeStat"
$actual = "$testbed\$name\out.txt"
$actual_adjusted = "$testbed\$name\out1.txt"
$actual_final = "$testbed\$name\out2.txt"
$target = "localhost:5000"
$params = @()

if ($PSBoundParameters.ContainsKey('language')) {
    $params += "language=$language"
}

if ($PSBoundParameters.ContainsKey('languages')) {
    $params += "languages=$languages"
}

if ($PSBoundParameters.ContainsKey('tabsize')) {
    $params += "tabsize=$tabsize"
}

if ($errors) {
    $params += "errors"
}

if ($wide) {
    $params += "wide"
}

if ($tiebreak) {
    $params += "tiebreak-keywords"
}

if ($params.Count -gt 0) {
        $paramstext = $params -join '&'
    $url = "http://" + $target + "/" + $action + "?" + $paramstext
} else {
    $url = "http://" + $target + "/" + $action
}

if (!(Test-Path -Path $testbed\$name )) {
    New-Item -ItemType directory -Path $testbed\$name
}
Remove-Item $testbed\$name\*.*

Write-Output "Invoking service $url -infile $inputfile -outfile $actual..."
Invoke-RestMethod -method post -uri "$url" -infile $inputfile -outfile $actual

if ($json) {
    Write-Output "Adjusting JSON output..."
    Get-Content $actual | python AddNlToJson.py | python IndentJson.py | Out-File $actual_adjusted
    dotnet test\bin\UTF16-UTF8.dll "$actual_adjusted" "$actual_final"
} else {
    Get-Content $actual | Out-File $actual_final
}

Write-Output "Comparing $actual_final against $expected..."
if (Compare-Object $(Get-Content $expected) $(Get-Content $actual_final)) {
    Write-Output ""
    set-variable -name failures -value ($failures + 1) -scope 1
    "Test $name failed"
    Copy-Item $actual_final $expected
} else {
    Write-Output ""
    "Test $name passed"
}

Write-Output "****** ****** ******"
Write-Output ""
