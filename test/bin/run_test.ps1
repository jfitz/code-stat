Param
(
    [string]$name,
    [string]$language,
    [string]$action,
    [string]$inputfile,
    [string]$expected,
    [switch]$json
)

Write-Output "****** ****** ******"
Write-Output "Starting test $name..."
Write-Output ""

$actual = ".\tests\$name\out.txt"
$actual_adjusted = ".\tests\$name\out1.txt"
$actual_final = ".\tests\$name\out2.txt"
$target = "localhost:5000"
$params = ""

if (![string]::IsNullOrEmpty($language)) {
    $params = "language=$language"
}

if ([string]::IsNullOrEmpty($params)) {
    $url = "http://" + $target + "/" + $action
} else {
    $url = "http://" + $target + "/" + $action + "?" + $params
}

if (!(Test-Path -Path .\tests\$name )) {
    New-Item -ItemType directory -Path .\tests\$name
}
Remove-Item .\tests\$name\*.*

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
