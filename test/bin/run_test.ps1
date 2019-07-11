Param
(
    [string]$name,
    [string]$language,
    [string]$comment,
    [switch]$wide,
    [string]$languages,
    [string]$action,
    [switch]$errors,
    [string]$inputfile,
    [string]$expected,
    [switch]$json,
    [int32]$tabsize,
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

if ($PSBoundParameters.ContainsKey('comment')) {
    $params += "comment=$comment"
}

if ($wide) {
    $params += "wide"
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

if ($json) {
    Write-Output "Invoking service $url -infile $inputfile -outfile $actual..."
    Invoke-RestMethod -Method Post -Uri "$url" -InFile $inputfile | ConvertTo-JSON -Compress -Depth 4 | Out-File $actual
    Write-Output "Adjusting JSON output..."
    Get-Content $actual | python AddNlToJson.py | python IndentJson.py | Out-File $actual_adjusted
    dotnet test\bin\UTF16-UTF8.dll "$actual_adjusted" "$actual_final"
} else {
    Write-Output "Invoking service $url -infile $inputfile -outfile $actual..."
    Invoke-RestMethod -Method Post -Uri "$url" -InFile $inputfile | Out-File $actual
    Get-Content $actual | Out-File $actual_final
}

Write-Output "Comparing $actual_final against $expected..."
if (Compare-Object $(Get-Content $expected) $(Get-Content $actual_final)) {
    Write-Output ""
    Set-Variable -name failures -value ($failures + 1) -scope 1
    "Test $name failed"
    Copy-Item $actual_final $expected
} else {
    Write-Output ""
    "Test $name passed"
}

Write-Output "****** ****** ******"
Write-Output ""
