Param
(
    [string]$name,
    [string]$language,
    [string]$comment,
    [switch]$wide,
    [string]$action,
    [switch]$errors,
    [string]$inputfile,
    [string]$expected,
    [switch]$json,
    [int32]$tabsize,
    [switch]$notiebreak
)

function Request-CodeStat([string]$url, [string]$inputfile) {
    if ($PSVersionTable.PSVersion.Major -ge 7) {
        Invoke-RestMethod -Method Post -Uri "$url" -InFile $inputfile -SkipHttpErrorCheck
    } else {
        Invoke-RestMethod -Method Post -Uri "$url" -InFile $inputfile
    }
}

function Optimize-Json([string]$input_file, [string]$inter_file, [string]$output_file) {
    if ($PSVersionTable.PSVersion.Major -ge 7) {
        Get-Content $input_file | python AddNlToJson.py | python IndentJson.py | Out-File $output_file
    } else {
        Get-Content $input_file | python AddNlToJson.py | python IndentJson.py | Out-File $inter_file
        dotnet test\bin\UTF16-UTF8.dll "$inter_file" "$output_file"
    }
}

Write-Output "****** ****** ******"
Write-Output "Starting test $name..."
Write-Output ""

$testbed = "$env:USERPROFILE\Projects\CodeStat"
$actual = "$testbed\$name\out.txt"
$actual_adjusted = "$testbed\$name\out1.txt"
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

if ($PSBoundParameters.ContainsKey('tabsize')) {
    $params += "tabsize=$tabsize"
}

if ($errors) {
    $params += "errors"
}

if ($notiebreak) {
    $params += "notiebreak"
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
    Request-CodeStat $url $inputfile | ConvertTo-JSON -Compress -Depth 4 | Out-File $actual
    Write-Output "Adjusting JSON output..."
    Optimize-Json $actual $actual_adjusted $expected
} else {
    Write-Output "Invoking service $url -infile $inputfile -outfile $actual..."
    Request-CodeStat $url $inputfile | Out-File $actual
    Get-Content $actual | Out-File $expected
}

Write-Output "****** ****** ******"
Write-Output ""
