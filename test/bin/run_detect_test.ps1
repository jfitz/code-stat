Param
(
    [string]$name,
    [string]$language,
    [string]$comment,
    [switch]$wide,
    [string]$inputfile,
    [string]$expected,
    [int32]$tabsize
)

Write-Output "****** ****** ******"
Write-Output "Starting test $name..."
Write-Output ""

$testbed = "$env:USERPROFILE\Projects\CodeStat"
$actual = "$testbed\$name\out.txt"

$params = @()

$params += "-i $inputfile"

if ($PSBoundParameters.ContainsKey('comment')) {
    $params += '--comment'
    $params += $comment
}

if ($wide) {
    $params += '--wide'
}

if ($PSBoundParameters.ContainsKey('tabsize')) {
    $params += '--tabsize'
    $params += $tabsize
}

if ($PSBoundParameters.ContainsKey('language')) {
    $params += '--languages'
    $l2 = $language.Split()
    $ls = $l2 -join ','
    $params += $ls
}

if (!(Test-Path -Path $testbed\$name)) {
    New-Item -ItemType directory -Path $testbed\$name
}
Remove-Item $testbed\$name\*.*

# call Python client
Write-Output "Invoking detect Python script $params -infile $inputfile -outfile $actual..."
python test\bin\run_detect_test.py $params | Out-File $actual
Write-Output "Adjusting JSON output..."
Get-Content $actual | python AddNlToJson.py | python IndentJson.py | Out-File $expected

Write-Output "****** ****** ******"
Write-Output ""
