Param
(
    [string]$name,
    [string]$language,
    [string]$action,
    [string]$inputfile,
    [string]$expected
)

Write-Output "Starting test $name..."
$actual = ".\tests\$name\out.txt"
$actual_adjusted = ".\tests\$name\out1.txt"
$url = "http://localhost:5000/${action}?language=$language"
mkdir -path .\tests\$name
Write-Output "Invoking service $url -infile $inputfile -outfile $actual..."
Invoke-RestMethod -method post -uri "$url" -infile $inputfile -outfile $actual

Write-Output "Adjusting JSON output..."
Get-Content $actual | python src/AddNlToJson.py | Out-File $actual_adjusted

Write-Output "Comparing $actual against $expected..."
if (Compare-Object $(Get-Content $expected) $(Get-Content $actual_adjusted)) {
    "Test $name failed"
    Copy-Item $actual_adjusted $expected
    set-variable -name failures -value ($failures + 1) -scope 1
} else {
    "Test $name passed"
}
Write-Output ""
