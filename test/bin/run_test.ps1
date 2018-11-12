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
$url = "http://localhost:5000/${action}?language=$language"
mkdir -path .\tests\$name
Write-Output "Invoking service $url -infile $inputfile -outfile $actual"
invoke-restmethod -method post -uri "$url" -infile $inputfile -outfile $actual

Write-Output "Comparing $actual against $expected"
if (Compare-Object $(Get-Content $expected) $(Get-Content $actual)) {
    "Test $name failed"
    Copy-Item $actual $expected
    set-variable -name failures -value ($failures + 1) -scope 1
} else {
    "Test $name passed"
}
Write-Output ""
