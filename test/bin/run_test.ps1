Param
(
    [string]$name,
    [string]$language="BASIC",
    [string]$action="confidence",
    [string]$inputfile,
    [string]$expected
)

Write-Output "Starting test $name..."
$actual = ".\tests\$name\out.txt"
$url = "http://localhost:5000/${action}?language=$language"
mkdir -path .\tests\$name
Write-Output "Invoking service $url"
invoke-restmethod -method post -uri "$url" -infile $input_file -outfile $actual
Write-Output "Comparing $actual against $expected"
if (Compare-Object $(Get-Content $expected) $(Get-Content $actual)) {
    "Test $name failed"
} else {
    "Test $name passed"
}
Write-Output ""
