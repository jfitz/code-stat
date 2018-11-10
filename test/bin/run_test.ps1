Param
(
    [string]$name,
    [string]$language="BASIC",
    [string]$action="confidence"
)

Write-Output "Starting test $name..."
$input_file = ".\test\data\simple.txt"
$expected_result_file = ".\test\ref\$action\BASIC.txt"
$actual_result_file = ".\tests\BASIC\out.txt"
$url = "http://localhost:5000/${action}?language=$language"
Write-Output "action: $action"
Write-Output "URL: $url"
invoke-restmethod -method post -uri "$url" -infile $input_file -outfile $actual_result_file
if (Compare-Object $(Get-Content $actual_result_file) $(Get-Content $expected_result_file)) {
    "Test $name failed"
} else {
    "Test $name passed"
}
