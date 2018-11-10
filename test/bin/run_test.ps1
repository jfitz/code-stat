Param
(
    [string]$name,
    [string]$language="BASIC"
)

echo "Starting test $name..."
$input_file = ".\test\data\simple.txt"
$expected_result_file = ".\test\ref\confidence\BASIC.txt"
$actual_result_file = ".\tests\BASIC\out.txt"
invoke-restmethod -method post -uri "http://localhost:5000/confidence?language=$language" -infile $input_file -outfile $actual_result_file
if (Compare $(Get-Content $actual_result_file) $(Get-Content $expected_result_file)) {
    "Test $name failed"
} else {
    "Test $name passed"
}
