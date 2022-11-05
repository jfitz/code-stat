Param
(
    [string]$language
)

Get-ChildItem ".\test\data\$language" | ForEach-Object {
    $filename = $_.BaseName + $_.Extension
    test\bin\run_detect_test_file.ps1 -json -name $filename-$language -action $action -inputfile ".\test\data\$language\$filename" -expected ".\test\ref\$action\$filename.txt"
}
