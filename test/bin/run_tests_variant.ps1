Param
(
    [string]$language,
    [string]$variant
)

if (Test-Path -Path ".\test\ref\$action\$language\$variant") {
} else {
    New-Item -ItemType Directory -Path ".\test\ref\$action\$language\$variant"
}

Get-ChildItem ".\test\data\$language" | ForEach-Object {
    $filename = $_.BaseName + $_.Extension
    test\bin\run_test_file.ps1 -json -name $filename-$language-$variant -action $action -language $variant -inputfile ".\test\data\$language\$filename" -expected ".\test\ref\$action\$language\$variant\$filename.txt"
}
