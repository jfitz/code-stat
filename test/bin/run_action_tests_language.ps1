Param
(
    [string]$language,
    [string[]]$variants,
    [string]$format
)

if (Test-Path -Path ".\test\ref\$action\$language") {
} else {
    New-Item -ItemType Directory -Path ".\test\ref\$action\$language"
}

ForEach ($variant in $variants) {
    test\bin\run_action_tests_variant.ps1 -language $language -variant $variant -format $format
}
