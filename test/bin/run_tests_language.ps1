Param
(
    [string]$language,
    [string[]]$variants
)

if (Test-Path -Path ".\test\ref\$action\$language") {
} else {
    New-Item -ItemType Directory -Path ".\test\ref\$action\$language"
}

ForEach ($variant in $variants) {
    test\bin\run_tests_variant.ps1 -language $language -variant $variant
}
