$names = @{
    'C-K&R'='C-78';
    'C++'='Cplusplus';
    'C#'='Csharp';
    'F#'='Fsharp';
    'PL/1'='PL1';
    'PL/M'='PLM'
}

Get-ChildItem .\test\ref\detect |
ForEach-Object {
    $test = $_.BaseName
    $a = Get-Content $_ | Select-String ":"
    $c = $a -replace ':', '' -replace '"', '' -replace ',', '' | Sort-Object { [double]$_.split()[-1] } -Descending | Select-Object -first 1

    if (-Not ([string]::IsNullOrEmpty($c))) {
        $d = $c.Trim()
        $detected, $confidence = $d.Split(' ', 2)

        # map detected name to standard name
        If ($names.ContainsKey($detected)) {
            Set-Variable -Name detected -Value $names[$detected]
        }

        Write-Output "$test : $detected $confidence"
    }
}