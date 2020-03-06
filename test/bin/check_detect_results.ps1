$names = @{
    'C-K&R'='C-78';
    'C++'='Cplusplus';
    'C#'='Csharp';
    'PL/1-Fixed'='PL1';
    'PL/1-Free'='PL1'
}

Get-ChildItem .\test\ref\detect |
ForEach-Object {
    $language = $_.BaseName
    Get-ChildItem $_ -Filter *.txt |
    ForEach-Object {
        $test = $_.BaseName
        $a = Get-Content $_ | Select-String ":"
        $c = $a -replace ":", " " | Sort-Object { $_.split()[-1] } -Descending | Select-Object -first 1
        if (-Not ([string]::IsNullOrEmpty($c))) {
            $d = $c.Trim()
            $e, $f = $d.Split(' ', 2)
            $detected = $e -replace '"',''
            $confidence = $f -replace ',',''

            # map detected name to standard name
            If ($names.ContainsKey($detected)) {
                Set-Variable -Name detected -Value $names[$detected]
            }

            If ($detected -ne $language) {
                Write-Output "$language $test $detected $confidence"
            }
        }
    }
}