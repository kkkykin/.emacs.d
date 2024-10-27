$currentScript = $MyInvocation.MyCommand.Name
Get-ChildItem -Path $PSScriptRoot -Filter "*.ps1" | 
    Where-Object { $_.Name -ne $currentScript } | 
    ForEach-Object {
        Write-Host "Executing script: $($_.Name)" -ForegroundColor Green
        try {
            & $_.FullName
            Write-Host "Successfully executed: $($_.Name)" -ForegroundColor Green
        }
        catch {
            Write-Host "Error executing $($_.Name): $($_.Exception.Message)" -ForegroundColor Red
        }
    }
