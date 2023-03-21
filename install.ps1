# install.ps1 -- Install the git config files

# Mike Barker <mike@thebarkers.com>
# July 23rd, 2018

# Test-SymbolicLink
Function Test-SymbolicLink([string]$path) {
    $file = Get-ChildItem $path -Force -ea SilentlyContinue
    Return [bool]($file.LinkType -eq "SymbolicLink")
}

# New-SymbolicLink
Function New-SymbolicLink([string]$link, [string]$target) {
    New-Item -ItemType SymbolicLink -Path $link -Value $target -Force
}

# Test-Elevated - Test if the current powershell session is being run with elevated permisions
Function Test-Elevated() {
    return [Security.Principal.WindowsIdentity]::GetCurrent().Groups -contains 'S-1-5-32-544'
}

# Get all the files and directories in the home folder
$files = $(Get-ChildItem $PSScriptRoot\home\.*)

# Foreach file and or folder
ForEach ($file in $files) {
    $link = "$($env:userprofile)\$($file.Name)"
    $target = $file
    # If the file/folder exists and is not a link
    if ((Test-Path $link) -And (-Not (Test-SymbolicLink $link))) {
        # backup the file/folder
        Write-Warning "Backup $link $link.backup"
        Move-Item -Path $link -Destination "$($link).backup"
    }
    # Create a link to file/folder
    Write-Output "Linking: $($link) to $($target)"
    New-SymbolicLink $link $target | Out-Null
}
