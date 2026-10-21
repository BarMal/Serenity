# Starts the packaged Windows app image and checks its process model: a Serenity.exe launcher with one Serenity.exe
# application child, which closes on an ordinary window-close request and takes the launcher with it. Run from the
# repository root, after jpackage has written dist/Serenity (desktop-release.yml and desktop-publish.yml).
#
# The check waits for the application's main window instead of sleeping a fixed time: a slow runner had no window after
# 5 s, CloseMainWindow() failed, and the step failed on a startup that was only slow.
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'WindowWait.ps1')

$WindowTimeoutSeconds = 30

$launcherPath = (Resolve-Path 'dist/Serenity/Serenity.exe').Path
$launcher = Start-Process -FilePath $launcherPath -PassThru
$observed = @()
$processIds = @()
$normalExit = $false

try {
  $probe = {
    $launcher.Refresh()
    if ($launcher.HasExited) {
      return [pscustomobject]@{ Ready = $false; Final = $true; Description = "the launcher exited with code $($launcher.ExitCode)" }
    }
    $processes = Get-CimInstance Win32_Process
    $applications = @($processes | Where-Object {
      $_.ParentProcessId -eq $launcher.Id -and $_.Name -ieq 'Serenity.exe' -and $_.ExecutablePath -ieq $launcherPath
    })
    if ($applications.Count -ne 1) {
      return [pscustomobject]@{ Ready = $false; Final = $false; Description = "the launcher is running with $($applications.Count) Serenity.exe children, expected 1" }
    }
    $application = Get-Process -Id $applications[0].ProcessId -ErrorAction SilentlyContinue
    if ($null -eq $application) {
      return [pscustomobject]@{ Ready = $false; Final = $false; Description = "the application process $($applications[0].ProcessId) is already gone" }
    }
    $application.Refresh()
    if ($application.MainWindowHandle -eq [IntPtr]::Zero) {
      return [pscustomobject]@{ Ready = $false; Final = $false; Description = "the application (pid $($application.Id)) is running but shows no main window yet" }
    }
    return [pscustomobject]@{
      Ready = $true
      Final = $false
      Description = 'ready'
      Processes = $processes
      Applications = $applications
      Application = $application
    }
  }

  $waited = Wait-Until -Probe $probe -TimeoutSeconds $WindowTimeoutSeconds -IntervalMilliseconds 250
  if (-not $waited.Ready) {
    $launcher.Refresh()
    $launcherState = if ($launcher.HasExited) { "has exited (code $($launcher.ExitCode))" } else { 'is still running' }
    throw "No Serenity main window after $([math]::Round($waited.Elapsed.TotalSeconds, 1)) s of a $WindowTimeoutSeconds s limit: $($waited.State.Description). The launcher process $launcherState."
  }
  Write-Host "The application showed its main window after $([math]::Round($waited.Elapsed.TotalSeconds, 1)) s."

  $state = $waited.State
  $launcherProcesses = @($state.Processes | Where-Object { $_.ProcessId -eq $launcher.Id })
  if ($launcherProcesses.Count -ne 1 -or $state.Applications.Count -ne 1) {
    throw "Expected a Serenity.exe launcher with one Serenity.exe child; observed $($launcherProcesses.Count) launchers and $($state.Applications.Count) application children."
  }
  $observed = @($launcherProcesses + $state.Applications)

  $observed | ForEach-Object {
    $runtime = Get-Process -Id $_.ProcessId -ErrorAction SilentlyContinue
    [pscustomobject]@{
      ProcessId = $_.ProcessId
      ParentProcessId = $_.ParentProcessId
      Name = $_.Name
      ExecutablePath = $_.ExecutablePath
      CommandLine = $_.CommandLine
      CpuSeconds = if ($null -eq $runtime) { $null } else { $runtime.CPU }
      WorkingSetMiB = [math]::Round([double]$_.WorkingSetSize / 1MB, 2)
    }
  } | Format-List
  $processIds = @($observed | ForEach-Object ProcessId | Sort-Object -Descending -Unique)
  $application = $state.Application
  if (-not $application.CloseMainWindow()) {
    throw "The Serenity application child did not accept a normal window-close request."
  }
  if (-not $application.WaitForExit(15000) -or -not $launcher.WaitForExit(15000)) {
    throw "Serenity launcher processes did not exit after a normal window-close request."
  }
  $remaining = Get-Process -Id $processIds -ErrorAction SilentlyContinue
  if ($null -ne $remaining) {
    throw "Serenity launcher processes remained after normal shutdown: $($remaining.Id -join ', ')."
  }
  $normalExit = $true
}
finally {
  if (-not $normalExit) {
    $processIds = @($launcher.Id) + $processIds
    $processIds | ForEach-Object {
      Stop-Process -Id $_ -Force -ErrorAction SilentlyContinue
    }
  }
}
