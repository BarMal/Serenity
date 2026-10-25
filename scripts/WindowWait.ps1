# Polling for the Windows launcher check (scripts/verify-windows-launcher.ps1). A fixed sleep before looking for the
# application's window failed on a runner slow enough that the JVM had none yet; this waits for it up to a limit
# instead. Cases: scripts/WindowWait.Tests.ps1.

<#
  Calls $Probe until it reports Ready, says it is Final, or $TimeoutSeconds have passed. The probe returns an object with
  Ready, Final (nothing more can change, so stop waiting) and Description (what it saw). It is called before the first
  sleep, so a state that is ready already costs no delay. Returns Ready, Elapsed and the State of the last probe.
#>
function Wait-Until {
  param(
    [Parameter(Mandatory)] [scriptblock]$Probe,
    [int]$TimeoutSeconds = 30,
    [int]$IntervalMilliseconds = 250
  )
  $clock = [System.Diagnostics.Stopwatch]::StartNew()
  do {
    $state = & $Probe
    if ($state.Ready -or $state.Final) { break }
    Start-Sleep -Milliseconds $IntervalMilliseconds
  } while ($clock.Elapsed.TotalSeconds -lt $TimeoutSeconds)
  return [pscustomobject]@{ Ready = [bool]$state.Ready; Elapsed = $clock.Elapsed; State = $state }
}
