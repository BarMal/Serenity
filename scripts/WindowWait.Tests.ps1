# Cases for Wait-Until (scripts/WindowWait.ps1), the poll that replaced the fixed 5 s sleep in front of the Windows
# launcher check (a slow runner had no main window yet when CloseMainWindow() ran). Plain PowerShell, no Pester: the
# step that runs it fails when this script throws.
#   pwsh scripts/WindowWait.Tests.ps1
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'WindowWait.ps1')

$script:failures = 0

function Test-Case {
  param([string]$Name, [scriptblock]$Body)
  try {
    & $Body
    Write-Host "ok   $Name"
  }
  catch {
    Write-Host "FAIL $Name : $($_.Exception.Message)"
    $script:failures++
  }
}

function Assert-True {
  param($Condition, [string]$Message)
  if (-not $Condition) { throw $Message }
}

Test-Case 'waits for a state that turns ready after several probes' {
  $calls = @{ Probes = 0 }
  $probe = {
    $calls.Probes++
    [pscustomobject]@{ Ready = ($calls.Probes -ge 4); Final = $false; Description = "call $($calls.Probes)" }
  }

  $result = Wait-Until -Probe $probe -TimeoutSeconds 10 -IntervalMilliseconds 10

  Assert-True $result.Ready 'expected the wait to end ready'
  Assert-True ($calls.Probes -eq 4) "expected 4 probes, saw $($calls.Probes)"
  Assert-True ($result.State.Description -eq 'call 4') "expected the state of the ready probe, saw '$($result.State.Description)'"
}

Test-Case 'gives up at the timeout and reports the last state it saw' {
  $probe = { [pscustomobject]@{ Ready = $false; Final = $false; Description = 'still waiting' } }

  $result = Wait-Until -Probe $probe -TimeoutSeconds 1 -IntervalMilliseconds 50

  Assert-True (-not $result.Ready) 'expected the wait to end not ready'
  Assert-True ($result.Elapsed.TotalSeconds -ge 1) "gave up after only $($result.Elapsed.TotalSeconds) s"
  Assert-True ($result.Elapsed.TotalSeconds -lt 8) "ran on to $($result.Elapsed.TotalSeconds) s"
  Assert-True ($result.State.Description -eq 'still waiting') "expected the last state, saw '$($result.State.Description)'"
}

Test-Case 'stops at once when the probe says nothing more can change' {
  $calls = @{ Probes = 0 }
  $probe = {
    $calls.Probes++
    [pscustomobject]@{ Ready = $false; Final = $true; Description = 'the launcher exited' }
  }

  $result = Wait-Until -Probe $probe -TimeoutSeconds 30 -IntervalMilliseconds 50

  Assert-True (-not $result.Ready) 'expected the wait to end not ready'
  Assert-True ($calls.Probes -eq 1) "expected 1 probe, saw $($calls.Probes)"
  Assert-True ($result.Elapsed.TotalSeconds -lt 5) "waited $($result.Elapsed.TotalSeconds) s for a final state"
}

Test-Case 'probes before it sleeps, so a state that is ready already costs no delay' {
  $probe = { [pscustomobject]@{ Ready = $true; Final = $false; Description = 'ready' } }

  $result = Wait-Until -Probe $probe -TimeoutSeconds 30 -IntervalMilliseconds 5000

  Assert-True $result.Ready 'expected the wait to end ready'
  Assert-True ($result.Elapsed.TotalSeconds -lt 2) "waited $($result.Elapsed.TotalSeconds) s although the first probe was ready"
}

if ($script:failures -gt 0) {
  throw "$($script:failures) window-wait case(s) failed."
}
Write-Host 'All window-wait cases passed.'
