# Nightly bench pull

Real GPU timings on the two benchmark machines, without a self-hosted runner (#2575, P4). GitHub builds the bench bundle every night; a timer on each machine pulls it, runs the machine's benchmarks and compares with the previous night on the same machine. Nothing connects in to the machines, and no GitHub workflow ever runs on them.

```
02:23 UTC  bench-artifacts.yml (schedule, master)  ->  artifact bench-<sha7>, kept 14 days
local time timer -> bench/pull_nightly.py -> download -> run -> compare -> results dir (+ optional issue comment)
```

The puller is `bench/pull_nightly.py` (stdlib Python 3, Linux and Windows). Run it from a checkout you own, not from a downloaded bundle, and update that checkout with `git pull`. The example configs are `bench/nightly-pull.laptop.example.json` and `bench/nightly-pull.windows.example.json`; each machine copies one and edits it. `--dry-run` shows which commit it would benchmark; `--force` re-runs a commit already done; `--fail-on-regression` exits 1 when a metric got slower.

What it does each run: finds the newest successful *scheduled* `bench-artifacts.yml` run on master; stops if that commit is in `<results_dir>/state.json`; downloads the artifact into a fresh directory under `<results_dir>/bundles`, refusing any zip member that would leave it; runs the config's commands with `<results_dir>/runs/<sha7>` as the working directory; reduces the results to one p50 per metric (`metrics.json`); prints and saves a summary (`summary.txt`); deletes the bundle. A failed command records nothing, so the next run retries that commit.

A metric counts as slower (or faster) when its p50 moved by at least `regress_ratio` (default 1.20) and `min_delta_ms` (default 0.05) against the previous benchmarked commit on this machine. One run a night is too few for `scripts/abba_compare.py`'s range test, so this is deliberately a simple self-relative rule. It is a trend signal, not a gate.

## 1. Create the tokens

Make a fine-grained personal access token. Do this once per machine, so one can be revoked alone.

1. GitHub, Settings, Developer settings, Personal access tokens, Fine-grained tokens, Generate new token.
2. Name it `serenity-bench-pull-<machine>`. Resource owner: `BarMal`.
3. Expiration: 90 days. Put the renewal date in a calendar; an expired token shows up as `HTTP 401` in the log.
4. Repository access: Only select repositories, then `BarMal/Serenity` and nothing else.
5. Permissions, Repository: **Actions: Read-only**. Metadata: Read-only is added automatically. Grant nothing else.
6. Generate, and copy the token once. Store it as in step 2 of your machine's section. Never put it in a config file, the repository or a shell history.

Optional, to post each summary as a comment on a tracking issue: a second token, same steps, name `serenity-bench-post-<machine>`, **Issues: Read and write** and nothing else. Set `"post": {"issue": 2575}` in the config and expose the token as `SERENITY_BENCH_ISSUES_TOKEN`. Without that variable nothing is posted. The download token stays read-only either way.

## 2. NixOS laptop (systemd user service and timer)

Assumes the repository is checked out at `~/src/Serenity`.

1. Config and results:
   ```bash
   mkdir -p ~/.config/serenity-bench ~/serenity-bench/laptop
   cp ~/src/Serenity/bench/nightly-pull.laptop.example.json ~/.config/serenity-bench/laptop.json
   ```
2. Token in a file only you can read:
   ```bash
   install -m 0600 /dev/null ~/.config/serenity-bench/token.env
   printf 'SERENITY_BENCH_TOKEN=%s\n' 'PASTE_TOKEN_HERE' > ~/.config/serenity-bench/token.env
   ```
   Add `SERENITY_BENCH_ISSUES_TOKEN=...` on a second line if you use posting. Alternative: keep the token in a credential file and use `LoadCredential=token:%h/.config/serenity-bench/token` in the unit, then read `$CREDENTIALS_DIRECTORY/token` in a small wrapper that exports `SERENITY_BENCH_TOKEN`. Caution, here be imagine dragons: I have not checked that your NixOS release's systemd supports `LoadCredential=` in user units.
3. Let user units see the graphical session. The benchmark opens real windows and `hyprland-float.sh` calls `hyprctl`, so the unit needs `WAYLAND_DISPLAY`, `DISPLAY` (XWayland) and `HYPRLAND_INSTANCE_SIGNATURE`. Add to the Hyprland config, then restart the session:
   ```
   exec-once = systemctl --user import-environment WAYLAND_DISPLAY DISPLAY HYPRLAND_INSTANCE_SIGNATURE XDG_RUNTIME_DIR
   exec-once = dbus-update-activation-environment --systemd WAYLAND_DISPLAY DISPLAY HYPRLAND_INSTANCE_SIGNATURE
   ```
4. `~/.config/systemd/user/serenity-bench.service`:
   ```ini
   [Unit]
   Description=Serenity nightly benchmark pull
   ConditionEnvironment=DISPLAY

   [Service]
   Type=oneshot
   EnvironmentFile=%h/.config/serenity-bench/token.env
   Environment=PATH=/run/wrappers/bin:%h/.nix-profile/bin:/etc/profiles/per-user/%u/bin:/run/current-system/sw/bin
   ExecStart=python3 %h/src/Serenity/bench/pull_nightly.py --config %h/.config/serenity-bench/laptop.json
   TimeoutStartSec=3h
   ```
   `~/.config/systemd/user/serenity-bench.timer`:
   ```ini
   [Unit]
   Description=Serenity nightly benchmark pull

   [Timer]
   OnCalendar=*-*-* 09:30:00
   Persistent=true
   RandomizedDelaySec=10m

   [Install]
   WantedBy=timers.target
   ```
   Pick a time you are away: `bench-laptop.sh` injects keystrokes and mouse clicks into the focused window for about 15 minutes, so leave the laptop alone, awake, on AC and logged in (see `bench/README.md`).
5. Dependencies. The config's `app-benchmark` command wraps `bench-laptop.sh` in `nix-shell -p xdotool wmctrl python3 xorg.xwininfo`, as in `bench/README.md`. `python3` (for the puller) and a Java 25 `java` must be on the unit's `PATH`; the `PATH` above is a guess. Caution, here be imagine dragons: I have not run this on NixOS, so check `systemctl --user show-environment` and adjust the `PATH` line, for example with a `jdk` in your user profile.
6. Try it before trusting the timer:
   ```bash
   SERENITY_BENCH_TOKEN=... python3 ~/src/Serenity/bench/pull_nightly.py --config ~/.config/serenity-bench/laptop.json --dry-run
   systemctl --user daemon-reload
   systemctl --user enable --now serenity-bench.timer
   systemctl --user start serenity-bench.service     # one real run now
   ```
7. Logs and results:
   ```bash
   journalctl --user -u serenity-bench.service -e
   systemctl --user list-timers serenity-bench.timer
   cat ~/serenity-bench/laptop/runs/*/summary.txt
   ```
8. Disable: `systemctl --user disable --now serenity-bench.timer`. Remove the unit files to uninstall.

## 3. Windows 11 desktop (Task Scheduler)

Assumes the repository is checked out at `%USERPROFILE%\src\Serenity`, and Temurin 25 (`java`) and Python 3 (`python`) are on `PATH`. Use PowerShell.

1. Config and results:
   ```powershell
   $cfg = Join-Path $env:APPDATA 'serenity-bench'
   New-Item -ItemType Directory -Force $cfg, "$env:USERPROFILE\serenity-bench\windows" | Out-Null
   Copy-Item "$env:USERPROFILE\src\Serenity\bench\nightly-pull.windows.example.json" "$cfg\windows.json"
   ```
2. Token, encrypted with Windows DPAPI so only your user on this PC can decrypt it:
   ```powershell
   Read-Host 'Token' -AsSecureString | ConvertFrom-SecureString | Set-Content "$cfg\token.dpapi"
   icacls $cfg /inheritance:r /grant:r "${env:USERNAME}:(OI)(CI)F"
   ```
   This was chosen over Credential Manager because reading that from PowerShell needs an extra module. For the optional posting token, make `token-post.dpapi` the same way and set `SERENITY_BENCH_ISSUES_TOKEN` in the script below.
3. `%APPDATA%\serenity-bench\run-nightly.ps1`:
   ```powershell
   $ErrorActionPreference = 'Stop'
   $cfg = Join-Path $env:APPDATA 'serenity-bench'
   $secure = Get-Content "$cfg\token.dpapi" | ConvertTo-SecureString
   $env:SERENITY_BENCH_TOKEN = [System.Net.NetworkCredential]::new('', $secure).Password
   python "$env:USERPROFILE\src\Serenity\bench\pull_nightly.py" --config "$cfg\windows.json" *>> "$cfg\pull.log"
   ```
4. Register the task. `-LogonType Interactive` means it runs only while you are logged on, so the desktop and the RTX 3070 are available:
   ```powershell
   $action    = New-ScheduledTaskAction -Execute 'powershell.exe' -Argument "-NoProfile -ExecutionPolicy Bypass -File `"$env:APPDATA\serenity-bench\run-nightly.ps1`""
   $trigger   = New-ScheduledTaskTrigger -Daily -At 9:30am
   $principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive -RunLevel Limited
   $settings  = New-ScheduledTaskSettingsSet -StartWhenAvailable -AllowStartIfOnBatteries -MultipleInstances IgnoreNew -ExecutionTimeLimit (New-TimeSpan -Hours 3)
   Register-ScheduledTask -TaskName 'Serenity nightly bench' -Action $action -Trigger $trigger -Principal $principal -Settings $settings
   ```
   Caution, here be imagine dragons: I have not run these on Windows 11. Keep the session unlocked and the display on during the run; a locked screen may stop the benchmark windows from being presented.
5. Run once now and check: `Start-ScheduledTask 'Serenity nightly bench'`, then `Get-Content "$env:APPDATA\serenity-bench\pull.log" -Tail 40`. Results are under `%USERPROFILE%\serenity-bench\windows\runs\<sha7>\`, including `summary.txt`.
6. Disable: `Disable-ScheduledTask 'Serenity nightly bench'`. Remove: `Unregister-ScheduledTask 'Serenity nightly bench' -Confirm:$false`.

## 4. Security notes

- Only master's nightly artifact runs on your machines: the puller asks for runs with `event=schedule` on `branch=master` and nothing else, so a pull request or a manually dispatched build of another ref is never picked. The code is your own build of master, but it does run with your user's rights, so treat a compromised master as a compromised laptop.
- No inbound connections, no open port, no runner agent, no workflow targeting the machines. The only traffic out is HTTPS to `api.github.com` and GitHub's artifact storage.
- The download token is limited to `BarMal/Serenity` with Actions: read. It can read workflow runs and artifacts, and cannot push, comment, or change settings. The token is not passed to the benchmark processes, and it is not sent on to the storage host when the download is redirected.
- The zip is checked before anything is written: absolute paths, `..`, backslashes, drive letters, symbolic links and an oversized unpack all abort the run. It is extracted into a new empty directory, and the benchmarks run with the results directory as their working directory.
- Revoke: GitHub, Settings, Developer settings, Fine-grained tokens, open the token, Delete. It stops working at once; then delete the local token file (`token.env`, `token.dpapi`). If a machine is lost, revoke its token first.
- Results hold your hardware and OS details. They stay local unless you configure `post`, and the tracking issue is public.
