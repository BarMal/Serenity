# Cold-start timing breakdown

Measurement only — no source changes, no PR. Worktree: additive, from `origin/master`
(62f024e7, matches the commit named in the brief). JDK: OpenJDK 21.0.12.1 (nixos),
8 logical CPUs.

## Big caveat first: this machine was not quiet

`uptime` during the runs showed load average 5.7–9.8 on an 8-core box, with Firefox
(multiple content processes) and several long-running Claude Code sessions (one with
7+ hours, another with 1+ day of CPU time) active throughout. Absolute numbers below
are noticeably higher than the ~4.6 s the brief quoted from an undisturbed launch, and
show an upward drift within each variant's run sequence (contention increasing over the
~12 minutes the full sweep took), not a property of the launch method itself. The
**relative** comparison between variants — measured back-to-back, same machine, same
load regime — is the trustworthy part of this data; the absolute seconds are not
representative of a quiet machine. A quieter re-run would be needed for absolute numbers
to use in a regression budget.

One more fixed artifact specific to this harness: every run used `-Duser.home=<scratch>`
to avoid touching the owner's real `~/.serenity` (see Method), and that scratch path is
long enough that `InstanceMessenger`'s unix-domain socket bind fails
(`SocketException: Unix domain path too long`) on every run. That costs a small,
constant amount of exception-handling time on every run in every variant alike, so it
doesn't bias the comparison between variants, but it does mean none of these runs are
quite what a real `~/.serenity`-length path would do.

## Results (5 cold runs each, run 1 dropped as warm-up per the brief; see `results.csv` for all 18 raw rows)

| Variant | run 1 (warm-up) launch→paint | runs 2–6 avg launch→paint | min | max |
|---|---|---|---|---|
| `sbt run --smoke-test` | 40.7 s | **43.8 s** | 29.4 s | 59.3 s |
| `java -jar` (plain, no shipped flags) | 8.2 s | **12.0 s** | 9.8 s | 14.9 s |
| `java -jar` + shipped AppCDS/heap flags | 15.2 s (builds the archive) | **8.4 s** | 7.7 s | 9.5 s |

"Packaged image" variant: **not available to test locally.** There is no jlink/jpackage
sbt task in this build (only `sbt-assembly`; `project/plugins.sbt` has no packager
plugin), and no app-image artifact already built in this worktree or checked into the
repo — packaging happens in CI (`desktop-publish.yml`/`desktop-release.yml` via
`jpackage`, per `DEVELOPMENT.md`), not via anything runnable here. Per the brief's "if
available," this is skipped rather than faked.

### Phase breakdown (runs 2–6 average, seconds)

| Variant | launch→first log line | first log→"Starting Serenity" | "Starting Serenity"→first paint |
|---|---|---|---|
| `sbt run` | 4.8 | **35.6** | 3.4 |
| plain `java -jar` | 4.2 | 3.7 | 4.1 |
| `java -jar` + AppCDS | 2.5 | 3.1 | 2.9 |

For `sbt run`, "first log line" is sbt's own first `[info]` output, not the app's — the
35.6 s middle column is sbt's task-graph/classpath work *and* the forked JVM's own
startup bundled together (sbt's `Compile / run / fork := true` forks a fresh JVM per
run, and that fork's startup isn't separately observable from outside sbt's output).
That number is the direct answer to "how much is `sbt run` overhead": **roughly 32 s of
the ~44 s `sbt run` total is sbt itself, not the editor** — the remaining ~8–9 s after
sbt hands off matches the plain/AppCDS `java -jar` numbers almost exactly (compare the
other two columns, which line up closely between all three variants once sbt's own
window is set aside).

For the two `java -jar` variants, **AppCDS's win is concentrated before the app's first
log line even prints** (4.2s → 2.5s average), i.e. in core JVM/Scala/Cats-Effect
bootstrap and class loading — not in `Main`'s own startup logic (the "first log→Starting
Serenity" column is within noise of each other, 3.7s vs 3.1s). This matches
`DEVELOPMENT.md`'s own claim ("most of a cold start is spent loading and verifying
classes") and this harness's measurements back it up directly (see class-loading pass
below).

### Why the gap before "Starting Serenity" is still multiple seconds even outside sbt

One extra plain `java -jar` pass with `-Xlog:class+load:file=classload-plain-run.log.gz:uptime,tags`
(gzipped in this directory; `-Xshare:auto`, i.e. only the JDK's own bundled base CDS
archive, no app-specific archive):

- **13,516** class-load events total.
- Only **1,261** (9.3%) came from `source: shared` (the JDK's built-in base archive —
  core `java.lang`/`java.util`, nothing Scala/Cats-Effect/app-specific).
- **5,445** loaded individually from `Serenity.jar` (Scala stdlib, Cats Effect, FS2, and
  Serenity's own ~900 classes).
- **2,059** loaded from JDK modules (`jrt:`) not covered by the base archive.
- **718** are `__JVM_LookupDefineClass__` — anonymous/lambda classes generated at
  runtime by `invokedynamic` (Scala/Cats' heavy use of lambdas generates one of these
  per distinct lambda call site the first time it's hit; this project's functional-core
  style multiplies this cost at cold start specifically).

So: ~92% of class-loading work at cold start is verifying and loading classes the JDK's
own default archive doesn't cover. This is exactly the gap the app-specific
`-XX:SharedArchiveFile` (the shipped AppCDS flags) closes — and the measured ~1.7s
average reduction in "launch→first log line" above is consistent with that archive
absorbing most of those 5,445 + 2,059 + 718 ≈ 8,222 non-shared loads.

## Method

- Worktree `serenity-laptop-startup-timing`, built with `sbt assembly` under
  `systemd-run --user --scope`.
- Every run used a **fresh, empty** `-Duser.home=<scratch dir>` (new directory per run,
  `rm -rf` between runs) so `ConfigManager`/`SessionManager`'s `~/.serenity`-rooted paths
  never touch the owner's real config/session, and so no run carries over session state
  from the previous one. Confirmed again at the end: `~/.serenity/config.conf`
  md5sum unchanged (`ff047b31b43560df136682b9ab1e27cf`, matches the pre-run backup from
  the earlier summon-chord diagnostic session and this session's own check).
- `--smoke-test` (the flag the CI app-image check itself uses) stands in for "closing it
  once first paint has happened": `Main` paints the first frame, prints
  `SERENITY_SMOKE_READY`, and quits itself — no manual watch-log-then-kill needed, and it
  exercises the exact code path CI already trusts for "started and painted" vs "started
  and hung."
- Every log line was re-timestamped externally at arrival (`time.time()` in a small
  Python filter piped after the process) rather than relying solely on the app's own
  embedded `HH:MM:SS.mmm` log timestamps, so `launch→first log line` (time before the
  app's logger even runs) is measurable and all deltas share one clock. One early sanity
  run showed `SERENITY_SMOKE_READY` printing ~3.4 s *before* the later
  "Initial render completed, starting main loop" log line; a second sanity run showed
  them within 1 ms of each other. `SERENITY_SMOKE_READY` (the documented, CI-trusted
  first-frame signal — see `SmokeTest.scala`) was treated as the authoritative first-paint
  marker throughout; the "Initial render completed" line is logged but not used as a
  phase boundary, since its timing relative to first paint wasn't consistent across the
  two sanity checks and wasn't investigated further (out of scope for a timing-only
  diagnostic).
- AppCDS variant: `-XX:+UseG1GC -XX:MaxRAMPercentage=25 -XX:G1PeriodicGCInterval=60000
  -XX:SharedArchiveFile=<fixed path>/serenity.jsa -XX:+AutoCreateSharedArchive`, exactly
  as documented in `DEVELOPMENT.md`'s "Faster startup with a class-data archive" section.
  The archive path was **fixed across that variant's 6 runs** (unlike `-Duser.home`,
  which was fresh every run) so the archive built on run 1 persists for runs 2–6, matching
  real usage (the archive is a jar/JVM-level artifact, not per-user-config state).
- `sbt run` variant: `-Duser.home` passed to the *forked* app JVM via
  `sbt 'set Compile / run / javaOptions += "-Duser.home=<scratch>"' 'run --smoke-test'`,
  since `Compile / run / fork := true` means the forked JVM does not inherit `-D`
  properties from the sbt launcher's own JVM. Each of the 6 runs started a brand-new
  `sbt` process (not a persistent interactive shell), matching "a developer runs
  `sbt run` cold" rather than reusing a warm sbt session.
