# Memory footprint of the running app

Serenity's packaged app is capped at a quarter of the machine's RAM (`-XX:MaxRAMPercentage=25`), which is 4 GB on a
16 GB laptop. A cap is not a footprint. This study measures what the app actually holds.

**Answer in one paragraph.** With a 15k-line text file and a markdown file open, the windowed app holds about
**520 MiB** of RSS (PSS about the same) when idle under the shipped flags, and about **730 MiB** at its peak just after
opening files, on a 16 GB / 4-CPU machine. It does not use 4 GB. Most of it is Java heap that the JVM keeps committed
long after it is garbage: the live data is about 100 MiB. Three JVM flags that change when G1 returns heap to the OS
take idle RSS to about **320 MiB** (-44 %) with no change to the application and no cap on the largest file it can
open. Details and the risks follow.

## Method

The probe is `scripts/measure-footprint.sh` (header comment documents every option); parsing is
`scripts/footprint_parse.py`, tested by `scripts/test_footprint_parse.py` on captured fixtures.

- **Build**: `sbt assembly` at `1b22ee31b`, run as `java <flags> -jar Serenity.jar --gui` under `xvfb-run`
  (1280x800). The jar, not the jlinked app image, so the JDK is the container's OpenJDK 21.0.11 (Ubuntu). The packaged
  app's own runtime may differ slightly.
- **Flags**: the shipped ones from `.github/workflows/desktop-release.yml` (`-XX:+UseG1GC -XX:MaxRAMPercentage=25
  -XX:G1PeriodicGCInterval=60000`, AppCDS with `-XX:+AutoCreateSharedArchive`), plus `-XX:NativeMemoryTracking=summary`
  for the breakdown. NMT is for measurement only; its own category is about 2 MiB, and a run without it (`c00-nonmt`)
  is inside the noise of the others.
- **Isolation**: a private `user.home`, so the user's config and session are untouched. Not safe mode: spell check and
  the rest run as they do for a person.
- **Workload**: wait for the single-instance socket and for CPU use to stay under 3 % of a core for three 2 s windows
  (the startup warm-up types through an off-screen editor and must not count as idle) -> checkpoint `start`. Open a
  generated 15,000-line plain-text file (about 1 MB) by launching a second `Serenity.jar --open FILE`, which the
  running instance receives, as a file manager would -> `open_text`. Open a 40-section markdown file the same way ->
  `open_markdown`. Then sit idle -> `idle`.
- **Per checkpoint**: RSS and PSS from `/proc/PID/smaps_rollup`; heap used/committed from `jcmd GC.heap_info`;
  non-heap from `jcmd VM.native_memory summary`.
- **Machine**: 16,093 MiB RAM, 4 CPUs, no swap. `MaxRAMPercentage` scales with RAM, so heap ceilings below are for this
  machine (G1 reserved 3,420 MiB of heap address space under the shipped flags, per NMT). Other agents' builds shared
  the machine, so absolute numbers are noisy.
- **Repetitions**: three per configuration, interleaved; tables show the median and, for idle RSS, the range.
  G1's heap sizing at startup is not deterministic: the baseline's idle RSS ranged 523-633 MiB across three runs, so
  differences under about 50 MiB between rows are not evidence.
- **Two idle lengths.** Round 1 samples `idle` 65 s after the last open, which is what the issue asked for. The JVM's
  periodic collection did not fire in that window (see below), so rounds 2 and later sample after 150 s (100 s for the
  30 s interval). Both are reported.
- **Correctness**: every run finished with `status=ok` (process alive at every checkpoint, no `OutOfMemoryError`
  in the output, no JVM crash log). Screenshots from a sample of runs show the editor with the markdown file drawn.
  The large file's window is stacked under the markdown one and the probe does not assert it loaded; heap and RSS
  rise when it opens, but treat "opened" as likely, not proven.

## Results

All sizes are MiB. RSS and PSS differ by under 5 MiB almost everywhere (little memory is shared with other
processes), so PSS is shown only for the idle checkpoint.

### Round 1: the nine requested configurations (idle = 65 s after last open)

Medians of three. "Heap used / committed" is at idle.

| # | Config (change from baseline) | RSS start | RSS open_text | RSS open_md | RSS idle (range) | PSS idle | Heap used / committed | CPU s | Ran |
|---|---|---|---|---|---|---|---|---|---|
| 1 | shipped flags (baseline) | 480 | 508 | 524 | 524 (523-633) | 519 | 153 / 344 | 24.3 | ok |
| 2 | + `MaxRAMPercentage=10` | 500 | 522 | 526 | 526 (501-587) | 522 | 167 / 270 | 23.5 | ok |
| 3 | `-Xmx512m` | 496 | 516 | 525 | 536 (525-551) | 531 | 175 / 272 | 23.1 | ok |
| 4 | `-Xmx256m` | 451 | 483 | 497 | 497 (474-506) | 482 | 147 / 221 | 24.0 | ok |
| 5 | SerialGC instead of G1 (same 25 % cap) | 382 | 396 | 402 | 401 (396-413) | 396 | 111 / 207 | 21.3 | ok |
| 6a | + `G1PeriodicGCInterval=10000` | 658 | 675 | 579 | 526 (507-540) | 511 | 85 / 288 | 23.6 | ok |
| 6b | + `SoftMaxHeapSize=512m` | 652 | 789 | 812 | 812 (649-829) | 797 | 271 / 540 | 23.8 | ok |
| 7 | + `-Xss16m` | 560 | 647 | 661 | 660 (512-820) | 643 | 125 / 390 | 24.2 | ok |
| 8 | + `TieredStopAtLevel=1` | 525 | 529 | 532 | 532 (514-665) | 528 | 252 / 342 | 12.7 | ok |
| 9 | + `ReservedCodeCacheSize=64m` | 538 | 564 | 570 | 569 (554-571) | 553 | 132 / 320 | 25.6 | ok |
| - | baseline without NMT | 558 | 576 | 582 | 582 (553-835) | 567 | 111 / 354 | 24.4 | ok |

CPU s is the process's cumulative CPU time at the idle checkpoint, a proxy for startup, warm-up and JIT cost.

Reading the rows:

- **Rows 2, 3, 4, 7, 9 and 6b are indistinguishable from baseline** within the run-to-run range. The cap does not
  change what the app uses; it changes what it may use. `-Xmx256m` kept the 15k-line workload comfortably (heap used
  147 MiB) and also survived a 500,000-line (about 30 MB) file below.
- **Row 5 (SerialGC) is the one big, consistent, memory-only win**: about -125 MiB RSS, ranges do not overlap with
  baseline. G1's own native structures are about 49 MiB in NMT against 1 MiB for Serial, and its heap commits more.
- **Row 6a** looks no better than baseline at 65 s idle only because heap had not yet been returned; see round 2.
- **Row 6b**: `SoftMaxHeapSize` is accepted without a warning and did nothing useful on G1 (committed heap was larger).
  Caution, here be imagine dragons: from the JDK's documentation as I remember it, G1 does not implement it in JDK 21
  (ZGC and Shenandoah do). I did not confirm that in source.
- **Row 7**: the packaged app sets no `-Xss`. `.jvmopts` sets `-Xss16m` for sbt's compiler only (`Compile / run /
  fork := true` forks without it). Thread stacks total 2.7 MiB committed across 35 threads, so 16 MiB stacks would
  cost nothing until a thread recursed deeply. Not applicable to the app.
- **Row 8 (C1 only)** halves CPU time to idle (12.7 against 24.3 s) and saves 15 MiB of code cache and 6 MiB of
  metaspace, but RSS is within noise because the heap dominates. See the tradeoff in the recommendations.
- **Row 9**: the code cache is only 34 MiB committed of 240 MiB reserved; reserving 64 MiB changes committed code by
  6 MiB. Reserved address space is not RSS.

### Round 2: why the periodic collection did not show up, and what does

A GC log of the shipped flags (`-Xlog:gc`, 190 s idle) shows the periodic collection firing at JVM uptime 120 s, not at
"60 s of idleness": the last ordinary GC was at 17.6 s, the first 60 s check at uptime 60 s found only 42 s since a GC
and skipped, and the next check was at 120 s. So **the shipped flag takes between 60 and 120 s after the last
collection to act**, and when it does the heap goes 494 -> 360 MiB committed, not lower, because G1 only shrinks when
free space exceeds `MaxHeapFreeRatio` (default 70 %) and 360 MiB committed with 106 MiB live is 70 % free.

`MaxHeapFreeRatio=30 MinHeapFreeRatio=10` makes G1 give back more. Medians of three, idle sampled after 150 s
(30 s interval: 100 s), RSS and PSS in MiB:

| Config | RSS start | RSS open_text | RSS open_md | RSS idle (range) | PSS idle | Heap used / committed at idle | CPU s | Ran |
|---|---|---|---|---|---|---|---|---|
| shipped flags (`L01`) | 641 | 725 | 732 | 578 (525-582) | 574 | 104 / 354 | 25.4 | ok |
| shipped + `MaxHeapFreeRatio=30 MinHeapFreeRatio=10` + `G1PeriodicGCInterval=10000` (`L15`) | 476 | 512 | 395 | **325 (311-335)** | 321 | 51 / 78 | 25.9 | ok |
| same with `G1PeriodicGCInterval=30000` (`L17`) | 340 | 417 | 436 | **317 (308-387)** | 298 | 53 / 80 | 26.5 | ok |
| `L15` + `-Xmx512m` (`L12`) | 487 | 510 | 393 | 337 (331-341) | 333 | 49 / 72 | 25.8 | ok |
| SerialGC, 25 % cap (`L05`) | 384 | 401 | 415 | 415 (407-425) | 411 | 125 / 207 | 23.2 | ok |
| SerialGC + `-Xmx512m` + free ratios + C1 only + 64 MiB code cache (`L14`) | 335 | 344 | 345 | 345 (345-347) | 341 | 147 / 207 | 11.5 | ok |
| shipped, AppCDS off (`L16`) | 574 | 595 | 600 | 578 (569-602) | 574 | 90 / 308 | 27.1 | ok |

Single-run `c11` (free ratios only, 65 s idle) reached 440 (364-629), and `c13` (Serial + 512 MiB + free ratios) 415.

- Heap shrinking is the dominant lever: **-250 MiB idle, and about -300 to -340 MiB just after the markdown file opens**,
  because the periodic collection also runs during the quiet gaps of the open sequence. (The baseline's `start` was
  480 MiB in round 1 and 641 in round 2: the same flags, different machine load, which is the noise to keep in mind.)
- It works without `-Xmx`. `L15` against `L12` differ by 12 MiB, within noise, so no heap cap is needed for the saving.
- SerialGC cannot shrink without a full collection, so its heap stays at its initial 207 MiB; it wins on peak but loses
  to tuned G1 at idle.
- AppCDS is memory-neutral: without it metaspace is 88 MiB and the CDS mapping 13 MiB, with it 30 and 84, the same total.
  It saves about 2 CPU-seconds of startup (25.4 against 27.1).

**Cost of a short interval.** A 10 s interval keeps G1 running a collection every 10 s for as long as the app is idle
(26 collections in 240 s, each 2-11 ms, plus a 0.1 s concurrent mark about every 30 s). Process CPU rose by 1.6 s over
240 s, about 0.7 % of one core, against 0.4 % for the shipped 60 s interval. At 30 s it fires every 30 s. Caution, here be
imagine dragons: I measured CPU time, not wakeups or battery; on a laptop in `--eco` mode the difference between 10 s
and 30 s may matter more than these numbers say.

### Terminal interface (`--tui`)

Two runs each, on a pty, idle sampled after 150 s.

| Config | RSS start | RSS open_text | RSS open_md | RSS idle | Heap used / committed at idle | CPU s |
|---|---|---|---|---|---|---|
| shipped flags | 379 | 467 | 340 | 340 (327-353) | 48 / 184 | 11.9 |
| SerialGC | 290 | 333 | 337 | 337 (335-338) | 48 / 207 | 11.3 |

The TUI starts at 380 MiB and idles at 340 MiB: no AWT, and about half the CPU time of the window (12 s against 24 s). It already gets the heap back at idle, because the 150 s window let the periodic collection run. The drop between
`open_text` and `open_markdown` is the heap shrinking, not the large file closing.

### Large file

One run each with a 500,000-line (about 30 MB) plain-text file, 20 s idle: RSS at `open_markdown` was 589 MiB
(shipped), 649 MiB (`-Xmx512m`), 540 MiB (`-Xmx256m`), all `status=ok`. A 30 MB file fits inside a 256 MiB heap. This says nothing about a 500 MB file, or about editing one: the probe only opens it.

## Where non-heap memory goes

NMT summary on the shipped flags at idle (round 1, median of three; NMT "committed", not all resident):

| Category | MiB | What it is |
|---|---|---|
| Java heap | 344 | G1 committed, 153 used |
| Shared class space | 84 | the AppCDS archive mapped into memory (the app's `.jsa` 72 MiB + the JDK's base archive 13 MiB, both counted resident in `smaps`) |
| GC | 49 | G1 remembered sets, card tables, mark bitmaps and marking stacks; 1 MiB under SerialGC |
| Code cache | 34 | JIT-compiled code (240 MiB reserved, not resident) |
| Class + Metaspace | 30 | ~29 MiB used by the app's classes |
| Symbol, Internal, Compiler | 6 | |
| Thread | 3 | 35 threads, stacks 2.7 MiB committed (27 threads under SerialGC) |
| Other | 1 | includes direct (NIO) buffers |
| Everything else | 2 | arena chunks, modules, NMT itself |
| **NMT total committed** | **553** | against 524 RSS; Caution, here be imagine dragons: I take the gap to be committed heap not yet touched |

- **Java2D/AWT natives are not visible as a cost.** RSS anonymous memory minus the NMT total is -8 MiB (baseline) to +29
  MiB (`-Xmx256m`), and the native libraries (`libjvm` 9 MiB, `libawt`, `libfreetype`, `libharfbuzz`, X11) together
  hold about 15 MiB of PSS in `top_mappings`. Caution, here be imagine dragons: NMT does not see memory that native
  libraries `malloc` themselves, so a small positive remainder could be Java2D, but the data cannot separate it from
  untouched-but-committed heap.
- **Direct buffers**: NMT "Other" is 1 MiB. Not a factor.
- **Threads**: not a factor at the default `-Xss`.
- **Metaspace and code cache** are about 64 MiB together: the next-largest controllable items after the heap and
  GC structures, and neither shrinks much with a flag (row 8 saves about 21 MiB combined).

So of 524 MiB at idle: roughly 344 + 49 is heap and its GC bookkeeping, 84 the CDS archive, 64 code and class metadata,
and about 12 everything else. The heap's *live* data is about 100 MiB.

## Recommendations, ranked by saving against risk

1. **Return heap sooner and further. Saving about 250 MiB idle (-44 %) and about 300 MiB at the post-open peak. Risk
   low.** Add `-XX:MaxHeapFreeRatio=30 -XX:MinHeapFreeRatio=10` and lower `G1PeriodicGCInterval` from 60000 to 30000.
   Measured on `L17`: idle RSS 317 MiB (range 308-387), heap committed 80 MiB. Nothing about what the app can open
   changes; the heap can still grow to the 25 % cap. Costs: a short pause (2-11 ms measured) every 30 s while idle,
   and regrowth work after an idle spell. The shipped comment in `desktop-release.yml` says freed heap returns
   "within a minute"; measured, it is one to two minutes and then only down to 70 % free, which these flags fix.
   Decide the interval on battery grounds (see the cost note); 30 s is my pick, 10 s saved no more.
2. **SerialGC. Saving about 125 MiB (-24 %) at every checkpoint. Risk moderate.** Longer collection pauses than G1 on a big heap, one thread doing all the work. The app's live set is about
   100 MiB, so pauses are short here; Caution, here be imagine dragons: I did not measure pause times on a multi-hundred
   MiB buffer, and the existing comment in the workflow explains G1 was named explicitly to avoid the JVM silently picking
   Serial on small machines. Tuned G1 (item 1) beats Serial at idle (317 against 415 MiB) while keeping G1's
   behaviour, so prefer item 1; take Serial only if peak RSS during use matters more than idle RSS.
3. **`-XX:TieredStopAtLevel=1`. Memory saving about 20 MiB; CPU saving about half (12 s against 24 s of CPU to idle).
   Risk moderate to high for the editing feel.** The saving that matters is battery and fan at startup, not RAM.
   The cost is lower peak speed on hot paths: C1 code is slower than C2 on long-running loops. Caution, here be imagine
   dragons: I did not measure typing latency, scroll frame times or search speed under C1; the repo's `bench`
   module and `scripts/check_perf_regression.py` are the place to do that before anyone ships it.

Not recommended, with the evidence: lowering `MaxRAMPercentage` or setting `-Xmx` (no effect on RSS at these sizes, and
it caps what a large file can use); `SoftMaxHeapSize` (no effect on G1); `-Xss` (the app does not set one);
`ReservedCodeCacheSize` (reserved address space, not RSS); turning AppCDS off (memory-neutral, costs about 2 CPU-seconds).

## What this study did not measure

- A packaged app image on a real desktop: the Wayland/X11 toolkit, a real compositor and HiDPI scaling are not in an
  Xvfb run, and Skia/Skiko paths were not exercised (the default Java2D renderer ran).
- macOS and Windows. The flags are the same on all three; allocation behaviour and the native libraries are not.
- Files over about 30 MB, edits, undo history, language servers and project folders. All were off or empty.
- Pause times, frame times and typing latency under any candidate.
- Machines with other RAM sizes: only the cap moves, but the G1 initial heap (1/64 of RAM) and thus the start-up
  committed heap do too. Caution, here be imagine dragons: a 32 GB machine will probably start with a larger heap
  than the 250 MiB seen here, and gain more from shrinking.

## Reproducing

```bash
sbt assembly
scripts/measure-footprint.sh --label baseline --csv footprint.csv
scripts/measure-footprint.sh --label shrink --idle-wait 150 --csv footprint.csv \
  --java-opt -XX:MaxHeapFreeRatio=30 --java-opt -XX:MinHeapFreeRatio=10 --java-opt -XX:G1PeriodicGCInterval=30000
scripts/measure-footprint.sh --mode tui --label tui --csv footprint.csv
python3 -m unittest scripts.test_footprint_parse -v
```

`--java-opt` options follow the shipped flags, so the last one wins. For a different collector, pass `--no-shipped` and
name every flag. Run each configuration at least three times: the baseline's idle RSS alone varied by 110 MiB.
