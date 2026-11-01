# #2465 probe jobs (throwaway; probe branch only, never merged)

Four jobs for the laptop. Each script takes the **artifact folder** of a Bench Artifacts run of this branch (it holds
`Serenity.jar` and `serenity-perf.jar`) and an output folder, and prints one line per measurement. All of them use the
display the session already has (`$DISPLAY`); none uses xvfb. Use the same power state for every run (AC, screen on,
`powersave` governor as in the #2382 jobs) and note it.

| Job | Script | Claim it judges |
|---|---|---|
| J1 | `j1-warm-text-abba.sh` | Part A, candidate (a): how much of a cold frame is Skia's font and shaping first use (the ceiling of pre-shaping) |
| J2 | `j2-first-frames.sh` | Part A, candidate (b): what the first frames of a new Skia GPU window cost over later ones (context, pipelines, atlas) |
| J3 | `j3-aot.sh` | Part A, candidate (c): launch to first frame with an AOT cache trained as shipped, or on a Skia launch (Linux only) |
| J4 | `j4-glass.sh` | Part B, effects: the #1812 glass recipe on one panel, on the GPU layer and on Skia's CPU raster |
