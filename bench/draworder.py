#!/usr/bin/env python3
"""Scratch analysis for #2618: reads the log written with -Dserenity.skia.drawOrderLog=<file> and says how the
publishes, render callbacks and Skiko frame brackets are ordered, and what the in-app `paint` stage would read.

    python3 -I bench/draworder.py LOG [LOG ...]

Each line is "<nanoTime> <thread> <kind> <per-kind counter>". Kinds: publish, carets, render-start, render-end,
skiko-before, skiko-after, draw-end-edt. Draw k is render-start k ... draw-end-edt k (the counters of a kind advance
once per draw).
"""
import statistics
import sys
from collections import Counter


def load(path):
    events = []
    with open(path) as handle:
        for line in handle:
            parts = line.split()
            if len(parts) == 4:
                events.append((int(parts[0]) / 1e6, parts[1], parts[2], int(parts[3])))
    events.sort(key=lambda event: event[0])
    return events


def pct(values, fraction):
    ordered = sorted(values)
    return ordered[max(0, min(len(ordered) - 1, int(fraction * len(ordered) + 0.9999) - 1))]


def line(name, values):
    if not values:
        return f"  {name:<46} none"
    return (f"  {name:<46} n={len(values):<4} p50={pct(values, .5):8.2f} p95={pct(values, .95):8.2f} "
            f"max={max(values):8.2f} ms")


def analyse(path):
    events = load(path)
    by = {}
    for time, thread, kind, number in events:
        by.setdefault(kind, {})[number] = (time, thread)
    print(f"== {path}: {len(events)} events, kinds {dict(Counter(e[2] for e in events))}")
    threads = {kind: sorted({thread for _, thread in values.values()}) for kind, values in by.items()}
    print(f"  threads per kind: {threads}")
    draws = sorted(set(by.get("render-start", {})) & set(by.get("draw-end-edt", {})))
    gaps = {
        "render callback (start to end)": [], "end of callback to skiko-before": [],
        "skiko frame bracket (before to after)": [], "skiko-after to draw-end on the event thread": [],
        "draw start to its own draw end": [],
    }
    for k in draws:
        start = by["render-start"][k][0]
        for name, first, second in (
            ("render callback (start to end)", "render-start", "render-end"),
            ("end of callback to skiko-before", "render-end", "skiko-before"),
            ("skiko frame bracket (before to after)", "skiko-before", "skiko-after"),
            ("skiko-after to draw-end on the event thread", "skiko-after", "draw-end-edt"),
        ):
            if k in by.get(first, {}) and k in by.get(second, {}):
                gaps[name].append(by[second][k][0] - by[first][k][0])
        gaps["draw start to its own draw end"].append(by["draw-end-edt"][k][0] - start)
    print("Per draw (matched by counter):")
    for name, values in gaps.items():
        print(line(name, values))

    starts = sorted((time, number) for number, (time, _) in by.get("render-start", {}).items())
    ends = sorted((time, number) for number, (time, _) in by.get("draw-end-edt", {}).items())
    emulated, own, stray = [], [], 0
    for time, number in sorted((t, n) for n, (t, _) in by.get("publish", {}).items()):
        later = [(t, n) for t, n in starts if t >= time]
        if not later:
            continue
        start_time, start_number = later[0]
        after = [(t, n) for t, n in ends if t >= start_time]
        if not after:
            continue
        end_time, end_number = after[0]
        emulated.append(end_time - start_time)
        if end_number in by["draw-end-edt"] and start_number in by["draw-end-edt"]:
            own.append(by["draw-end-edt"][start_number][0] - start_time)
        if end_number < start_number:
            stray += 1
    print("Per publish:")
    print(line("emulated `paint` (first draw start to first end)", emulated))
    print(line("true paint (that draw's own end)", own))
    print(f"  publishes whose first end after the draw start is an EARLIER draw's end: {stray} of {len(emulated)}")
    publish_to_end = []
    for time, number in sorted((t, n) for n, (t, _) in by.get("publish", {}).items()):
        later = [(t, n) for t, n in starts if t >= time]
        if later and later[0][1] in by.get("draw-end-edt", {}):
            publish_to_end.append(by["draw-end-edt"][later[0][1]][0] - time)
    print(line("publish to the end of the draw that shows it", publish_to_end))
    print(f"  carets events: {len(by.get('carets', {}))}, publishes: {len(by.get('publish', {}))}, draws: {len(by.get('render-start', {}))}")
    print("First 3 publishes, as event order until the next publish:")
    publishes = sorted((t, n) for n, (t, _) in by.get("publish", {}).items())
    for index, (time, number) in enumerate(publishes[:3]):
        stop = publishes[index + 1][0] if index + 1 < len(publishes) else float("inf")
        window = [(t - time, kind, n) for t, _, kind, n in events if time <= t < stop][:14]
        print(f"  publish {number}: " + " ".join(f"{kind}{n}@{t:.1f}" for t, kind, n in window))


if __name__ == "__main__":
    for argument in sys.argv[1:]:
        analyse(argument)
