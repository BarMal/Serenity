#!/usr/bin/env python3
"""Throwaway (#2380, never merge). Reads a WAYLAND_DEBUG=1 client log and reports:
  - frame-callback intervals (wl_callback.done timestamps the compositor sends for wl_surface.frame), and
  - the damage rectangles the client sent (wl_surface.damage_buffer), against the buffer size.
usage: analyze.py WIRE_LOG BUFFER_WIDTH BUFFER_HEIGHT
"""
import re
import sys
from collections import Counter

path, bw, bh = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])

frame_ids = set()
done_ms = []
damage = Counter()
commits = 0

re_sync = re.compile(r'wl_display@\d+\.sync\(new id wl_callback@(\d+)\)')
re_frame = re.compile(r'wl_surface@\d+\.frame\(new id wl_callback@(\d+)\)')
re_done = re.compile(r'wl_callback@(\d+)\.done\((\d+)\)')
re_damage = re.compile(r'wl_surface@\d+\.damage_buffer\((-?\d+), (-?\d+), (-?\d+), (-?\d+)\)')
re_commit = re.compile(r'-> wl_surface@\d+\.commit\(\)')

with open(path, errors='replace') as f:
    for line in f:
        m = re_sync.search(line)
        if m:
            frame_ids.discard(m.group(1))  # ids are reused; a sync callback's done() carries a serial, not a time
            continue
        m = re_frame.search(line)
        if m:
            frame_ids.add(m.group(1))
            continue
        m = re_done.search(line)
        if m and m.group(1) in frame_ids and '->' not in line:
            frame_ids.discard(m.group(1))
            done_ms.append(int(m.group(2)))
            continue
        m = re_damage.search(line)
        if m:
            damage[tuple(map(int, m.groups()))] += 1
            continue
        if re_commit.search(line):
            commits += 1


def pct(sorted_values, p):
    return sorted_values[min(len(sorted_values) - 1, int(p * len(sorted_values)))]


print(f'wire: commits={commits} damage_buffer_calls={sum(damage.values())} frame_callbacks={len(done_ms)}')
if len(done_ms) > 2:
    iv = sorted(b - a for a, b in zip(done_ms, done_ms[1:]) if b >= a)
    med = pct(iv, .5)
    late = sum(1 for v in iv if v > 1.5 * med)
    span = max(0.001, (max(done_ms) - min(done_ms)) / 1000.0)
    print(f'frame-callback interval (compositor clock, 1 ms resolution): n={len(iv)} p50={med} ms p95={pct(iv, .95)} ms '
          f'p99={pct(iv, .99)} ms max={iv[-1]} ms late(>1.5x p50)={late} rate={len(done_ms) / span:.2f}/s')
else:
    print('frame-callback interval: too few callbacks (window hidden or compositor never sent frame callbacks?)')

full = sum(n for r, n in damage.items() if r[0] <= 0 and r[1] <= 0 and r[2] >= bw and r[3] >= bh)
total = sum(damage.values())
print(f'damage: buffer={bw}x{bh} full-surface-or-unbounded={full} of {total}; distinct rectangles={len(damage)}')
for r, n in damage.most_common(5):
    print(f'  damage_buffer{r} x{n}')
