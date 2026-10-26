#!/usr/bin/env python3
"""Throwaway (#2380): edit the java-options of a jpackage Serenity.cfg.   cfgtool.py <cfg> drop:<substring> add:<option> ..."""
import sys

path, ops = sys.argv[1], sys.argv[2:]
lines = open(path).read().splitlines()
for op in ops:
    kind, _, value = op.partition(":")
    if kind == "drop":
        lines = [l for l in lines if not (l.startswith("java-options=") and value in l)]
    elif kind == "add":
        at = lines.index("[JavaOptions]") + 1
        lines.insert(at, "java-options=" + value)
open(path, "w").write("\n".join(lines) + "\n")
