#!/usr/bin/env python3
"""Turns the raw captures taken by scripts/measure-footprint.sh into one CSV row.

    footprint_parse.py header
    footprint_parse.py row LABEL MODE CHECKPOINT UPTIME_S STATUS SMAPS_FILE HEAP_FILE [NMT_FILE]

Sizes are KiB. STATUS is "ok" or the reason the run is suspect. A figure the capture does not carry (no NMT, a collector whose heap_info this does not read) is left
blank rather than reported as zero.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

_SMAPS_FIELDS = {
    "Rss": "rss_kb",
    "Pss": "pss_kb",
    "Pss_Anon": "pss_anon_kb",
    "Pss_File": "pss_file_kb",
}

_HEAP_LINE = re.compile(r"^ (?!Metaspace)\S.*?\s+total (?:reserved \d+K, committed )?(\d+)K, used (\d+)K")
_METASPACE_LINE = re.compile(r"^ Metaspace\s+used (\d+)K, committed (\d+)K")

_NMT_TOTAL = re.compile(r"^Total: reserved=\d+KB, committed=(\d+)KB")
_NMT_CATEGORY = re.compile(r"^-\s+(.+?) \(reserved=\d+KB, committed=(\d+)KB")
_NMT_THREADS = re.compile(r"\(threads #(\d+)\)")

# NMT category -> column. Categories listed together are summed into one column.
_NMT_COLUMNS: dict[str, tuple[str, ...]] = {
    "nmt_java_heap_kb": ("Java Heap",),
    "nmt_class_kb": ("Class",),
    "nmt_metaspace_kb": ("Metaspace",),
    "nmt_thread_kb": ("Thread",),
    "nmt_code_kb": ("Code",),
    "nmt_gc_kb": ("GC", "GCCardSet"),
    "nmt_compiler_kb": ("Compiler",),
    "nmt_internal_kb": ("Internal",),
    "nmt_symbol_kb": ("Symbol",),
    "nmt_cds_kb": ("Shared class space",),
    "nmt_other_kb": ("Other",),
}
NMT_NAMED = tuple(_NMT_COLUMNS)

_SMAPS_COLUMNS = ["rss_kb", "pss_kb", "pss_anon_kb", "pss_file_kb"]
_HEAP_COLUMNS = ["heap_used_kb", "heap_committed_kb", "metaspace_used_kb", "metaspace_committed_kb"]
_NMT_ALL_COLUMNS = ["nmt_total_committed_kb", *NMT_NAMED, "nmt_misc_kb", "threads"]

COLUMNS = [
    "label",
    "mode",
    "checkpoint",
    "uptime_s",
    *_SMAPS_COLUMNS,
    *_HEAP_COLUMNS,
    *_NMT_ALL_COLUMNS,
    "anon_minus_nmt_kb",
    "status",
]
HEADER = ",".join(COLUMNS)


def parse_smaps_rollup(text: str) -> dict[str, int | None]:
    sizes: dict[str, int | None] = {column: None for column in _SMAPS_COLUMNS}
    for line in text.splitlines():
        name, _, rest = line.partition(":")
        column = _SMAPS_FIELDS.get(name)
        if column is not None:
            sizes[column] = int(rest.split()[0])
    return sizes


def parse_heap_info(text: str) -> dict[str, int | None]:
    heap: dict[str, int | None] = {column: None for column in _HEAP_COLUMNS}
    committed = used = 0
    found = False
    for line in text.splitlines():
        match = _HEAP_LINE.match(line)
        if match:
            found = True
            committed += int(match.group(1))
            used += int(match.group(2))
            continue
        match = _METASPACE_LINE.match(line)
        if match:
            heap["metaspace_used_kb"] = int(match.group(1))
            heap["metaspace_committed_kb"] = int(match.group(2))
    if found:
        heap["heap_committed_kb"] = committed
        heap["heap_used_kb"] = used
    return heap


def parse_nmt(text: str) -> dict[str, int | None]:
    blank: dict[str, int | None] = {column: None for column in _NMT_ALL_COLUMNS}
    total: int | None = None
    categories: dict[str, int] = {}
    threads: int | None = None
    for line in text.splitlines():
        match = _NMT_TOTAL.match(line)
        if match:
            total = int(match.group(1))
            continue
        match = _NMT_CATEGORY.match(line)
        if match:
            categories[match.group(1)] = int(match.group(2))
            continue
        match = _NMT_THREADS.search(line)
        if match:
            threads = int(match.group(1))
    if total is None:
        return blank
    nmt: dict[str, int | None] = {"nmt_total_committed_kb": total, "threads": threads}
    for column, names in _NMT_COLUMNS.items():
        nmt[column] = sum(categories.get(name, 0) for name in names)
    nmt["nmt_misc_kb"] = total - sum(nmt[column] or 0 for column in NMT_NAMED)
    return nmt


def build_row(
    label: str, mode: str, checkpoint: str, uptime_s: int, smaps: str, heap: str, nmt: str, status: str = "ok"
) -> str:
    values: dict[str, int | str | None] = {
        "label": label,
        "mode": mode,
        "checkpoint": checkpoint,
        "uptime_s": uptime_s,
        **parse_smaps_rollup(smaps),
        **parse_heap_info(heap),
        **parse_nmt(nmt),
        "status": status,
    }
    pss_anon, tracked = values["pss_anon_kb"], values["nmt_total_committed_kb"]
    values["anon_minus_nmt_kb"] = (
        int(pss_anon) - int(tracked) if isinstance(pss_anon, int) and isinstance(tracked, int) else None
    )
    return ",".join("" if values[column] is None else str(values[column]) for column in COLUMNS)


def _read(path: str | None) -> str:
    if path is None or not Path(path).is_file():
        return ""
    return Path(path).read_text()


def main(argv: list[str]) -> int:
    if argv[:1] == ["header"]:
        print(HEADER)
        return 0
    if argv[:1] == ["row"] and len(argv) in (8, 9):
        label, mode, checkpoint, uptime, status, smaps, heap = argv[1:8]
        nmt = argv[8] if len(argv) == 9 else None
        print(build_row(label, mode, checkpoint, int(uptime), _read(smaps), _read(heap), _read(nmt), status))
        return 0
    print(__doc__, file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
