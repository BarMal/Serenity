#!/usr/bin/env python3
"""Aggregate the execution samples of a JFR recording of the per-keystroke state path.

Usage:
    python3 bench/jfr-aggregate.py RECORDING.jfr [--top N]

Needs the `jfr` tool from the JDK on PATH. Only samples whose stack passes through application code (com.serenity.*,
excluding the com.serenity.perf driver itself) are kept, so idle and JIT threads do not dilute the shares. Prints:

  1. the inclusive share per phase (a sample counts for every phase found on its stack, so shares sum past 100%),
  2. the top N self frames (the method on top of the stack),
  3. the top N inclusive com.serenity frames.

N defaults to 20. Shares are only comparable between recordings made on the same machine with the same flags; see
bench/README.md, "Profiling the per-keystroke state path with JFR".
"""
import argparse
import collections
import json
import re
import subprocess

PHASES = [
    (
        "event translation and dispatch (StateManagerEventPipeline, EventPipelineTransitions, StateManagerOperationBoundary, InputRouter, FocusedInputTranslator)",
        r"StateManagerEventPipeline|EventPipelineTransitions|StateManagerOperationBoundary|InputRouter|FocusedInputTranslator",
    ),
    (
        "reducers (com.serenity.state.reducers, VerticalNavSupport, EditorEditSupport)",
        r"com\.serenity\.state\.reducers\.|VerticalNavSupport|EditorEditSupport",
    ),
    (
        "commit and validation (ModelCommit, AppStateValidation)",
        r"ModelCommit|AppStateValidation",
    ),
    (
        "effect pipeline (StateManagerEffectDispatcher, StateManagerEffectHandlers, EffectResult, EffectLanes)",
        r"StateManagerEffectDispatcher|StateManagerEffectHandlers|EffectResult|EffectLanes",
    ),
    ("cursor viewport (CursorViewport)", r"CursorViewport"),
    ("wrapped line cache (WrappedLineCache)", r"WrappedLineCache"),
    (
        "layout snapshot and visual line index (TextLayoutSnapshot, VisualLineIndex, TextVisualLine)",
        r"TextLayoutSnapshot|VisualLineIndex|TextVisualLine",
    ),
    (
        "text measurement (ParagraphMeasurement, GlyphAdvances, TextCaretMeasurement)",
        r"ParagraphMeasurement|GlyphAdvances|TextCaretMeasurement",
    ),
    (
        "AWT font shaping (java.awt.font, sun.font, GlyphVector, FontRenderContext, java.awt.Font)",
        r"java\.awt\.font|sun\.font|GlyphVector|FontRenderContext|java\.awt\.Font",
    ),
    (
        "word-break iteration (wordBoundarySegmentLength, BreakIterator)",
        r"wordBoundarySegmentLength|BreakIterator",
    ),
    ("damage computation (DamageProducer)", r"DamageProducer"),
    ("rope edits (com.serenity.rope)", r"com\.serenity\.rope\."),
    (
        "document analysis and outline scheduling (scheduleDocumentAnalysis, scheduleOutlineRefresh, SpellCheck, Lsp)",
        r"scheduleDocumentAnalysis|scheduleOutlineRefresh|DocumentAnalysis|SpellCheck|Lsp",
    ),
    (
        "session and file persistence (SessionWorkflowTransitions, StateManagerFilePersistence, FileWriteLedger)",
        r"SessionWorkflowTransitions|StateManagerFilePersistence|FileWriteLedger",
    ),
    (
        "cats-effect runtime (IOFiber, cats.effect.unsafe, WorkStealing)",
        r"IOFiber|cats\.effect\.unsafe|WorkStealing",
    ),
]


def frame_name(frame):
    method = frame["method"]
    return f"{method['type']['name']}.{method['name']}".replace("/", ".")


def application_samples(path):
    printed = subprocess.run(
        ["jfr", "print", "--json", "--events", "jdk.ExecutionSample", "--stack-depth", "96", path],
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    events = json.loads(printed)["recording"]["events"]
    stacks = [[frame_name(f) for f in e["values"]["stackTrace"]["frames"]] for e in events]
    kept = [
        names
        for names in stacks
        if any(n.startswith("com.serenity.") and not n.startswith("com.serenity.perf.") for n in names)
    ]
    return len(events), kept


def print_shares(counter, total, limit):
    for name, count in counter.most_common(limit):
        print(f"  {100 * count / total:5.1f}%  {name}")


def main():
    parser = argparse.ArgumentParser(description="Aggregate JFR execution samples by phase and frame.")
    parser.add_argument("recording", help="a .jfr file recorded with jdk.ExecutionSample enabled")
    parser.add_argument("--top", type=int, default=20, metavar="N", help="frames to list per table (default 20)")
    args = parser.parse_args()

    event_count, samples = application_samples(args.recording)
    total = len(samples)
    print(f"{args.recording}: {event_count} samples total, {total} with application (com.serenity) frames")
    if total == 0:
        return

    print("\nInclusive share of application samples (a sample counts for every phase on its stack):")
    for label, pattern in PHASES:
        regex = re.compile(pattern)
        matching = sum(1 for names in samples if any(regex.search(n) for n in names))
        print(f"  {100 * matching / total:5.1f}%  {label}")

    print(f"\nTop {args.top} self frames:")
    print_shares(collections.Counter(names[0] for names in samples), total, args.top)

    inclusive = collections.Counter(n for names in samples for n in set(names) if n.startswith("com.serenity"))
    print(f"\nTop {args.top} inclusive com.serenity frames:")
    print_shares(inclusive, total, args.top)


if __name__ == "__main__":
    main()
