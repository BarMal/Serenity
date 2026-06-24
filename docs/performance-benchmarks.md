# Performance Benchmarks

Run the local performance harness with:

```text
sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"
```

The harness prints CSV rows with `min_ms`, `median_ms`, and `max_ms` for repeatable hot-path scenarios:

- large single-line JSON rope search and cursor offset lookup
- large multi-line visible viewport layout
- rich-text rendering on a large document
- document comment rendering on a large visible viewport
- Markdown preview source-window mapping and HTML fragment rendering
- inline Markdown lens rendering
- large visible animation tick advancement

Use this before and after rope constant changes, render-loop rewrites, markdown preview changes, and animation changes. CI still runs correctness checks; this harness is intended for manual baseline capture during performance PRs and release checks.
