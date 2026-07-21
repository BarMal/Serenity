# Performance Benchmarks

Run the local performance harness with:

```text
sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"
```

The harness prints CSV rows with `min_ms`, `p50_ms`, `p95_ms`, and `max_ms`. Every scenario builds its immutable document, state, search, LSP, and project-task fixtures before timing, then checks the fixture contract before warmup. Java2D frame and cursor-overlay image allocation remain inside their timed paths because those allocations, drawing, copying, and repaint requests are part of the user-visible work being measured.

Scenarios cover:

- large JSON rope search and cursor-offset lookup
- visible multiline layout, normal editing, and deep plain/rich-text scrolling reducers
- real Java2D full frames, cursor-overlay copying, diagnostics/comments, and HiDPI buffers
- large find/replace result-set presentation
- LSP frame decoding and project-task detection/terminal preparation
- Markdown preview and inline-lens rendering
- visible animation tick advancement

This remains a manual comparison tool. CI runs correctness tests but has no absolute timing gate.

## Repeatable before/after workflow

1. Close CPU-intensive applications and use the same power and display-scale settings for both captures.
2. Run the command above once and discard that output if dependencies or classes were cold.
3. Run it again, save the CSV output with the commit SHA, and compare p50 and p95 for like-named scenarios.
4. Record the printed `context` rows with the results. Treat changes as signals for investigation, not pass/fail thresholds.

## Baseline: 2026-07-21

Captured on x86_64 Linux 5.15.153.1-microsoft-standard-WSL2, AMD Ryzen 5 5600X 6-Core Processor (12 available processors), Microsoft OpenJDK Runtime 21.0.8+9-LTS. Times are milliseconds.

| Scenario | p50 | p95 |
| --- | ---: | ---: |
| `rope.large_json.search` | 9.037 | 12.737 |
| `rope.large_json.cursor_offset` | 0.554 | 0.568 |
| `layout.large_multiline.visible_viewport` | 5.287 | 8.142 |
| `render.full_frame.java2d` | 10.671 | 15.119 |
| `render.cursor_only.java2d_overlay` | 0.872 | 0.896 |
| `render.diagnostics_and_comments.java2d` | 6.715 | 12.550 |
| `render.hidpi_frame.java2d` | 9.116 | 19.484 |
| `reducer.normal_editing` | 4.709 | 9.263 |
| `reducer.deep_scroll.plain` | 0.010 | 0.056 |
| `reducer.deep_scroll.rich_text` | 0.007 | 0.060 |
| `find_replace.large_result_set` | 0.423 | 0.848 |
| `lsp.framer.large_batch` | 3.530 | 5.787 |
| `project_task.responsiveness` | 0.052 | 0.066 |
| `markdown.preview.window_mapping` | 2.069 | 2.931 |
| `markdown.preview.html_fragment` | 0.773 | 1.308 |
| `render.markdown.inline_lens` | 12.759 | 15.686 |
| `animation.large_visible_tick` | 1.284 | 2.358 |
