# Performance Benchmarks

Run the local performance harness with:

```text
sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"
```

The cursor-only scenario opens a temporary Swing window so it can measure Serenity's real overlay publication path. Run it in a graphical session; a headless Linux environment can use `xvfb-run -a sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"`.

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
| `rope.large_json.search` | 12.184 | 14.570 |
| `rope.large_json.cursor_offset` | 0.698 | 0.829 |
| `layout.large_multiline.visible_viewport` | 4.901 | 8.302 |
| `render.full_frame.java2d` | 9.792 | 13.047 |
| `render.cursor_only.java2d_overlay` | 7.170 | 12.525 |
| `render.diagnostics_and_comments.java2d` | 6.717 | 7.524 |
| `render.hidpi_frame.java2d` | 10.858 | 12.973 |
| `reducer.normal_editing` | 4.223 | 6.973 |
| `reducer.deep_scroll.plain` | 0.006 | 0.029 |
| `reducer.deep_scroll.rich_text` | 0.006 | 0.011 |
| `find_replace.large_result_set` | 0.559 | 0.836 |
| `lsp.framer.large_batch` | 3.929 | 6.969 |
| `project_task.responsiveness` | 0.075 | 0.089 |
| `markdown.preview.window_mapping` | 2.180 | 2.283 |
| `markdown.preview.html_fragment` | 0.543 | 2.166 |
| `render.markdown.inline_lens` | 18.431 | 22.116 |
| `animation.large_visible_tick` | 1.307 | 2.034 |
