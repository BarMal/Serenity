# Performance Benchmarks

Run the local performance harness with:

```text
sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"
```

The cursor-only scenario opens a temporary Swing window so it can measure Serenity's real overlay publication path. Run it in a graphical session; a headless Linux environment can use `xvfb-run -a sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks"`.

The harness prints CSV rows with `min_ms`, `p50_ms`, `p95_ms`, and `max_ms`. Every scenario builds its immutable document, state, search, LSP, and project-task fixtures before timing, runs the measured operation once, and asserts its observable result before warmup. Java2D frame and cursor-overlay image allocation remain inside their timed paths because those allocations, drawing, copying, and repaint requests are part of the user-visible work being measured.

Scenarios cover:

- large JSON rope search and cursor-offset lookup
- visible multiline layout, normal editing, and deep plain/rich-text scrolling reducers
- real Java2D full frames, cursor-overlay copying, diagnostics/comments, and HiDPI buffers
- large find/replace result-set presentation and complete find-query updates, including grapheme filtering, offset-to-position conversion, and selected-result application
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
| `rope.large_json.search` | 12.789 | 14.422 |
| `rope.large_json.cursor_offset` | 0.693 | 0.864 |
| `layout.large_multiline.visible_viewport` | 4.292 | 6.355 |
| `render.full_frame.java2d` | 11.284 | 15.223 |
| `render.cursor_only.java2d_overlay` | 5.158 | 10.270 |
| `render.diagnostics_and_comments.java2d` | 7.169 | 10.973 |
| `render.hidpi_frame.java2d` | 10.277 | 20.254 |
| `reducer.normal_editing` | 5.144 | 10.683 |
| `reducer.deep_scroll.plain` | 0.006 | 0.048 |
| `reducer.deep_scroll.rich_text` | 0.011 | 0.036 |
| `find_replace.large_result_set` | 0.255 | 0.460 |
| `lsp.framer.large_batch` | 3.864 | 5.803 |
| `project_task.responsiveness` | 0.025 | 0.028 |
| `markdown.preview.window_mapping` | 1.058 | 2.602 |
| `markdown.preview.html_fragment` | 0.523 | 0.550 |
| `render.markdown.inline_lens` | 10.894 | 13.942 |
| `animation.large_visible_tick` | 1.354 | 1.783 |

## After #827: 2026-07-22

Captured on the same x86_64 Linux WSL2 host and Microsoft OpenJDK 21.0.8+9-LTS runtime as the baseline. The changed rope paths were measured with the same harness invocation after warmup; times are milliseconds.

| Scenario | Before p50 | Before p95 | After p50 | After p95 |
| --- | ---: | ---: | ---: | ---: |
| `rope.large_json.search` | 12.789 | 14.422 | 1.626 | 1.779 |
| `layout.large_multiline.visible_viewport` | 4.292 | 6.355 | 3.355 | 5.303 |

## After #828: 2026-07-22

Captured on the same x86_64 Linux WSL2 host and Microsoft OpenJDK 21.0.8+9-LTS runtime. `find_replace.large_query_update` measures the complete work that previously ran synchronously during a find update: sequential rope search, grapheme filtering, offset-to-position conversion, and applying the selected result. `find_replace.large_query_keystroke` measures the reducer path after that work was moved behind the debounced, cancellable request boundary.

| Scenario | p50 | p95 |
| --- | ---: | ---: |
| `find_replace.large_query_update` | 16.198 | 19.746 |
| `find_replace.large_query_keystroke` | 0.026 | 0.039 |

## #836 validation: 2026-07-28

The focused viewport regressions are covered by indexed-line tests: deep scrolling keeps source reads bounded, and visible annotation projections avoid expanding comment ranges outside the rendered lines. The Markdown block suite also covers active fenced blocks through 4,500 interior lines and long paragraph/list blocks without materializing line sets.

| Scenario | Result |
| --- | --- |
| `markdown.deep_scroll.no_fence.reads` | bounded indexed reads; 10,000-line fixture |
| `markdown.long_fenced_block.interior` | passes through 4,500 lines |
| `renderer.focused_body.long_block` | range predicate; no block-sized `Set` allocation |
| `renderer.diagnostics.visible_projection` | only visible lines retained in render annotations |
