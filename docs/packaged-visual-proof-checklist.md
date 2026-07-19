# Packaged visual proof checklist

Complete this checklist before closing a P0 or P1 visual issue.

- Record the exact release commit and packaged asset digest.
- Use a clean, isolated Serenity home and note the platform and tester.
- Record the precise repro actions and expected result.
- Attach a screenshot or short recording from the packaged application.
- Confirm the relevant deterministic UI scenario passed and state whether PNG diagnostics were retained.

For a preset transaction, the repro record must name the preset, identify the initial effective workspace, and include both
of these sequences:

1. Create or edit → preview changes → reopen the runner → Discard → verify the original workspace returns.
2. Create or edit → preview changes → Save → Apply after restart → verify the saved workspace returns.

For each sequence, record whether diagnostics were emitted and, if so, whether they were retained with the evidence.

The scenario suite is regression evidence, not a replacement for the packaged proof: it runs headlessly with fixed fonts, metrics, viewport, theme, and device scale, while the packaged check verifies the delivered desktop artifact.
