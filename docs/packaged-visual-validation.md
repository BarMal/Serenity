# Packaged visual validation

Use this checklist before closing a P0 or P1 visual issue. Scenario coverage is a regression contract; it does not replace packaged-app validation.

1. Record the exact release commit and packaged asset digest.
2. Start from a clean, isolated Serenity home with no user configuration or session files.
3. Record the exact reproduction actions and expected visible result.
4. Attach a screenshot or short recording of the packaged application.
5. Record the tester and platform, including operating-system version and device scale.
6. Run the independently callable matching UI scenario and attach its result. If needed, enable diagnostic PNG output; files are written under `test-results/ui-scenarios/` and are ignored by Git.

The issue must link both the maintained scenario and this completed packaged proof before it is closed.
