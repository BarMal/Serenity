#!/usr/bin/env bash
# Helpers bench-scaling.sh sources. They live apart so scripts/test_bench_scaling.py can source them too.

# The steps that put a different buffer in each of 1, 2 or 4 panes of a window that has opened at least 4 files in order,
# one a line: `key <xdotool key>` or `click <left-top|right-top|...>` (a click on that quarter of the window focuses the
# pane there). The app's rules, which the test replays: a split opens a pane on its parent's buffer and focuses it
# (Ctrl+Alt+V side by side, Ctrl+Shift+D stacked); Ctrl+Tab moves the focused pane to the next open buffer.
scaling_split_plan() {
  case "$1" in
    1) ;;
    2)
      echo "key ctrl+alt+v"
      echo "key ctrl+Tab"
      ;;
    4)
      echo "key ctrl+alt+v"
      echo "key ctrl+Tab"
      echo "key ctrl+shift+d"
      echo "key ctrl+Tab"
      echo "click left-top"
      echo "key ctrl+shift+d"
      echo "key ctrl+Tab"
      echo "key ctrl+Tab"
      echo "key ctrl+Tab"
      ;;
    *)
      echo "scaling_split_plan: $1 panes; the rows ask for 1, 2 or 4" >&2
      return 1
      ;;
  esac
}
