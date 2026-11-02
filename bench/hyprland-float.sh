#!/usr/bin/env bash
# Makes Hyprland float the render-benchmark window at the size it asks for, instead of tiling it (#2480).
# On a tiling manager the window stays at the tile's size, whatever the benchmark asks for.
#
#   hyprland-float.sh on [WIDTH HEIGHT]   add the float and size rules (default 1600 1000, logical pixels)
#   hyprland-float.sh off                 drop every rule added at run time
#   hyprland-float.sh status              show the benchmark window as Hyprland sees it
#
# `off` is `hyprctl reload`: Hyprland has no way to remove one rule, so a rule or keyword set by hand since the
# config was last read goes too.

WINDOW_TITLE='^Serenity render benchmark'

hypr_ok() { [[ "$(hyprctl "$@" 2>&1)" == ok* ]]; }

# The rule syntax changed between Hyprland releases, so the older form is tried and then the newer.
add_rules() {
  local width="$1" height="$2"
  if hypr_ok keyword windowrulev2 "float,title:$WINDOW_TITLE" \
    && hypr_ok keyword windowrulev2 "size $width $height,title:$WINDOW_TITLE" \
    && hypr_ok keyword windowrulev2 "center,title:$WINDOW_TITLE"; then
    echo "float rules added (windowrulev2)"
  elif hypr_ok keyword windowrule "float on, match:title $WINDOW_TITLE" \
    && hypr_ok keyword windowrule "size $width $height, match:title $WINDOW_TITLE" \
    && hypr_ok keyword windowrule "center on, match:title $WINDOW_TITLE"; then
    echo "float rules added (windowrule)"
  else
    echo "hyprctl refused the float rules in both syntaxes" >&2
    return 1
  fi
}

main() {
  command -v hyprctl >/dev/null || { echo "hyprctl not found: this is for Hyprland" >&2; return 2; }
  case "${1:-}" in
    on) add_rules "${2:-1600}" "${3:-1000}" ;;
    off) hyprctl reload >/dev/null && echo "rules reloaded away" ;;
    status) hyprctl clients | grep -A12 "title: Serenity render benchmark" || echo "no benchmark window" ;;
    *) echo "usage: $0 on [WIDTH HEIGHT] | off | status" >&2; return 2 ;;
  esac
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then main "$@"; fi
