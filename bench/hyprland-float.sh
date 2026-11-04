#!/usr/bin/env bash
# Makes Hyprland float the render-benchmark window at the size it asks for, instead of tiling it (#2480).
# On a tiling manager the window stays at the tile's size, whatever the benchmark asks for.
#
#   hyprland-float.sh on [WIDTH HEIGHT]   add the float and size rules (default 1600 1000, logical pixels)
#   hyprland-float.sh off                 drop every rule added at run time
#   hyprland-float.sh status              show the benchmark window as Hyprland sees it
#
# `status` reads `hyprctl clients -j`, so it needs `jq`. `on` tries the hyprlang `keyword` forms and then, for a Lua
# config (Hyprland 0.55 and later, where `keyword` is refused), `hyprctl eval` with an `hl.window_rule` call.
#
# `off` is `hyprctl reload`: Hyprland has no way to remove one rule, so a rule or keyword set by hand since the
# config was last read goes too.

WINDOW_TITLE='^Serenity render benchmark'

hypr_ok() { [[ "$(hyprctl "$@" 2>&1)" == ok* ]]; }

# A Lua config rejects `hyprctl keyword`, so the rule goes in as Lua. The value format of `size` is not confirmed against
# the 0.55 documentation, so the string form is tried and then the table form.
add_lua_rule() {
  local width="$1" height="$2" size
  for size in "\"$width $height\"" "{$width, $height}"; do
    if hypr_ok eval "hl.window_rule({ name = \"serenity-render-benchmark\", match = { title = \"$WINDOW_TITLE\" }, float = true, size = $size, center = true })"; then
      return 0
    fi
  done
  return 1
}

# The rule syntax changed between Hyprland releases, so the older form is tried, then the newer, then Lua.
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
  elif add_lua_rule "$width" "$height"; then
    echo "float rules added (eval)"
  else
    echo "hyprctl refused the float rules in every syntax (windowrulev2, windowrule, eval)" >&2
    return 1
  fi
}

show_window() {
  command -v jq >/dev/null || { echo "jq not found: status reads hyprctl clients -j" >&2; return 2; }
  local report
  report="$(hyprctl clients -j | jq -r --arg prefix "${WINDOW_TITLE#^}" '
    .[] | select(.title | startswith($prefix))
    | "title: \(.title)", "floating: \(if .floating then 1 else 0 end)", "size: \(.size | join(","))", "at: \(.at | join(","))"')"
  echo "${report:-no benchmark window}"
}

main() {
  command -v hyprctl >/dev/null || { echo "hyprctl not found: this is for Hyprland" >&2; return 2; }
  case "${1:-}" in
    on) add_rules "${2:-1600}" "${3:-1000}" ;;
    off) hyprctl reload >/dev/null && echo "rules reloaded away" ;;
    status) show_window ;;
    *) echo "usage: $0 on [WIDTH HEIGHT] | off | status" >&2; return 2 ;;
  esac
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then main "$@"; fi
