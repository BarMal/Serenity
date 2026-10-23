#define _DEFAULT_SOURCE
// Throwaway (#2380, never merge): a persistent wlr-virtual-pointer so the seat keeps its pointer capability.
// Reads commands on stdin: "abs X Y W H" | "click L|R" | "scroll N" | "sleep MS" | "quit".
#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include "wlr-virtual-pointer-unstable-v1-client-protocol.h"

static struct wl_seat *seat; static struct zwlr_virtual_pointer_manager_v1 *mgr;
static void global(void *d, struct wl_registry *r, uint32_t name, const char *iface, uint32_t v) {
  if (!strcmp(iface, "wl_seat") && !seat) seat = wl_registry_bind(r, name, &wl_seat_interface, 1);
  else if (!strcmp(iface, "zwlr_virtual_pointer_manager_v1")) mgr = wl_registry_bind(r, name, &zwlr_virtual_pointer_manager_v1_interface, v < 2 ? v : 2);
}
static void gremove(void *d, struct wl_registry *r, uint32_t n) {}
static const struct wl_registry_listener rl = { global, gremove };
static uint32_t now(void) { struct timespec t; clock_gettime(CLOCK_MONOTONIC, &t); return t.tv_sec * 1000 + t.tv_nsec / 1000000; }

int main(void) {
  struct wl_display *dpy = wl_display_connect(NULL);
  if (!dpy) { fprintf(stderr, "no display\n"); return 1; }
  struct wl_registry *reg = wl_display_get_registry(dpy);
  wl_registry_add_listener(reg, &rl, NULL);
  wl_display_roundtrip(dpy);
  if (!seat || !mgr) { fprintf(stderr, "missing seat or virtual pointer manager\n"); return 2; }
  struct zwlr_virtual_pointer_v1 *p = zwlr_virtual_pointer_manager_v1_create_virtual_pointer(mgr, seat);
  wl_display_roundtrip(dpy);
  printf("vptr ready\n"); fflush(stdout);
  char line[256];
  while (fgets(line, sizeof line, stdin)) {
    int x, y, w, h, n, ms; char c;
    if (sscanf(line, "abs %d %d %d %d", &x, &y, &w, &h) == 4) {
      zwlr_virtual_pointer_v1_motion_absolute(p, now(), x, y, w, h); zwlr_virtual_pointer_v1_frame(p);
    } else if (sscanf(line, "click %c", &c) == 1) {
      uint32_t b = c == 'R' ? 0x111 : 0x110; // BTN_RIGHT : BTN_LEFT
      zwlr_virtual_pointer_v1_button(p, now(), b, WL_POINTER_BUTTON_STATE_PRESSED); zwlr_virtual_pointer_v1_frame(p);
      zwlr_virtual_pointer_v1_button(p, now(), b, WL_POINTER_BUTTON_STATE_RELEASED); zwlr_virtual_pointer_v1_frame(p);
    } else if (sscanf(line, "scroll %d", &n) == 1) {
      zwlr_virtual_pointer_v1_axis_discrete(p, now(), WL_POINTER_AXIS_VERTICAL_SCROLL, wl_fixed_from_int(n * 10), n > 0 ? 1 : -1);
      zwlr_virtual_pointer_v1_frame(p);
    } else if (sscanf(line, "sleep %d", &ms) == 1) { wl_display_flush(dpy); usleep(ms * 1000); }
    else if (!strncmp(line, "quit", 4)) break;
    wl_display_flush(dpy);
  }
  wl_display_roundtrip(dpy);
  return 0;
}
