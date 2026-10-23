import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.nio.file.*;
import java.util.*;
import java.util.List;

import org.lwjgl.sdl.*;
import org.lwjgl.system.MemoryStack;

import static org.lwjgl.sdl.SDLClipboard.*;
import static org.lwjgl.sdl.SDLError.*;
import static org.lwjgl.sdl.SDLEvents.*;
import static org.lwjgl.sdl.SDLHints.*;
import static org.lwjgl.sdl.SDLInit.*;
import static org.lwjgl.sdl.SDLKeyboard.*;
import static org.lwjgl.sdl.SDLMouse.*;
import static org.lwjgl.sdl.SDLVideo.*;

/**
 * Throwaway SDL3 spike for #2380 (never merge). Gates 1-7 of the nojbr SDL spike:
 * present (partial damage), cost, scale, input, clipboard, coexistence with headless Java2D (+Skia raster), footprint.
 * Phases are paced by go.N files in the output dir so a shell script can screenshot between steps.
 */
public final class SdlSpike {
  static Path out;
  static long window;
  static final SDL_Event ev = SDL_Event.malloc();
  static final StringBuilder evlog = new StringBuilder();
  static long t0 = System.nanoTime();
  static volatile boolean resized = false;

  static void say(String s) { System.out.println(s); System.out.flush(); }

  static void logEvent(String s) {
    String line = String.format("%8.1fms %s", (System.nanoTime() - t0) / 1e6, s);
    evlog.append(line).append('\n');
    System.out.println("EV " + line);
    System.out.flush();
  }

  /** Pump events for up to `ms` milliseconds, logging them. */
  static void pump(int ms) {
    long end = System.nanoTime() + ms * 1_000_000L;
    do {
      int wait = (int) Math.max(1, Math.min(20, (end - System.nanoTime()) / 1_000_000L));
      if (SDL_WaitEventTimeout(ev, wait)) handle();
      while (SDL_PollEvent(ev)) handle();
    } while (System.nanoTime() < end);
  }

  static void handle() {
    int t = ev.type();
    if (t == SDL_EVENT_KEY_DOWN || t == SDL_EVENT_KEY_UP) {
      SDL_KeyboardEvent k = ev.key();
      logEvent("key down=" + k.down() + " repeat=" + k.repeat() + " scancode=" + k.scancode()
          + " key=0x" + Integer.toHexString(k.key()) + " name=" + SDL_GetKeyName(k.key()));
    } else if (t == SDL_EVENT_TEXT_INPUT) {
      logEvent("text_input '" + ev.text().textString() + "' cps=" + cps(ev.text().textString()));
    } else if (t == SDL_EVENT_TEXT_EDITING) {
      SDL_TextEditingEvent e = ev.edit();
      logEvent("text_editing '" + e.textString() + "' cps=" + cps(e.textString()) + " start=" + e.start() + " length=" + e.length());
    } else if (t == SDL_EVENT_TEXT_EDITING_CANDIDATES) {
      logEvent("text_editing_candidates");
    } else if (t == SDL_EVENT_MOUSE_MOTION) {
      logEvent("mouse_motion x=" + ev.motion().x() + " y=" + ev.motion().y());
    } else if (t == SDL_EVENT_MOUSE_BUTTON_DOWN || t == SDL_EVENT_MOUSE_BUTTON_UP) {
      logEvent("mouse_button down=" + ev.button().down() + " x=" + ev.button().x() + " y=" + ev.button().y());
    } else if (t == SDL_EVENT_MOUSE_WHEEL) {
      logEvent("mouse_wheel x=" + ev.wheel().x() + " y=" + ev.wheel().y());
    } else if (t == SDL_EVENT_CLIPBOARD_UPDATE) {
      logEvent("clipboard_update");
    } else if (t == SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED) {
      logEvent("window_pixel_size_changed " + ev.window().data1() + "x" + ev.window().data2());
    } else if (t == SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED) {
      logEvent("window_display_scale_changed");
    } else if (t == SDL_EVENT_QUIT) {
      logEvent("quit");
    } else if (t == SDL_EVENT_WINDOW_FOCUS_GAINED) {
      logEvent("window_focus_gained");
    } else if (t == SDL_EVENT_WINDOW_FOCUS_LOST) {
      logEvent("window_focus_lost");
    } else if (t == SDL_EVENT_WINDOW_MOUSE_ENTER) {
      logEvent("window_mouse_enter");
    } else if (t == SDL_EVENT_WINDOW_RESIZED) {
      resized = true;
      logEvent("window_resized " + ev.window().data1() + "x" + ev.window().data2());
    }
  }

  static String cps(String t) {
    StringBuilder b = new StringBuilder();
    t.codePoints().forEach(cp -> b.append(Integer.toHexString(cp)).append(' '));
    return "[" + b.toString().trim() + "]";
  }

  static void waitGo(int k) throws InterruptedException {
    Path go = out.resolve("go." + k);
    int guard = 0;
    while (!Files.exists(go) && guard++ < 1500) pump(40);
    if (!Files.exists(go)) say("WARN waitGo(" + k + ") timed out");
  }

  static SDL_Surface surf() {
    SDL_Surface s = SDL_GetWindowSurface(window);
    if (s == null) throw new IllegalStateException("SDL_GetWindowSurface: " + SDL_GetError());
    return s;
  }

  /** Copy rows [y0, y0+h) of the image into the window surface (little-endian XRGB/ARGB8888). */
  static void copyRows(BufferedImage img, SDL_Surface s, int y0, int h) {
    int[] data = ((DataBufferInt) img.getRaster().getDataBuffer()).getData();
    ByteBuffer px = s.pixels();
    px.order(ByteOrder.LITTLE_ENDIAN);
    int w = img.getWidth();
    int pitch = s.pitch();
    if (pitch == w * 4) {
      IntBuffer ib = px.asIntBuffer();
      ib.position(y0 * w);
      ib.put(data, y0 * w, h * w);
    } else {
      IntBuffer ib = px.asIntBuffer();
      for (int y = y0; y < y0 + h; y++) {
        ib.position(y * (pitch / 4));
        ib.put(data, y * w, w);
      }
    }
  }

  static void fill(BufferedImage img, int x, int y, int w, int h, int rgb) {
    int[] data = ((DataBufferInt) img.getRaster().getDataBuffer()).getData();
    int iw = img.getWidth();
    for (int yy = y; yy < y + h; yy++) Arrays.fill(data, yy * iw + x, yy * iw + x + w, rgb);
  }

  static void updateRect(int x, int y, int w, int h) {
    try (MemoryStack st = MemoryStack.stackPush()) {
      SDL_Rect.Buffer r = SDL_Rect.calloc(1, st);
      r.get(0).set(x, y, w, h);
      if (!SDL_UpdateWindowSurfaceRects(window, r)) say("WARN UpdateWindowSurfaceRects: " + SDL_GetError());
    }
  }

  static void updateFull() {
    if (!SDL_UpdateWindowSurface(window)) say("WARN UpdateWindowSurface: " + SDL_GetError());
  }

  static void drawTextFrame(BufferedImage img, double scale, int seed) {
    Graphics2D g = img.createGraphics();
    try {
      g.setColor(new Color(0x1e1e24));
      g.fillRect(0, 0, img.getWidth(), img.getHeight());
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
      g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
      g.setColor(new Color(0xdddddd));
      Font f = new Font(Font.MONOSPACED, Font.PLAIN, (int) Math.round(14 * scale));
      g.setFont(f);
      int lh = g.getFontMetrics().getHeight();
      for (int row = 0, y = lh; y < img.getHeight(); row++, y += lh)
        g.drawString("line " + (row + seed) + " the quick brown fox jumps over the lazy dog 0123456789 {}[]()", 8, y);
    } finally {
      g.dispose();
    }
  }

  static double pct(long[] sorted, double p) {
    return sorted[(int) Math.min(sorted.length - 1, Math.floor(p * sorted.length))] / 1e6;
  }

  static void timing(String label, BufferedImage img, double scale) {
    int warm = 30, n = 200;
    long[] copy = new long[n], upd = new long[n], updCpu = new long[n];
    java.lang.management.ThreadMXBean tmx = java.lang.management.ManagementFactory.getThreadMXBean();
    int lh = (int) Math.round(14 * scale * 1.3);
    int dirtyY = img.getHeight() / 2, dirtyH = Math.min(2 * lh, img.getHeight() - dirtyY);
    for (int i = 0; i < warm + n; i++) {
      SDL_Surface s = surf();
      boolean typing = label.startsWith("typing");
      drawTextFrame(img, scale, i); // render cost excluded from the timing below
      long a = System.nanoTime();
      if (typing) copyRows(img, s, dirtyY, dirtyH); else copyRows(img, s, 0, img.getHeight());
      long b = System.nanoTime();
      long cpu0 = tmx.getCurrentThreadCpuTime();
      if (typing) updateRect(0, dirtyY, img.getWidth(), dirtyH); else updateFull();
      long c = System.nanoTime();
      long cpu1 = tmx.getCurrentThreadCpuTime();
      if (i >= warm) { copy[i - warm] = b - a; upd[i - warm] = c - b; updCpu[i - warm] = cpu1 - cpu0; }
      while (SDL_PollEvent(ev)) handle();
    }
    Arrays.sort(copy); Arrays.sort(upd); Arrays.sort(updCpu);
    say(String.format(Locale.ROOT,
        "TIMING %s %dx%d copy_p50=%.3fms copy_p95=%.3fms update_wall_p50=%.3fms update_wall_p95=%.3fms update_cpu_p50=%.3fms update_cpu_p95=%.3fms",
        label, img.getWidth(), img.getHeight(), pct(copy, .5), pct(copy, .95), pct(upd, .5), pct(upd, .95),
        pct(updCpu, .5), pct(updCpu, .95)));
  }

  static void skiaDirectTiming(java.lang.reflect.Method frame, double scale) throws Exception {
    int warm = 30, n = 200;
    long[] draw = new long[n], updCpu = new long[n], upd = new long[n];
    java.lang.management.ThreadMXBean tmx = java.lang.management.ManagementFactory.getThreadMXBean();
    for (int i = 0; i < warm + n; i++) {
      SDL_Surface s = surf();
      long a = System.nanoTime();
      frame.invoke(null, org.lwjgl.system.MemoryUtil.memAddress(s.pixels()), s.w(), s.h(), s.pitch(), i, scale, false, 0);
      long b = System.nanoTime(); long cpu0 = tmx.getCurrentThreadCpuTime();
      updateFull();
      long c = System.nanoTime(); long cpu1 = tmx.getCurrentThreadCpuTime();
      if (i >= warm) { draw[i - warm] = b - a; upd[i - warm] = c - b; updCpu[i - warm] = cpu1 - cpu0; }
      while (SDL_PollEvent(ev)) handle();
    }
    Arrays.sort(draw); Arrays.sort(upd); Arrays.sort(updCpu);
    say(String.format(Locale.ROOT, "TIMING skia-direct-full %dx%d skia_draw_into_sdl_p50=%.3fms p95=%.3fms update_wall_p50=%.3fms update_cpu_p50=%.3fms",
        surf().w(), surf().h(), pct(draw, .5), pct(draw, .95), pct(upd, .5), pct(updCpu, .5)));
  }

  static void coexist(BufferedImage img) {
    say("COEXIST headless=" + GraphicsEnvironment.isHeadless() + " toolkit=" + Toolkit.getDefaultToolkit().getClass().getName());
    Font f = new Font(Font.MONOSPACED, Font.PLAIN, 16);
    FontMetrics fm = img.createGraphics().getFontMetrics(f);
    say("COEXIST java2d font=" + f.getFontName() + " advance(Hello)=" + fm.stringWidth("Hello"));
    try {
      Class<?> c = Class.forName("SkiaCoexist");
      Object r = c.getMethod("run").invoke(null);
      say("COEXIST skia " + r);
    } catch (ClassNotFoundException e) {
      say("COEXIST skia not-on-classpath");
    } catch (Throwable t) {
      Throwable r = t.getCause() != null ? t.getCause() : t;
      say("COEXIST skia FAILED " + r);
    }
  }

  public static void main(String[] args) throws Exception {
    out = Paths.get(args[0]);
    Files.createDirectories(out);
    double scaleHint = args.length > 1 ? Double.parseDouble(args[1]) : 1.0;
    say("JAVA " + System.getProperty("java.version") + " " + System.getProperty("java.vendor"));
    SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "wayland");
    if (!SDL_Init(SDL_INIT_VIDEO)) { say("FATAL SDL_Init: " + SDL_GetError()); System.exit(2); }
    say("DRIVER " + SDL_GetCurrentVideoDriver());
    window = SDL_CreateWindow("sdl-spike", 800, 600, SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIGH_PIXEL_DENSITY);
    if (window == 0) { say("FATAL SDL_CreateWindow: " + SDL_GetError()); System.exit(3); }
    SDL_StartTextInput(window);
    try (MemoryStack st = MemoryStack.stackPush()) {
      SDL_Rect.Buffer r = SDL_Rect.calloc(1, st);
      r.get(0).set(100, 100, 50, 20);
      SDL_SetTextInputArea(window, r, 0);
    }
    // A Wayland toplevel is only mapped (and then configured to the output size) once a buffer is committed.
    SDL_ShowWindow(window);
    { SDL_Surface first = surf(); first.pixels().clear(); updateFull(); }
    for (int i = 0; i < 800 && !resized; i++) pump(50);
    pump(800);

    try (MemoryStack st = MemoryStack.stackPush()) {
      java.nio.IntBuffer w = st.mallocInt(1), h = st.mallocInt(1), pw = st.mallocInt(1), ph = st.mallocInt(1);
      SDL_GetWindowSize(window, w, h);
      SDL_GetWindowSizeInPixels(window, pw, ph);
      say(String.format(Locale.ROOT, "SCALE logical=%dx%d pixels=%dx%d pixelDensity=%.4f displayScale=%.4f contentScale=%.4f",
          w.get(0), h.get(0), pw.get(0), ph.get(0), SDL_GetWindowPixelDensity(window), SDL_GetWindowDisplayScale(window),
          SDL_GetDisplayContentScale(SDL_GetPrimaryDisplay())));
    }
    SDL_Surface s0 = surf();
    int pw = s0.w(), ph = s0.h();
    say("SURFACE " + pw + "x" + ph + " pitch=" + s0.pitch() + " format=0x" + Integer.toHexString(s0.format())
        + " pixelsCapacity=" + s0.pixels().capacity());
    double scale = Math.max(1.0, SDL_GetWindowPixelDensity(window));
    BufferedImage img = new BufferedImage(pw, ph, BufferedImage.TYPE_INT_ARGB);

    if ("ime".equals(System.getProperty("spike.mode"))) {
      fill(img, 0, 0, pw, ph, 0xFF202030); copyRows(img, surf(), 0, ph); updateFull();
      say("PHASE ime-ready"); waitGo(99);
      Files.writeString(out.resolve("events.log"), evlog.toString());
      SDL_DestroyWindow(window); SDL_Quit(); say("DONE"); return;
    }
    // ---- Gate 1: present with partial damage ----
    int rw = Math.min(240, pw / 4), rh = Math.min(160, ph / 4);
    Runnable report = () -> {};
    fill(img, 0, 0, pw, ph, 0xFFC81E1E);
    copyRows(img, surf(), 0, ph); updateFull();
    say("PHASE 1 200,30,30 expect=" + (long) pw * ph); waitGo(1);
    fill(img, 100, 100, rw, rh, 0xFF1EC81E);
    copyRows(img, surf(), 100, rh); updateRect(100, 100, rw, rh);
    say("PHASE 2 30,200,30 expect=" + (long) rw * rh); waitGo(2);
    fill(img, 400, 300, rw, rh, 0xFF1E1EC8);
    copyRows(img, surf(), 300, rh); updateRect(400, 300, rw, rh);
    say("PHASE 3 30,30,200 expect=" + (long) rw * rh); waitGo(3);
    fill(img, 0, 0, pw, ph, 0xFFC8C81E);
    copyRows(img, surf(), 0, ph); updateFull();
    say("PHASE 4 200,200,30 expect=" + (long) pw * ph); waitGo(4);

    // ---- Gate 6b: Skia draws straight into SDL's surface memory (what a BufferedImage cannot do, see comment 12) ----
    java.lang.reflect.Method skiaFrame = null;
    try {
      skiaFrame = Class.forName("SkiaCoexist").getMethod("frame", long.class, int.class, int.class, int.class, int.class, double.class, boolean.class, int.class);
      SDL_Surface sd = surf();
      skiaFrame.invoke(null, org.lwjgl.system.MemoryUtil.memAddress(sd.pixels()), sd.w(), sd.h(), sd.pitch(), 0, scale, true, 0xFF1EC8C8);
      updateFull();
      say("PHASE 4b 30,200,200 expect=" + (long) pw * ph); waitGo(41);
    } catch (Throwable t) { say("SKIA-DIRECT FAILED " + (t.getCause() != null ? t.getCause() : t)); skiaFrame = null; }

    // ---- Gate 2: cost ----
    SDL_SetWindowSurfaceVSync(window, SDL_WINDOW_SURFACE_VSYNC_DISABLED);
    timing("full-novsync", img, scale);
    timing("typing-novsync", img, scale);
    SDL_SetWindowSurfaceVSync(window, 1);
    timing("full-vsync", img, scale);
    timing("typing-vsync", img, scale);
    SDL_SetWindowSurfaceVSync(window, SDL_WINDOW_SURFACE_VSYNC_DISABLED);

    // ---- Gate 6: coexistence ----
    coexist(img);
    if (skiaFrame != null) skiaDirectTiming(skiaFrame, scale);

    // ---- Gate 4: input window (script drives wtype / swaymsg) ----
    say("PHASE input-ready"); waitGo(5);
    // cursor shapes need a pointer-enter serial: the script has moved the pointer over the window
    long text = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_TEXT);
    long hand = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_POINTER);
    long ew = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_EW_RESIZE);
    SDL_SetCursor(text); pump(300); SDL_SetCursor(hand); pump(300); SDL_SetCursor(ew); pump(300);
    say("CURSOR set text,pointer,ew-resize");
    say("PHASE input-done"); waitGo(6);

    // ---- Gate 5: clipboard ----
    say("PHASE clip-ready"); waitGo(7);
    pump(300);
    say("CLIP get clipboard=" + (SDL_HasClipboardText() ? "'" + SDL_GetClipboardText() + "'" : "none")
        + " primary=" + (SDL_HasPrimarySelectionText() ? "'" + SDL_GetPrimarySelectionText() + "'" : "none"));
    boolean c = SDL_SetClipboardText("from-sdl-clipboard");
    boolean p = SDL_SetPrimarySelectionText("from-sdl-primary");
    say("CLIP set clipboard=" + c + " primary=" + p + (c && p ? "" : " err=" + SDL_GetError()));
    pump(500);
    say("PHASE clip-set"); waitGo(8);
    Files.writeString(out.resolve("events.log"), evlog.toString());
    SDL_DestroyWindow(window);
    SDL_Quit();
    say("DONE");
  }
}
