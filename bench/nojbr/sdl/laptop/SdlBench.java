import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.io.PrintWriter;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.nio.file.*;
import java.util.*;

import org.lwjgl.sdl.*;
import org.lwjgl.system.MemoryStack;

import static org.lwjgl.sdl.SDLError.*;
import static org.lwjgl.sdl.SDLEvents.*;
import static org.lwjgl.sdl.SDLHints.*;
import static org.lwjgl.sdl.SDLInit.*;
import static org.lwjgl.sdl.SDLVideo.*;

/**
 * Throwaway benchmark for #2380 (never merge): present cost and frame pacing of SDL3's Wayland software surface on a
 * real compositor. Usage: SdlBench OUTDIR native|one [FRAMES_PER_ARM [ONLY_ARM]], ONLY_ARM being e.g. vsync-on/typing
 *
 * "native" opens the window with SDL_WINDOW_HIGH_PIXEL_DENSITY (buffer = logical size x display scale).
 * "one" omits it: SDL then gives a buffer of the logical size and the compositor scales it up (a scale-1 buffer).
 *
 * Render cost is excluded on purpose: two text frames are drawn once and alternated, so the loop measures only the
 * copy into SDL's surface and SDL_UpdateWindowSurface(Rects).
 */
public final class SdlBench {
  static long window;
  static final SDL_Event ev = SDL_Event.malloc();
  static int resizedEvents, focusLost, otherEvents;
  static PrintWriter report;

  static void say(String s) {
    System.out.println(s);
    System.out.flush();
    report.println(s);
    report.flush();
  }

  static void drain() {
    while (SDL_PollEvent(ev)) {
      int t = ev.type();
      if (t == SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED || t == SDL_EVENT_WINDOW_RESIZED) resizedEvents++;
      else if (t == SDL_EVENT_WINDOW_FOCUS_LOST) focusLost++;
      else otherEvents++;
    }
  }

  static void pump(int ms) {
    long end = System.nanoTime() + ms * 1_000_000L;
    do {
      SDL_WaitEventTimeout(ev, 10);
      drain();
    } while (System.nanoTime() < end);
  }

  static SDL_Surface surf() {
    SDL_Surface s = SDL_GetWindowSurface(window);
    if (s == null) throw new IllegalStateException("SDL_GetWindowSurface: " + SDL_GetError());
    return s;
  }

  static void copyRows(BufferedImage img, SDL_Surface s, int y0, int h) {
    int[] data = ((DataBufferInt) img.getRaster().getDataBuffer()).getData();
    ByteBuffer px = s.pixels();
    px.order(ByteOrder.LITTLE_ENDIAN);
    int w = img.getWidth(), pitch = s.pitch();
    IntBuffer ib = px.asIntBuffer();
    if (pitch == w * 4) {
      ib.position(y0 * w);
      ib.put(data, y0 * w, h * w);
    } else {
      for (int y = y0; y < y0 + h; y++) {
        ib.position(y * (pitch / 4));
        ib.put(data, y * w, w);
      }
    }
  }

  static void drawTextFrame(BufferedImage img, double scale, int seed) {
    Graphics2D g = img.createGraphics();
    try {
      g.setColor(new Color(0x1e1e24));
      g.fillRect(0, 0, img.getWidth(), img.getHeight());
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
      g.setColor(new Color(0xdddddd));
      g.setFont(new Font(Font.MONOSPACED, Font.PLAIN, (int) Math.round(14 * scale)));
      int lh = g.getFontMetrics().getHeight();
      for (int row = 0, y = lh; y < img.getHeight(); row++, y += lh)
        g.drawString("line " + (row + seed) + " the quick brown fox jumps over the lazy dog 0123456789 {}[]()", 8, y);
    } finally {
      g.dispose();
    }
  }

  static double ms(long[] sorted, double p) {
    return sorted[(int) Math.min(sorted.length - 1, Math.floor(p * sorted.length))] / 1e6;
  }

  static long[] sorted(long[] a) {
    long[] c = a.clone();
    Arrays.sort(c);
    return c;
  }

  static void arm(Path out, BufferedImage[] imgs, double scale, boolean typing, boolean vsync, int frames, double refreshHz) throws Exception {
    final int warm = 60;
    SDL_SetWindowSurfaceVSync(window, vsync ? 1 : SDL_WINDOW_SURFACE_VSYNC_DISABLED);
    int w = imgs[0].getWidth(), h = imgs[0].getHeight();
    int lh = (int) Math.round(14 * scale * 1.3);
    int dirtyY = h / 2, dirtyH = Math.min(2 * lh, h - dirtyY);
    long[] copy = new long[frames], upd = new long[frames], cpu = new long[frames], iv = new long[frames], at = new long[frames];
    java.lang.management.ThreadMXBean tmx = java.lang.management.ManagementFactory.getThreadMXBean();
    int focusBefore = focusLost, resizeBefore = resizedEvents;
    long prevEnd = 0, runStart = 0;
    for (int i = 0; i < warm + frames; i++) {
      SDL_Surface s = surf();
      if (s.w() != w || s.h() != h) throw new IllegalStateException("surface changed size during the run: " + s.w() + "x" + s.h());
      BufferedImage img = imgs[i & 1];
      long a = System.nanoTime();
      if (typing) copyRows(img, s, dirtyY, dirtyH); else copyRows(img, s, 0, h);
      long b = System.nanoTime();
      long c0 = tmx.getCurrentThreadCpuTime();
      if (typing) {
        try (MemoryStack st = MemoryStack.stackPush()) {
          SDL_Rect.Buffer r = SDL_Rect.calloc(1, st);
          r.get(0).set(0, dirtyY, w, dirtyH);
          SDL_UpdateWindowSurfaceRects(window, r);
        }
      } else {
        SDL_UpdateWindowSurface(window);
      }
      long c = System.nanoTime();
      long c1 = tmx.getCurrentThreadCpuTime();
      if (i == warm) runStart = c;
      if (i >= warm) {
        int k = i - warm;
        copy[k] = b - a; upd[k] = c - b; cpu[k] = c1 - c0; iv[k] = c - prevEnd; at[k] = c - runStart;
      }
      prevEnd = c;
      drain();
    }
    long total = prevEnd - runStart;
    long[] sIv = sorted(iv), sUpd = sorted(upd), sCpu = sorted(cpu), sCopy = sorted(copy);
    double period = 1e9 / refreshHz;
    int missed = 0;
    for (long v : iv) if (v > 1.5 * period) missed++;
    String label = (vsync ? "vsync-on" : "vsync-off") + "/" + (typing ? "typing" : "full");
    say(String.format(Locale.ROOT,
        "BENCH %s %dx%d frames=%d fps=%.1f interval_p50=%.2fms p95=%.2fms p99=%.2fms max=%.2fms missed_gt1.5x_period=%d copy_p50=%.3fms copy_p95=%.3fms update_wall_p50=%.3fms p95=%.3fms update_cpu_p50=%.3fms p95=%.3fms focus_lost=%d resize_events=%d",
        label, w, h, frames, frames / (total / 1e9), ms(sIv, .5), ms(sIv, .95), ms(sIv, .99), sIv[frames - 1] / 1e6, missed,
        ms(sCopy, .5), ms(sCopy, .95), ms(sUpd, .5), ms(sUpd, .95), ms(sCpu, .5), ms(sCpu, .95),
        focusLost - focusBefore, resizedEvents - resizeBefore));
    try (PrintWriter csv = new PrintWriter(Files.newBufferedWriter(out.resolve("frames-" + label.replace('/', '-') + ".csv")))) {
      csv.println("t_ns,copy_ns,update_wall_ns,update_cpu_ns,interval_ns");
      for (int k = 0; k < frames; k++) csv.println(at[k] + "," + copy[k] + "," + upd[k] + "," + cpu[k] + "," + iv[k]);
    }
  }

  public static void main(String[] args) throws Exception {
    Path out = Paths.get(args[0]);
    Files.createDirectories(out);
    boolean nativeDensity = !"one".equals(args[1]);
    int frames = args.length > 2 ? Integer.parseInt(args[2]) : 600;
    report = new PrintWriter(Files.newBufferedWriter(out.resolve("bench.txt")));
    say("META java=" + System.getProperty("java.version") + " " + System.getProperty("java.vendor") + " density=" + (nativeDensity ? "native" : "one"));
    SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "wayland");
    if (!SDL_Init(SDL_INIT_VIDEO)) { say("FATAL SDL_Init: " + SDL_GetError()); System.exit(2); }
    say("META driver=" + SDL_GetCurrentVideoDriver());
    window = SDL_CreateWindow("sdl-bench", 1280, 800, SDL_WINDOW_RESIZABLE | (nativeDensity ? SDL_WINDOW_HIGH_PIXEL_DENSITY : 0));
    if (window == 0) { say("FATAL SDL_CreateWindow: " + SDL_GetError()); System.exit(3); }
    SDL_ShowWindow(window);
    { SDL_Surface first = surf(); first.pixels().clear(); SDL_UpdateWindowSurface(window); }
    // A tiling compositor sends the real size after the first buffer; let it settle before measuring.
    for (int i = 0; i < 60 && resizedEvents == 0; i++) pump(50);
    pump(1500);

    double refresh = 60.0;
    try (MemoryStack st = MemoryStack.stackPush()) {
      java.nio.IntBuffer lw = st.mallocInt(1), lh = st.mallocInt(1), pw = st.mallocInt(1), ph = st.mallocInt(1);
      SDL_GetWindowSize(window, lw, lh);
      SDL_GetWindowSizeInPixels(window, pw, ph);
      SDL_DisplayMode mode = SDL_GetCurrentDisplayMode(SDL_GetDisplayForWindow(window));
      if (mode != null && mode.refresh_rate() > 1) refresh = mode.refresh_rate();
      say(String.format(Locale.ROOT, "META logical=%dx%d pixels=%dx%d pixelDensity=%.4f displayScale=%.4f refresh=%.3fHz",
          lw.get(0), lh.get(0), pw.get(0), ph.get(0), SDL_GetWindowPixelDensity(window), SDL_GetWindowDisplayScale(window), refresh));
    }
    SDL_Surface s0 = surf();
    say("META surface=" + s0.w() + "x" + s0.h() + " pitch=" + s0.pitch() + " format=0x" + Integer.toHexString(s0.format()));
    double scale = Math.max(1.0, SDL_GetWindowPixelDensity(window));
    BufferedImage[] imgs = { new BufferedImage(s0.w(), s0.h(), BufferedImage.TYPE_INT_RGB), new BufferedImage(s0.w(), s0.h(), BufferedImage.TYPE_INT_RGB) };
    drawTextFrame(imgs[0], scale, 0);
    drawTextFrame(imgs[1], scale, 1);

    String only = args.length > 3 ? args[3] : "";
    for (boolean vsync : new boolean[] { true, false })
      for (boolean typing : new boolean[] { false, true }) {
        String name = (vsync ? "vsync-on" : "vsync-off") + "/" + (typing ? "typing" : "full");
        if (!only.isEmpty() && !only.equals(name)) continue;
        pump(300);
        arm(out, imgs, scale, typing, vsync, frames, refresh);
      }
    say("DONE");
    SDL_DestroyWindow(window);
    SDL_Quit();
    report.close();
  }
}
