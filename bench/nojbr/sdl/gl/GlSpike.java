import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.IntBuffer;
import java.util.Arrays;

import org.jetbrains.skia.*;
import org.lwjgl.sdl.SDL_Event;
import org.lwjgl.system.MemoryStack;

import static org.lwjgl.sdl.SDLError.*;
import static org.lwjgl.sdl.SDLEvents.*;
import static org.lwjgl.sdl.SDLInit.*;
import static org.lwjgl.sdl.SDLVideo.*;

/**
 * Throwaway (#2380, never merge): SDL3 window + EGL/GL context + Skiko DirectContext.makeGL + BackendRenderTarget.makeGL
 * + Surface.makeFromBackendRenderTarget + SDL_GL_SwapWindow. args: OUTDIR PROFILE(gl|es) SWAP_INTERVAL
 */
public final class GlSpike {
  static void say(String s) { System.out.println(s); System.out.flush(); }

  static String glString(int name) throws Throwable {
    long addr = SDL_GL_GetProcAddress("glGetString");
    if (addr == 0) return "<no glGetString>";
    MethodHandle h = Linker.nativeLinker().downcallHandle(MemorySegment.ofAddress(addr),
        FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.JAVA_INT));
    MemorySegment s = (MemorySegment) h.invoke(name);
    return s.equals(MemorySegment.NULL) ? "<null>" : s.reinterpret(256).getString(0);
  }

  static int attr(int a) {
    try (MemoryStack st = MemoryStack.stackPush()) {
      IntBuffer v = st.mallocInt(1);
      return SDL_GL_GetAttribute(a, v) ? v.get(0) : -1;
    }
  }

  static double pct(long[] sorted, double p) { return sorted[(int) Math.min(sorted.length - 1, Math.floor(p * sorted.length))] / 1e6; }

  public static void main(String[] args) throws Throwable {
    boolean es = args[1].equals("es");
    int swap = Integer.parseInt(args[2]);
    say("JAVA " + System.getProperty("java.version"));
    if (!SDL_Init(SDL_INIT_VIDEO)) { say("FAIL SDL_Init " + SDL_GetError()); return; }
    say("DRIVER " + SDL_GetCurrentVideoDriver());
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8); SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8); SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8); SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    if (es) {
      SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
      SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3); SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);
    }
    long window = SDL_CreateWindow("gl-spike", 900, 600, SDL_WINDOW_OPENGL | SDL_WINDOW_HIGH_PIXEL_DENSITY);
    if (window == 0) { say("FAIL SDL_CreateWindow " + SDL_GetError()); return; }
    long ctx = SDL_GL_CreateContext(window);
    if (ctx == 0) { say("FAIL SDL_GL_CreateContext " + SDL_GetError()); return; }
    say("GLCTX created profile=" + (es ? "es3.0" : "default"));
    say("GLCTX vendor=" + glString(0x1F00) + " renderer=" + glString(0x1F01) + " version=" + glString(0x1F02));
    say("GLCTX swap_interval_set=" + SDL_GL_SetSwapInterval(swap) + " requested=" + swap
        + " stencil=" + attr(SDL_GL_STENCIL_SIZE) + " samples=" + attr(SDL_GL_MULTISAMPLESAMPLES));
    int w, h;
    try (MemoryStack st = MemoryStack.stackPush()) {
      IntBuffer iw = st.mallocInt(1), ih = st.mallocInt(1);
      SDL_GetWindowSizeInPixels(window, iw, ih);
      w = iw.get(0); h = ih.get(0);
    }
    say("SIZE " + w + "x" + h);

    if (args.length > 3) {
      // RTLD_NOW|RTLD_GLOBAL so the symbol wins over libGL's when libskiko resolves it
      MethodHandle dl = Linker.nativeLinker().downcallHandle(Linker.nativeLinker().defaultLookup().find("dlopen").get(),
          FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_INT));
      try (Arena a = Arena.ofConfined()) {
        MemorySegment r = (MemorySegment) dl.invoke(a.allocateFrom(args[3]), 0x2 | 0x100);
        say("SHIM dlopen(" + args[3] + ") global=" + !r.equals(MemorySegment.NULL));
      }
    }
    org.jetbrains.skiko.Library.INSTANCE.load();
    DirectContext dc;
    try { dc = DirectContext.Companion.makeGL(); say("SKIA makeGL ok"); }
    catch (Throwable t) { say("FAIL SKIA makeGL " + t); return; }
    BackendRenderTarget rt = BackendRenderTarget.Companion.makeGL(w, h, 0, 8, 0, 0x8058);
    Surface surface = Surface.Companion.makeFromBackendRenderTarget(dc, rt, SurfaceOrigin.BOTTOM_LEFT,
        SurfaceColorFormat.RGBA_8888, null, null);
    if (surface == null) { say("FAIL makeFromBackendRenderTarget returned null"); return; }
    say("SKIA surface " + surface.getWidth() + "x" + surface.getHeight());

    Typeface tf = FontMgr.Companion.getDefault().makeFromFile("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 0);
    Font font = new Font(tf, 14f * 1f);
    Paint ink = new Paint(); ink.setColor(0xFFDDDDDD);
    say("FONT typeface=" + (tf == null ? "null" : tf.getFamilyName()));
    Canvas c = surface.getCanvas();
    int warm = 30, n = 240;
    long[] draw = new long[n], sw = new long[n], total = new long[n];
    SDL_Event ev = SDL_Event.malloc();
    for (int f = 0; f < warm + n; f++) {
      long t0 = System.nanoTime();
      c.clear(0xFF1E1E24);
      float lh = 18f;
      for (int row = 0; (row + 1) * lh < h; row++)
        c.drawString("line " + (row + f) + " the quick brown fox jumps over the lazy dog 0123456789 {}[]()", 8, (row + 1) * lh, font, ink);
      dc.flushAndSubmit(surface, false);
      long t1 = System.nanoTime();
      SDL_GL_SwapWindow(window);
      long t2 = System.nanoTime();
      while (SDL_PollEvent(ev)) { }
      if (f >= warm) { draw[f - warm] = t1 - t0; sw[f - warm] = t2 - t1; total[f - warm] = t2 - t0; }
    }
    for (long[] a : new long[][] { draw, sw, total }) Arrays.sort(a);
    say(String.format("TIMING software-GL llvmpipe %dx%d swap=%d frames=%d  draw+flush p50=%.2f p95=%.2f  swap p50=%.2f p95=%.2f  frame p50=%.2f p95=%.2f ms  (%.1f fps by p50)",
        w, h, swap, n, pct(draw, .5), pct(draw, .95), pct(sw, .5), pct(sw, .95), pct(total, .5), pct(total, .95), 1000.0 / pct(total, .5)));
    // hold a final known frame for the screenshot
    c.clear(0xFF1E1E24);
    for (int row = 0; (row + 1) * 18f < h; row++) c.drawString("HOLD line " + row + " the quick brown fox", 8, (row + 1) * 18f, font, ink);
    dc.flushAndSubmit(surface, false);
    SDL_GL_SwapWindow(window);
    say("PHASE hold");
    long end = System.nanoTime() + 6_000_000_000L;
    while (System.nanoTime() < end) { while (SDL_PollEvent(ev)) { } SDL_GL_SwapWindow(window); try { Thread.sleep(30); } catch (InterruptedException e) { } }
    say("DONE");
  }
}
