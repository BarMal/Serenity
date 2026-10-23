import org.jetbrains.skia.*;

/** Throwaway: Skiko natives load and draw in the same JVM as SDL, and the pixels land in a Java int[] (Serenity's copy-out shape). */
public final class SkiaCoexist {
  public static String run() {
    org.jetbrains.skiko.Library.INSTANCE.load();
    Surface s = Surface.Companion.makeRasterN32Premul(64, 48);
    s.getCanvas().clear(0xFFC81E1E);
    Paint p = new Paint();
    p.setColor(0xFF1EC81E);
    s.getCanvas().drawRect(Rect.Companion.makeXYWH(8, 8, 16, 16), p);
    Bitmap bmp = new Bitmap();
    bmp.allocN32Pixels(64, 48, false);
    boolean ok = s.readPixels(bmp, 0, 0);
    byte[] raw = bmp.readPixels(bmp.getImageInfo(), 64 * 4, 0, 0);
    java.nio.IntBuffer ib = java.nio.ByteBuffer.wrap(raw).order(java.nio.ByteOrder.LITTLE_ENDIAN).asIntBuffer();
    int[] px = new int[64 * 48];
    ib.get(px);
    return String.format("loaded readPixels=%b outside=0x%08X inside=0x%08X", ok, px[0], px[12 * 64 + 12]);
  }

  private static Font font;
  private static Paint ink, bg;

  /** Skia draws straight into the memory SDL gave us (no BufferedImage, no copy-out). */
  public static void frame(long ptr, int w, int h, int rowBytes, int seed, double scale, boolean solid, int argb) {
    org.jetbrains.skiko.Library.INSTANCE.load();
    Surface s = Surface.Companion.makeRasterDirect(ImageInfo.Companion.makeN32Premul(w, h), ptr, rowBytes);
    Canvas c = s.getCanvas();
    if (solid) { c.clear(argb); s.close(); return; }
    if (font == null) {
      font = new Font(Typeface.Companion.makeEmpty(), (float) (14 * scale));
      ink = new Paint(); ink.setColor(0xFFDDDDDD);
    }
    c.clear(0xFF1E1E24);
    float lh = (float) (14 * scale * 1.3);
    for (int row = 0; (row + 1) * lh < h; row++)
      c.drawString("line " + (row + seed) + " the quick brown fox jumps over the lazy dog 0123456789 {}[]()", 8, (row + 1) * lh, font, ink);
    s.close();
  }
}
