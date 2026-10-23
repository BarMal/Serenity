// Throwaway (#2380, Java 25 research): copy-out and direct-paint experiments against Skiko 0.153.0.
// Run: java [--enable-preview --source 21] [--enable-native-access=ALL-UNNAMED] -cp skiko-awt.jar:runtime.jar FfmBench.java
import java.awt.Graphics2D;
import java.awt.image.*;
import java.lang.foreign.*;
import java.lang.management.ManagementFactory;
import java.nio.*;
import java.util.*;
import org.jetbrains.skia.*;
import org.jetbrains.skia.impl.BufferUtil;

public class FfmBench {
  static final int W = 3840, H = 2160;
  static final com.sun.management.ThreadMXBean TM = (com.sun.management.ThreadMXBean) ManagementFactory.getThreadMXBean();

  interface Copy { void run(int left, int top, int w, int h); }

  static String stat(String name, Copy c, int left, int top, int w, int h) {
    for (int i = 0; i < 300; i++) c.run(left, top, w, h);
    int n = 200;
    long[] ns = new long[n];
    long a0 = TM.getCurrentThreadAllocatedBytes();
    for (int i = 0; i < n; i++) { long t = System.nanoTime(); c.run(left, top, w, h); ns[i] = System.nanoTime() - t; }
    long alloc = (TM.getCurrentThreadAllocatedBytes() - a0) / n;
    Arrays.sort(ns);
    return String.format("  %-34s p50 %8.3f ms  p95 %8.3f ms  alloc %10d B/iter", name, ns[n / 2] / 1e6, ns[(int) (n * 0.95)] / 1e6, alloc);
  }

  public static void main(String[] args) throws Exception {
    System.out.println(System.getProperty("java.vm.name") + " " + System.getProperty("java.runtime.version"));
    ImageInfo info = ImageInfo.Companion.makeN32Premul(W, H).withColorType(ColorType.BGRA_8888);
    Surface surface = Surface.Companion.makeRaster(info);
    Paint paint = new Paint();
    for (int i = 0; i < 400; i++) { paint.setColor(0xff000000 | (i * 2654435761L > 0 ? (int) (i * 2654435761L) : i)); surface.getCanvas().drawRect(Rect.Companion.makeXYWH((i * 37) % W, (i * 91) % H, 600, 300), paint); }
    Pixmap pm = new Pixmap();
    if (!surface.peekPixels(pm)) throw new IllegalStateException("peekPixels");
    final long addr = pm.getAddr();
    final int rowBytes = pm.getRowBytes();
    final Data data = pm.getBuffer();
    final int[] target = new int[W * H];

    final int rowsPerReadFull = Math.max(1, (256 * 1024) / (W * 4));
    Copy repoChunks = (l, t, w, h) -> {
      int rows = (w * 2 >= W) ? rowsPerReadFull : 1;
      int ints = rowBytes / 4;
      for (int first = 0; first < h; first += rows) {
        int count = Math.min(rows, h - first);
        byte[] raw = data.getBytes((t + first) * rowBytes + l * 4, (count - 1) * rowBytes + w * 4);
        IntBuffer read = ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer();
        for (int row = 0; row < count; row++) read.get(row * ints, target, (t + first + row) * W + l, w);
      }
    };
    final IntBuffer view = BufferUtil.INSTANCE.getByteBufferFromPointer(addr, rowBytes * H).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer();
    Copy directBuffer = (l, t, w, h) -> {
      for (int row = 0; row < h; row++) view.get((t + row) * (rowBytes / 4) + l, target, (t + row) * W + l, w);
    };
    final MemorySegment seg = MemorySegment.ofAddress(addr).reinterpret((long) rowBytes * H);
    Copy ffm = (l, t, w, h) -> {
      for (int row = 0; row < h; row++)
        MemorySegment.copy(seg, ValueLayout.JAVA_INT_UNALIGNED, (long) (t + row) * rowBytes + l * 4L, target, (t + row) * W + l, w);
    };
    int[][] areas = { {0, 0, W, H}, {0, 160, W, H - 160}, {0, 1000, W, 2}, {1200, 100, 200, 2000} };
    String[] labels = { "whole frame 3840x2160", "scroll area (all but 160 rows)", "typing, 2 full rows", "narrow 200x2000" };
    for (int i = 0; i < areas.length; i++) {
      int[] a = areas[i];
      System.out.println(labels[i]);
      System.out.println(stat("A repo today (Pixmap.buffer chunks)", repoChunks, a[0], a[1], a[2], a[3]));
      System.out.println(stat("B BufferUtil direct ByteBuffer (21+)", directBuffer, a[0], a[1], a[2], a[3]));
      System.out.println(stat("C FFM MemorySegment.copy (22+)", ffm, a[0], a[1], a[2], a[3]));
    }
    // correctness: all three give identical pixels
    int[] ref = new int[W * H]; repoChunks.run(0, 0, W, H); System.arraycopy(target, 0, ref, 0, ref.length);
    Arrays.fill(target, 0); directBuffer.run(0, 0, W, H); System.out.println("B == A: " + Arrays.equals(ref, target));
    Arrays.fill(target, 0); ffm.run(0, 0, W, H); System.out.println("C == A: " + Arrays.equals(ref, target));

    // Direct paint over native memory
    try (Arena arena = Arena.ofConfined()) {
      MemorySegment nat = arena.allocate((long) W * H * 4, 64);
      Surface direct = Surface.Companion.makeRasterDirect(info, nat.address(), W * 4);
      direct.getCanvas().clear(0xff000000);
      paint.setColor(0xffff0000);
      direct.getCanvas().drawRect(Rect.Companion.makeXYWH(10, 10, 100, 100), paint);
      System.out.printf("makeRasterDirect over native segment: pixel(20,20)=%08x pixel(500,500)=%08x%n",
          nat.get(ValueLayout.JAVA_INT_UNALIGNED, (20L * W + 20) * 4), nat.get(ValueLayout.JAVA_INT_UNALIGNED, (500L * W + 500) * 4));
      direct.close();
    }
    // Heap array: no native address
    MemorySegment heap = MemorySegment.ofArray(new int[16]);
    System.out.println("heap segment of int[]: isNative=" + heap.isNative() + " address()=" + heap.address() + " heapBase present=" + heap.heapBase().isPresent());
    BufferedImage plain = new BufferedImage(W, H, BufferedImage.TYPE_INT_ARGB_PRE);
    System.out.println("BufferedImage raster data buffer: " + plain.getRaster().getDataBuffer().getClass().getName());

    // Image whose pixels live off-heap: custom DataBuffer over the native segment, drawn into a normal image
    final MemorySegment imgSeg = Arena.global().allocate((long) 1920 * 1080 * 4, 64);
    DataBuffer nativeBuffer = new DataBuffer(DataBuffer.TYPE_INT, 1920 * 1080) {
      public int getElem(int bank, int i) { return imgSeg.getAtIndex(ValueLayout.JAVA_INT, i); }
      public void setElem(int bank, int i, int v) { imgSeg.setAtIndex(ValueLayout.JAVA_INT, i, v); }
    };
    SinglePixelPackedSampleModel sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT, 1920, 1080, 1920, new int[] {0xff0000, 0xff00, 0xff, 0xff000000});
    WritableRaster raster = Raster.createWritableRaster(sm, nativeBuffer, null);
    BufferedImage offHeap = new BufferedImage(new DirectColorModel(ColorSpace_sRGB(), 32, 0xff0000, 0xff00, 0xff, 0xff000000, true, DataBuffer.TYPE_INT), raster, true, null);
    BufferedImage onHeap = new BufferedImage(1920, 1080, BufferedImage.TYPE_INT_ARGB_PRE);
    BufferedImage dest = new BufferedImage(1920, 1080, BufferedImage.TYPE_INT_ARGB_PRE);
    System.out.println("raster classes: on-heap " + onHeap.getRaster().getClass().getSimpleName() + ", off-heap " + raster.getClass().getSimpleName());
    Graphics2D g = dest.createGraphics();
    Copy drawOn = (l, t, w, h) -> g.drawImage(onHeap, 0, 0, null);
    Copy drawOff = (l, t, w, h) -> g.drawImage(offHeap, 0, 0, null);
    System.out.println("drawImage 1920x1080 into a BufferedImage");
    System.out.println(stat("int[]-backed BufferedImage", drawOn, 0, 0, 0, 0));
    for (int i = 0; i < 3; i++) drawOff.run(0, 0, 0, 0);
    System.out.println(stat("MemorySegment-backed (custom DataBuffer)", drawOff, 0, 0, 0, 0));
    g.dispose();
  }

  static java.awt.color.ColorSpace ColorSpace_sRGB() { return java.awt.color.ColorSpace.getInstance(java.awt.color.ColorSpace.CS_sRGB); }
}
