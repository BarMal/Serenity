import java.io.IOException;
import java.net.StandardProtocolFamily;
import java.net.UnixDomainSocketAddress;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.*;

/** Prototype of a pre-AWT Wayland probe: globals plus per-output mode and xdg-output logical size, pure JDK. */
public class WlProbe {
  record Global(int name, String iface, int version) {}
  static SocketChannel ch;
  static int nextId = 2;
  static ByteBuffer out = ByteBuffer.allocate(4096).order(ByteOrder.nativeOrder());

  static void msg(int obj, int opcode, ByteBuffer body) {
    body.flip();
    out.putInt(obj).putInt(((8 + body.remaining()) << 16) | opcode).put(body);
  }
  static ByteBuffer body() { return ByteBuffer.allocate(256).order(ByteOrder.nativeOrder()); }
  static void str(ByteBuffer b, String s) {
    byte[] raw = s.getBytes(StandardCharsets.UTF_8);
    b.putInt(raw.length + 1).put(raw).put((byte) 0);
    while (b.position() % 4 != 0) b.put((byte) 0);
  }
  static void flush() throws IOException { out.flip(); while (out.hasRemaining()) ch.write(out); out.clear(); }

  interface Handler { void event(int obj, int opcode, ByteBuffer payload); }

  static void roundtrip(Handler h) throws IOException {
    int cb = nextId++;
    msg(1, 0, body().putInt(cb));
    flush();
    ByteBuffer in = ByteBuffer.allocate(65536).order(ByteOrder.nativeOrder());
    while (true) {
      in.clear().limit(8);
      readFully(in);
      in.flip();
      int obj = in.getInt(); int w = in.getInt();
      int size = w >>> 16, opcode = w & 0xffff;
      ByteBuffer payload = ByteBuffer.allocate(size - 8).order(ByteOrder.nativeOrder());
      readFully(payload); payload.flip();
      if (obj == cb) return;
      h.event(obj, opcode, payload);
    }
  }
  static void readFully(ByteBuffer b) throws IOException { while (b.hasRemaining()) if (ch.read(b) < 0) throw new IOException("eof"); }

  public static void main(String[] a) throws Exception {
    long t0 = System.nanoTime();
    String dir = System.getenv("XDG_RUNTIME_DIR"), disp = System.getenv("WAYLAND_DISPLAY");
    Path path = disp.startsWith("/") ? Path.of(disp) : Path.of(dir, disp);
    ch = SocketChannel.open(StandardProtocolFamily.UNIX);
    ch.connect(UnixDomainSocketAddress.of(path));
    int registry = nextId++;
    msg(1, 1, body().putInt(registry));
    List<Global> globals = new ArrayList<>();
    roundtrip((obj, op, p) -> {
      if (obj == registry && op == 0) {
        int name = p.getInt(); int len = p.getInt();
        byte[] s = new byte[len - 1]; p.get(s); p.position((p.position() + 1 + 3) & ~3);
        globals.add(new Global(name, new String(s, StandardCharsets.UTF_8), p.getInt()));
      }
    });
    long t1 = System.nanoTime();
    Map<Integer, Global> outputs = new LinkedHashMap<>();   // object id -> global
    Map<Integer, Integer> xdgOf = new HashMap<>();          // xdg_output id -> output id
    Global mgr = globals.stream().filter(g -> g.iface.equals("zxdg_output_manager_v1")).findFirst().orElse(null);
    int mgrId = -1;
    if (mgr != null) { mgrId = nextId++; ByteBuffer b = body(); b.putInt(mgr.name); str(b, mgr.iface); b.putInt(Math.min(mgr.version, 3)).putInt(mgrId); msg(registry, 0, b); }
    for (Global g : globals) if (g.iface.equals("wl_output")) {
      int id = nextId++; outputs.put(id, g);
      ByteBuffer b = body(); b.putInt(g.name); str(b, g.iface); b.putInt(Math.min(g.version, 4)).putInt(id); msg(registry, 0, b);
      if (mgrId > 0) { int x = nextId++; xdgOf.put(x, id); msg(mgrId, 1, body().putInt(x).putInt(id)); }
    }
    Map<Integer, int[]> mode = new HashMap<>(), logical = new HashMap<>(), scale = new HashMap<>();
    roundtrip((obj, op, p) -> {
      if (outputs.containsKey(obj)) {
        if (op == 1) { int flags = p.getInt(); int w = p.getInt(), h = p.getInt(); if ((flags & 1) != 0) mode.put(obj, new int[] {w, h}); }
        if (op == 3) scale.put(obj, new int[] {p.getInt()});
      } else if (xdgOf.containsKey(obj) && op == 1) logical.put(xdgOf.get(obj), new int[] {p.getInt(), p.getInt()});
    });
    long t2 = System.nanoTime();
    System.out.printf("globals=%d fractional_scale_manager=%s xdg_output_manager=%s%n", globals.size(),
        globals.stream().anyMatch(g -> g.iface.equals("wp_fractional_scale_manager_v1")), mgr != null);
    for (int id : outputs.keySet()) {
      int[] m = mode.get(id), l = logical.get(id);
      String ratio = (m != null && l != null) ? String.format("%.3f", (double) m[0] / l[0]) : "n/a";
      System.out.printf("output mode=%s logical=%s wl_output.scale=%s effective=%s%n",
          m == null ? "?" : m[0] + "x" + m[1], l == null ? "?" : l[0] + "x" + l[1], scale.containsKey(id) ? scale.get(id)[0] : "?", ratio);
    }
    System.out.printf("probe: connect+registry %.2f ms, outputs %.2f ms, total %.2f ms%n", (t1 - t0) / 1e6, (t2 - t1) / 1e6, (t2 - t0) / 1e6);
  }
}
