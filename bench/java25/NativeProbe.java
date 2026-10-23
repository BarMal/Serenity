// Throwaway (#2380): does each library's native load get flagged by Java 25 without --enable-native-access? The JDK warns
// once per module, so each library is probed in its own JVM.
public class NativeProbe {
  public static void main(String[] args) throws Exception {
    System.out.println("java " + System.getProperty("java.runtime.version") + " probing " + args[0]);
    if (args[0].equals("jna")) System.out.println("jna size of long: " + com.sun.jna.Native.getNativeSize(long.class));
    else { org.jline.nativ.JLineNativeLoader.initialize(); System.out.println("jline native loaded"); }
  }
}
