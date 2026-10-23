// Throwaway (#2380): do JNA and JLine's JNI provider load natives in a way Java 25 flags without --enable-native-access?
public class NativeProbe {
  public static void main(String[] args) throws Exception {
    System.out.println("java " + System.getProperty("java.runtime.version"));
    System.out.println("jna size of long: " + com.sun.jna.Native.getNativeSize(long.class));
    org.jline.nativ.JLineNativeLoader.initialize();
    System.out.println("jline native loaded");
  }
}
