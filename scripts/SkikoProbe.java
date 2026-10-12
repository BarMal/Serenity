import org.jetbrains.skia.Surface;
import org.jetbrains.skiko.Library;
import org.jetbrains.skiko.SkikoProperties;

/**
 * Loads the Skiko native library the way the app image does and renders into a 1x1 raster surface, then prints
 * SKIKO_PROBE_OK followed by the render API Skiko would use for a window. It needs no display and no GPU. Run it
 * against a jar stripped by SkikoNatives.java, with -Dskiko.library.path naming the extracted directory, so a pass means
 * the shipped layout loads and not that the jar happened to contain the library:
 *
 *   java -Dskiko.library.path=natives -cp Serenity.jar scripts/SkikoProbe.java
 */
public final class SkikoProbe {
  private SkikoProbe() {}

  public static void main(String[] args) {
    Library.INSTANCE.load();
    try (Surface surface = Surface.Companion.makeRasterN32Premul(1, 1)) {
      surface.getCanvas().clear(0xFF336699);
    }
    System.out.println("SKIKO_PROBE_OK " + SkikoProperties.INSTANCE.getRenderApi());
  }
}
