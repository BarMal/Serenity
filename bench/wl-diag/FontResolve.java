import java.awt.*;
import java.awt.font.*;
import java.io.File;
import java.util.*;

/** How the runtime resolves logical fonts, and what glyph codes layoutGlyphVector hands back for them. */
public class FontResolve {
  static final String TEXT = "The quick brown fox 0123456789 fi AVATAR";

  public static void main(String[] args) throws Exception {
    System.out.println("RUNTIME " + System.getProperty("java.vendor.version", System.getProperty("java.vm.vendor") + " " + System.getProperty("java.version")));
    var home = new File(System.getProperty("java.home"));
    var libFonts = new File(home, "lib/fonts");
    System.out.println("lib/fonts: " + (libFonts.isDirectory() ? Arrays.toString(libFonts.list()) : "absent"));
    System.out.println("fontconfig props: " + Arrays.toString(new File(home, "lib").list((d, n) -> n.contains("fontconfig"))));
    var frc = new FontRenderContext(null, true, true);
    for (String name : new String[]{"Dialog", "Monospaced", "SansSerif"}) {
      for (boolean liga : new boolean[]{false, true}) {
        Font f = new Font(name, Font.PLAIN, 18);
        if (liga) f = f.deriveFont(Map.of(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON));
        var gv = f.layoutGlyphVector(frc, TEXT.toCharArray(), 0, TEXT.length(), Font.LAYOUT_LEFT_TO_RIGHT);
        int n = gv.getNumGlyphs();
        int[] codes = gv.getGlyphCodes(0, n, new int[n]);
        var slots = new TreeMap<Integer, Integer>();
        for (int c : codes) slots.merge(c >>> 24, 1, Integer::sum);
        var rebuilt = f.createGlyphVector(frc, codes);
        int[] back = rebuilt.getGlyphCodes(0, n, new int[n]);
        int stable = 0;
        for (int i = 0; i < n; i++) if (back[i] == codes[i]) stable++;
        var chars = new StringBuilder();
        var cp = rebuilt.getGlyphCharIndex(0);
        for (int i = 0; i < n; i++) chars.append(rebuilt.getGlyphCharIndex(i)).append(i + 1 < n ? "," : "");
        System.out.printf("%-10s liga=%-5s physical=%s glyphs=%d slotHistogram=%s codesStableOnRebuild=%d/%d%n  firstCodes=%s%n  rebuiltCharIdx=%s%n",
          name, liga, f.getFontName(), n, slots, stable, n, hex(codes, 12), chars);
        var sample = new Font(name, Font.PLAIN, 18);
        System.out.println("  canDisplay(T,h,e)=" + sample.canDisplay('T') + sample.canDisplay('h') + sample.canDisplay('e')
          + " family=" + sample.getFamily() + " psname=" + sample.getPSName());
      }
    }
  }

  static String hex(int[] a, int n) {
    var sb = new StringBuilder();
    for (int i = 0; i < Math.min(n, a.length); i++) sb.append(String.format("%08x ", a[i]));
    return sb.toString();
  }
}
