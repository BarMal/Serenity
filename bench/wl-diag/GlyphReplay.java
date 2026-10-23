import java.awt.*;
import java.awt.font.*;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.List;
import javax.imageio.ImageIO;

/** Mirrors ShapedStretch: layoutGlyphVector -> glyph codes + positions -> createGlyphVector(codes) -> drawGlyphVector,
 *  and compares with drawString on the same graphics (AA on, fractional metrics on, 2x scale like the app). */
public class GlyphReplay {
  static final String TEXT = "Hello, wayland: the quick brown fox 0123456789 fi AVATAR {}[]()";

  static Font load(String path, float size) throws Exception {
    return Font.createFont(Font.TRUETYPE_FONT, new File(path)).deriveFont(size);
  }

  static BufferedImage canvas() { return new BufferedImage(1400, 120, BufferedImage.TYPE_INT_RGB); }

  static Graphics2D g(BufferedImage im, double scale) {
    var g = im.createGraphics();
    g.setColor(Color.BLACK); g.fillRect(0, 0, im.getWidth(), im.getHeight());
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
    g.scale(scale, scale);
    g.setColor(Color.WHITE);
    return g;
  }

  static int diff(BufferedImage a, BufferedImage b) {
    int n = 0;
    for (int y = 0; y < a.getHeight(); y++) for (int x = 0; x < a.getWidth(); x++) if (a.getRGB(x, y) != b.getRGB(x, y)) n++;
    return n;
  }

  static int ink(BufferedImage a) {
    int n = 0;
    for (int y = 0; y < a.getHeight(); y++) for (int x = 0; x < a.getWidth(); x++) if ((a.getRGB(x, y) & 0xffffff) != 0) n++;
    return n;
  }

  public static void main(String[] args) throws Exception {
    String fonts = args[0]; File out = new File(args[1]); out.mkdirs();
    System.out.println("RUNTIME " + System.getProperty("java.vendor.version", System.getProperty("java.vm.vendor") + " " + System.getProperty("java.version")));
    var list = new java.util.ArrayList<Object[]>();
    list.add(new Object[]{"Dialog", new Font("Dialog", Font.PLAIN, 18)});
    list.add(new Object[]{"Monospaced", new Font("Monospaced", Font.PLAIN, 18)});
    list.add(new Object[]{"CourierPrime", load(fonts + "/CourierPrime-Regular.ttf", 18)});
    list.add(new Object[]{"MonaspaceNeon-otf", load(fonts + "/MonaspaceNeon-Regular.otf", 18)});
    list.add(new Object[]{"MonaspaceNeonVar", load(fonts + "/MonaspaceNeonVarVF[wght,wdth,slnt].ttf", 18)});
    for (double scale : new double[]{1.0, 2.0}) {
      for (var e : list) {
        String name = (String) e[0]; Font font = (Font) e[1];
        var imA = canvas(); var gA = g(imA, scale);
        gA.setFont(font); var frc = gA.getFontRenderContext();
        gA.drawString(TEXT, 10f, 40f);
        gA.dispose();
        // replay: shape once, keep only codes + positions, rebuild
        var gv = font.layoutGlyphVector(frc, TEXT.toCharArray(), 0, TEXT.length(), Font.LAYOUT_LEFT_TO_RIGHT);
        int n = gv.getNumGlyphs();
        int[] codes = gv.getGlyphCodes(0, n, new int[n]);
        float[] pos = gv.getGlyphPositions(0, n + 1, new float[2 * (n + 1)]);
        var rebuilt = font.createGlyphVector(frc, codes);
        for (int i = 0; i < n; i++) rebuilt.setGlyphPosition(i, new Point2D.Float(pos[2 * i], pos[2 * i + 1]));
        var imB = canvas(); var gB = g(imB, scale);
        gB.setFont(font); gB.drawGlyphVector(rebuilt, 10f, 40f); gB.dispose();
        // replay into a graphics with a different FRC (cache keyed on frc in the app; here we deliberately reuse codes at the other scale)
        var imC = canvas(); var gC = g(imC, scale == 1.0 ? 2.0 : 1.0);
        gC.setFont(font); gC.drawGlyphVector(rebuilt, 10f, 40f); gC.dispose();
        System.out.printf("scale=%.0f font=%-18s glyphs=%d inkA=%d inkB=%d diff(drawString,replay)=%d%n", scale, name, n, ink(imA), ink(imB), diff(imA, imB));
        ImageIO.write(imA, "png", new File(out, name + "-s" + (int) scale + "-A-drawString.png"));
        ImageIO.write(imB, "png", new File(out, name + "-s" + (int) scale + "-B-replay.png"));
      }
    }
  }
}
