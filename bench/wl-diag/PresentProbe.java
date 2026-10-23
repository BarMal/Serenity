import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.concurrent.atomic.AtomicReference;
import javax.swing.*;

/** Replica of SwingWindow's presentation path: opaque JPanel, double-buffering off up the chain,
 *  base image drawn scaled into the clip, caret filled over it, partial paintImmediately from invokeLater. */
public class PresentProbe {
  static final Color[] PHASES = { new Color(255, 0, 255), new Color(0, 255, 255), new Color(255, 255, 0), new Color(0, 255, 0) };
  static final Rectangle REGION = new Rectangle(120, 120, 240, 160);
  static final AtomicReference<BufferedImage> base = new AtomicReference<>();
  static final AtomicReference<Rectangle> caret = new AtomicReference<>(new Rectangle(130, 130, 2, 20));
  static JPanel canvas;

  static BufferedImage frame(int w, int h, Color c) {
    var img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
    var g = img.createGraphics();
    g.setColor(new Color(20, 20, 20)); g.fillRect(0, 0, w, h);
    g.setColor(c); g.fillRect(REGION.x, REGION.y, REGION.width, REGION.height);
    g.dispose();
    return img;
  }

  static void present(String mode) {
    SwingUtilities.invokeLater(() -> {
      switch (mode) {
        case "rect" -> canvas.paintImmediately(REGION);
        case "whole" -> canvas.paintImmediately(0, 0, canvas.getWidth(), canvas.getHeight());
        default -> canvas.repaint();
      }
    });
  }

  public static void main(String[] a) throws Exception {
    String mode = a.length > 0 ? a[0] : "rect";
    File dir = new File(a.length > 1 ? a[1] : ".");
    System.out.println("TOOLKIT " + Toolkit.getDefaultToolkit().getClass().getName() + " mode=" + mode
        + " java=" + System.getProperty("java.vendor.version", System.getProperty("java.version")));
    SwingUtilities.invokeAndWait(() -> {
      var f = new JFrame("probe");
      canvas = new JPanel() {
        @Override protected void paintComponent(Graphics g) {
          var g2 = (Graphics2D) g.create();
          var clip = g2.getClipBounds();
          var img = base.get();
          if (img != null && clip != null) {
            g2.scale((double) getWidth() / img.getWidth(), (double) getHeight() / img.getHeight());
            g2.drawImage(img, clip.x, clip.y, clip.x + clip.width, clip.y + clip.height,
                clip.x, clip.y, clip.x + clip.width, clip.y + clip.height, null);
            g2.scale(img.getWidth() / (double) getWidth(), img.getHeight() / (double) getHeight());
          }
          var c = caret.get();
          g2.setColor(Color.WHITE); g2.fillRect(c.x, c.y, c.width, c.height);
          g2.dispose();
        }
      };
      canvas.setOpaque(true);
      canvas.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
      canvas.setPreferredSize(new Dimension(900, 600));
      canvas.setFocusable(true);
      f.setContentPane(canvas);
      for (Component c = canvas; c != null; c = c.getParent()) if (c instanceof JComponent jc) jc.setDoubleBuffered(false);
      f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
      f.pack(); f.setLocation(0, 0); f.setVisible(true);
      canvas.requestFocusInWindow();
    });
    Thread.sleep(2500);
    base.set(frame(900, 600, new Color(20, 20, 20)));
    present("whole");
    for (int k = 0; k < PHASES.length; k++) {
      var c = PHASES[k];
      base.set(frame(canvas.getWidth(), canvas.getHeight(), c)); // render thread publishes
      present(mode);
      System.out.printf("PHASE %d %d,%d,%d%n", k + 1, c.getRed(), c.getGreen(), c.getBlue());
      System.out.flush();
      File go = new File(dir, "go." + (k + 1));
      while (!go.exists()) Thread.sleep(100);
    }
    System.out.println("DONE");
    System.exit(0);
  }
}
