import java.awt.*;
import java.awt.geom.AffineTransform;
import javax.swing.*;

/** Prints what AWT sees on this session: toolkit, scale, bounds; then shows a frame for a moment. */
public class AwtProbe {
  public static void main(String[] args) throws Exception {
    System.out.println("awt.toolkit.name=" + System.getProperty("awt.toolkit.name") + " java.vm.vendor=" + System.getProperty("java.vm.vendor"));
    long t0 = System.nanoTime();
    Toolkit tk = Toolkit.getDefaultToolkit();
    System.out.printf("toolkit=%s (init %.0f ms)%n", tk.getClass().getName(), (System.nanoTime() - t0) / 1e6);
    GraphicsDevice dev = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
    GraphicsConfiguration gc = dev.getDefaultConfiguration();
    AffineTransform tx = gc.getDefaultTransform();
    System.out.printf("device=%s bounds=%s defaultTransform scale=%.3f x %.3f%n", dev.getClass().getSimpleName(), gc.getBounds(), tx.getScaleX(), tx.getScaleY());
    SwingUtilities.invokeAndWait(() -> {
      JFrame f = new JFrame("probe");
      f.add(new JTextField("hello", 20));
      f.setSize(400, 200);
      f.setVisible(true);
      try { Thread.sleep(1500); } catch (InterruptedException e) { }
      AffineTransform t = f.getGraphicsConfiguration().getDefaultTransform();
      System.out.printf("frame visible=%s showing=%s scale=%.3f bounds=%s%n", f.isVisible(), f.isShowing(), t.getScaleX(), f.getBounds());
      f.dispose();
    });
    System.exit(0);
  }
}
