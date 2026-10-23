import javax.swing.*;

/** A small Swing window for an AT-SPI client to look at. */
public class A11yApp {
  public static void main(String[] args) throws Exception {
    SwingUtilities.invokeAndWait(() -> {
      JFrame f = new JFrame("A11yProbeWindow");
      JPanel p = new JPanel();
      JButton b = new JButton("PressMe");
      b.getAccessibleContext().setAccessibleDescription("a button");
      p.add(b);
      p.add(new JTextField("some text", 15));
      f.add(p);
      f.setSize(400, 200);
      f.setVisible(true);
    });
    Thread.sleep(Long.parseLong(args.length > 0 ? args[0] : "12000"));
    System.exit(0);
  }
}
