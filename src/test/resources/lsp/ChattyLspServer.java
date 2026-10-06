import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A language server that logs a megabyte to stderr before it will answer anything, like metals or rust-analyzer
 * starting up. Run by LspConnectionStderrSpec as a single-file source program (`java ChattyLspServer.java`), which
 * needs no classpath and works the same on every platform.
 */
public final class ChattyLspServer {
  private static final Pattern ID = Pattern.compile("\"id\"\\s*:\\s*(\\d+)");
  private static final Pattern LENGTH = Pattern.compile("Content-Length:\\s*(\\d+)");

  public static void main(String[] args) throws IOException {
    byte[] line = ("x".repeat(99) + "\n").getBytes(StandardCharsets.UTF_8);
    for (int i = 0; i < 10_000; i++) System.err.write(line, 0, line.length);
    System.err.flush();

    InputStream in = System.in;
    OutputStream out = System.out;
    String message;
    while ((message = read(in)) != null) {
      if (message.contains("\"initialize\"")) {
        Matcher id = ID.matcher(message);
        if (id.find()) {
          String body = "{\"jsonrpc\":\"2.0\",\"id\":" + id.group(1) + ",\"result\":{\"capabilities\":{}}}";
          byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
          out.write(("Content-Length: " + bytes.length + "\r\n\r\n").getBytes(StandardCharsets.UTF_8));
          out.write(bytes);
          out.flush();
        }
      }
    }
  }

  private static String read(InputStream in) throws IOException {
    ByteArrayOutputStream header = new ByteArrayOutputStream();
    while (!endsWithBlankLine(header.toByteArray())) {
      int next = in.read();
      if (next < 0) return null;
      header.write(next);
    }
    Matcher length = LENGTH.matcher(header.toString(StandardCharsets.UTF_8));
    if (!length.find()) return null;
    byte[] body = in.readNBytes(Integer.parseInt(length.group(1)));
    return new String(body, StandardCharsets.UTF_8);
  }

  private static boolean endsWithBlankLine(byte[] bytes) {
    int n = bytes.length;
    return n >= 4 && bytes[n - 4] == '\r' && bytes[n - 3] == '\n' && bytes[n - 2] == '\r' && bytes[n - 1] == '\n';
  }
}
