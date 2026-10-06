import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A language server that answers `initialize` and then stops responding. Run by LspConnectionHungServerSpec as a
 * single-file source program: `java HungLspServer.java <ignore|deaf> <pid-file>`.
 *
 * `ignore` keeps reading stdin but never answers or exits, not even on `exit` or end of input. `deaf` stops reading
 * stdin altogether. Either way only the process being killed ends it. The pid file lets the spec check it is gone.
 */
public final class HungLspServer {
  private static final Pattern ID = Pattern.compile("\"id\"\\s*:\\s*(\\d+)");
  private static final Pattern LENGTH = Pattern.compile("Content-Length:\\s*(\\d+)");

  public static void main(String[] args) throws Exception {
    Files.writeString(Paths.get(args[1]), Long.toString(ProcessHandle.current().pid()));
    InputStream in = System.in;
    OutputStream out = System.out;
    String message;
    while ((message = read(in)) != null) {
      Matcher id = ID.matcher(message);
      if (message.contains("\"initialize\"") && id.find()) {
        String body = "{\"jsonrpc\":\"2.0\",\"id\":" + id.group(1) + ",\"result\":{\"capabilities\":{}}}";
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        out.write(("Content-Length: " + bytes.length + "\r\n\r\n").getBytes(StandardCharsets.UTF_8));
        out.write(bytes);
        out.flush();
        if (args[0].equals("deaf")) break;
      }
    }
    while (true) Thread.sleep(60_000);
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
    return new String(in.readNBytes(Integer.parseInt(length.group(1))), StandardCharsets.UTF_8);
  }

  private static boolean endsWithBlankLine(byte[] bytes) {
    int n = bytes.length;
    return n >= 4 && bytes[n - 4] == '\r' && bytes[n - 3] == '\n' && bytes[n - 2] == '\r' && bytes[n - 1] == '\n';
  }
}
