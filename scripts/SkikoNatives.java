import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Moves the Skiko native libraries out of an assembled Serenity jar and into a directory that the app image ships
 * beside the jar (#2028). Run with the JDK that builds the image, so it behaves the same on every runner:
 *
 *   java scripts/SkikoNatives.java path/to/Serenity.jar path/to/natives linux-x64
 *
 * The native for the named target is copied into the directory; every Skiko native entry (that target's, the other
 * architecture's dylib that the macOS runtime jar always carries, and the .sha256 files) is then deleted from the jar.
 * The jar therefore holds no Mach-O binary for notarisation to inspect, and Skiko can only load the signed copy that
 * -Dskiko.library.path points at. On Windows the ICU data file icudtl.dat travels with the DLL: Skiko's native code
 * looks for it in the DLL's own directory.
 */
public final class SkikoNatives {
  private static final Pattern NATIVE_ENTRY = Pattern.compile(
      "(?:lib)?skiko-(?:linux|macos|windows)-(?:x64|arm64)\\.(?:so|dylib|dll)(?:\\.sha256)?|icudtl\\.dat");

  private SkikoNatives() {}

  public static void main(String[] args) throws IOException {
    if (args.length != 3) {
      System.err.println("usage: java SkikoNatives.java <Serenity.jar> <natives-dir> <target, e.g. linux-x64>");
      System.exit(2);
    }
    Path jar = Path.of(args[0]);
    Path natives = Path.of(args[1]);
    String target = args[2];
    boolean windows = target.startsWith("windows-");

    Files.createDirectories(natives);
    List<String> extracted = new ArrayList<>();
    List<String> removed = new ArrayList<>();

    try (FileSystem zip = FileSystems.newFileSystem(jar)) {
      List<Path> nativeEntries = new ArrayList<>();
      try (DirectoryStream<Path> root = Files.newDirectoryStream(zip.getPath("/"))) {
        for (Path entry : root) {
          String name = entry.getFileName().toString();
          if (NATIVE_ENTRY.matcher(name).matches()) {
            nativeEntries.add(entry);
          }
        }
      }
      for (Path entry : nativeEntries) {
        String name = entry.getFileName().toString();
        boolean library = name.contains("skiko-" + target + ".") && !name.endsWith(".sha256");
        boolean icuData = windows && name.equals("icudtl.dat");
        if (library || icuData) {
          Files.copy(entry, natives.resolve(name), StandardCopyOption.REPLACE_EXISTING);
          extracted.add(name);
        }
        Files.delete(entry);
        removed.add(name);
      }
    }

    if (extracted.stream().noneMatch(name -> name.contains("skiko-" + target + "."))) {
      System.err.println("No Skiko native for " + target + " found in " + jar + "; was it assembled with"
          + " SERENITY_SKIKO_TARGET=" + target + "?");
      System.exit(1);
    }
    if (windows && !extracted.contains("icudtl.dat")) {
      System.err.println("icudtl.dat is missing from " + jar + "; the Windows Skiko DLL cannot load ICU without it.");
      System.exit(1);
    }
    System.out.println("Extracted to " + natives + ": " + String.join(", ", extracted));
    System.out.println("Removed from " + jar + ": " + String.join(", ", removed));
  }
}
