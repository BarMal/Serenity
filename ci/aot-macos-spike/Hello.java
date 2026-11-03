import java.util.List;
import java.util.stream.Collectors;

public class Hello {
  public static void main(String[] args) {
    System.out.println("SPIKE_READY " + List.of(1, 2, 3).stream().map(String::valueOf).collect(Collectors.joining(",")));
  }
}
