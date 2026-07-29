#!/usr/bin/env bash

set -euo pipefail

jar_path="${1:?usage: $0 path/to/Serenity.jar}"

if [[ ! -f "$jar_path" ]]; then
  echo "Assembled JAR does not exist: $jar_path" >&2
  exit 1
fi

service_descriptor="META-INF/services/org.slf4j.spi.SLF4JServiceProvider"
if ! jar tf "$jar_path" | grep -Fxq "$service_descriptor"; then
  echo "Assembled JAR is missing $service_descriptor" >&2
  exit 1
fi

probe_dir="$(mktemp -d)"
trap 'rm -rf "$probe_dir"' EXIT

cat > "$probe_dir/SerenitySlf4jProbe.java" <<'EOF'
import org.slf4j.ILoggerFactory;
import org.slf4j.LoggerFactory;

public class SerenitySlf4jProbe {
  public static void main(String[] args) {
    ILoggerFactory factory = LoggerFactory.getILoggerFactory();
    String implementation = factory.getClass().getName();
    if (!implementation.equals("ch.qos.logback.classic.LoggerContext")) {
      throw new IllegalStateException("Expected Logback, found " + implementation);
    }
    System.out.println(implementation);
  }
}
EOF

probe_output="$(java --class-path "$jar_path" "$probe_dir/SerenitySlf4jProbe.java" 2>&1)"
if grep -Fq "No SLF4J providers were found" <<<"$probe_output"; then
  echo "$probe_output" >&2
  exit 1
fi

printf '%s\n' "$probe_output"
