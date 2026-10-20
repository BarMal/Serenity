#!/usr/bin/env bash
# TEMP: aggregate the JFR execution samples.
jfr print --events jdk.ExecutionSample --stack-depth 40 jvm-diag/rec.jfr > jvm-diag/samples.txt 2>jvm-diag/jfr.err || { echo "jfr failed"; cat jvm-diag/jfr.err; exit 0; }
echo "events: $(grep -c 'jdk.ExecutionSample' jvm-diag/samples.txt)"
sed -n 1,28p jvm-diag/samples.txt
awk '
  /jdk.ExecutionSample/ { inEvent = 1; n = 0; top = ""; ser = ""; chain = ""; ns = 0; next }
  inEvent && /line:/ {
    n++
    line = $0; sub(/^[[:space:]]+/, "", line); sub(/\(.*/, "", line)
    if (n == 1) top = line
    if (line ~ /^com\.serenity/ && ns < 6) { ns++; if (ns == 1) ser = line; chain = chain " < " line }
    next
  }
  inEvent && /^}/ { if (ns > 0) { print "TOP " top; print "SER " ser; print "CHAIN" chain }; inEvent = 0 }
' jvm-diag/samples.txt > jvm-diag/agg.txt
echo "serenity samples: $(grep -c '^TOP' jvm-diag/agg.txt)"
echo "--- top frames";  grep '^TOP' jvm-diag/agg.txt | sort | uniq -c | sort -rn | head -14
echo "--- first serenity frame"; grep '^SER' jvm-diag/agg.txt | sort | uniq -c | sort -rn | head -14
echo "--- chains"; grep '^CHAIN' jvm-diag/agg.txt | sort | uniq -c | sort -rn | head -10
