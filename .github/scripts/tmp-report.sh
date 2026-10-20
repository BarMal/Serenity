#!/usr/bin/env bash
f=jvm-diag/samples.txt
echo "samples: $(grep -c '^TOP' "$f")"
echo "--- top frames"; grep '^TOP' "$f" | sed 's/(.*//' | sort | uniq -c | sort -rn | head -15
echo "--- first serenity frame"; grep '^CHAIN' "$f" | awk -F' < ' '{print $2}' | sed 's/(.*//' | sort | uniq -c | sort -rn | head -15
echo "--- chains"; grep '^CHAIN' "$f" | sed 's/([^)]*)//g' | sort | uniq -c | sort -rn | head -12
