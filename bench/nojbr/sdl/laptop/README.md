# SDL3 Wayland present benchmark (throwaway, #2380, never merge)

Measures, on the compositor you are running in: frame pacing, present cost for a full frame and for a typing-sized
update, vsync on/off, and whether the damage SDL sends stays full-surface. Benchmark only: it writes inside one scratch
directory and installs nothing.

```sh
git clone --depth 1 --branch ccr-157ed55f-u6khhf-nojbr-sdl-spike https://github.com/BarMal/Serenity.git /tmp/serenity-sdl-bench-src
cd /tmp/serenity-sdl-bench-src
JAVA="$(readlink -f "$(command -v java)")" \
  nix-shell bench/nojbr/sdl/laptop/shell.nix --run 'bash bench/nojbr/sdl/laptop/run.sh /tmp/sdl-bench'
```

Run it from a terminal inside the Hyprland session, on AC power, with the terminal's workspace visible. A window opens
for a few seconds at a time; leave the keyboard and mouse alone while it runs. It takes a few minutes. Result:
`/tmp/sdl-bench/out/summary.txt`.

Clean up afterwards: `rm -rf /tmp/sdl-bench /tmp/serenity-sdl-bench-src`.
