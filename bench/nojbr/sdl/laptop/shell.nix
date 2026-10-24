# Throwaway (#2380, never merge). `nix-shell bench/nojbr/sdl/laptop/shell.nix --run 'bash bench/nojbr/sdl/laptop/run.sh /tmp/sdl-bench'`
# Puts only the libraries libSDL3.so dlopens on LD_LIBRARY_PATH (nothing is installed into a profile). JAVA stays
# your own Temurin; pass it in with JAVA=/nix/store/...-temurin-bin-25.../bin/java if `java` on PATH is not it.
{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = [ pkgs.python3 pkgs.curl pkgs.coreutils ];
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
    pkgs.wayland
    pkgs.libxkbcommon
    pkgs.libdecor
    pkgs.dbus
    pkgs.libglvnd
    pkgs.libffi
  ];
}
