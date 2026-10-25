#!/usr/bin/env bash
# Builds libSDL3.so.0 from the upstream tag pinned in ci/sdl3/pins.env, Wayland only (#2380).
#
# usage: ci/sdl3/build-sdl3.sh WORK_DIR OUT_DIR
#   OUT_DIR receives libSDL3.so.0 (a real file, stripped), LICENSE-SDL.txt (zlib) and include/SDL3 (the headers).
# env: SDL_SOURCE_DIR  use this checkout instead of cloning (it must still be the pinned commit)
#
# OUT_DIR also receives libserenity-glx-shim.so, built from ci/sdl3/glx-shim.c (see com.serenity.ui.sdl.GlxShim).
#
# Build dependencies (Debian/Ubuntu): git cmake ninja-build gcc pkg-config libwayland-dev wayland-protocols
#   libxkbcommon-dev libegl-dev libdecor-0-dev libdbus-1-dev libudev-dev. At run time SDL dlopens libwayland-client,
#   libxkbcommon, libdecor-0 and libdbus-1 from the system; a missing one disables that feature, not the library.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=ci/sdl3/pins.env
source "$here/pins.env"
work="$(mkdir -p "$1" && cd "$1" && pwd)"
out="$(mkdir -p "$2" && cd "$2" && pwd)"

source_dir="${SDL_SOURCE_DIR:-$work/SDL}"
if [ ! -d "$source_dir/.git" ]; then
  git clone --quiet --depth 1 --branch "$SDL_TAG" https://github.com/libsdl-org/SDL "$source_dir"
fi
actual="$(git -C "$source_dir" rev-parse HEAD)"
if [ "$actual" != "$SDL_COMMIT" ]; then
  echo "SDL checkout is $actual, expected $SDL_COMMIT ($SDL_TAG)" >&2
  exit 1
fi

build="$work/build"
cmake -S "$source_dir" -B "$build" -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DSDL_SHARED=ON -DSDL_STATIC=OFF -DSDL_INSTALL=OFF -DSDL_TESTS=OFF -DSDL_EXAMPLES=OFF \
  -DSDL_X11=OFF -DSDL_KMSDRM=OFF -DSDL_VULKAN=OFF -DSDL_GPU=OFF \
  -DSDL_AUDIO=OFF -DSDL_JOYSTICK=OFF -DSDL_HAPTIC=OFF -DSDL_SENSOR=OFF -DSDL_CAMERA=OFF -DSDL_HIDAPI=OFF \
  -DSDL_WAYLAND=ON -DSDL_WAYLAND_SHARED=ON -DSDL_WAYLAND_LIBDECOR=ON -DSDL_WAYLAND_LIBDECOR_SHARED=ON
cmake --build "$build"

rm -f "$out/libSDL3.so.0"
cp "$(readlink -f "$build/libSDL3.so.0")" "$out/libSDL3.so.0"
strip --strip-unneeded "$out/libSDL3.so.0"
cp "$source_dir/LICENSE.txt" "$out/LICENSE-SDL.txt"
rm -f "$out/libserenity-glx-shim.so"
gcc -O2 -shared -fPIC -Wl,-soname,libserenity-glx-shim.so -o "$out/libserenity-glx-shim.so" "$here/glx-shim.c"
strip --strip-unneeded "$out/libserenity-glx-shim.so"
# The headers the library was built from, for ci/sdl3/layout.c to check the Scala binding against.
rm -rf "$out/include" && mkdir -p "$out/include" && cp -r "$source_dir/include/SDL3" "$out/include/SDL3"
echo "built $out/libSDL3.so.0 from $SDL_TAG ($SDL_COMMIT)"
