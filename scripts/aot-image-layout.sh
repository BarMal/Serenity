# Sourced by train-aot-cache.sh and check-aot-cache.sh. Given an app image in "$image", sets "launcher" and "appdir" for
# the Linux layout (bin/Serenity, lib/app) or the Windows layout (Serenity.exe, app); fails for anything else.
if [[ -x "$image/bin/Serenity" && -d "$image/lib/app" ]]; then
  launcher="$image/bin/Serenity"
  appdir="$image/lib/app"
elif [[ -x "$image/Serenity.exe" && -d "$image/app" ]]; then
  launcher="$image/Serenity.exe"
  appdir="$image/app"
else
  echo "Not an app image: $image" >&2
  exit 1
fi
