# Renderer research for #2380 (throwaway, never merge)

Evidence behind the comparison and ranking posted on issue #2380 (comment 6394975602). Every statement in these files is
labelled read, measured or inferred, and each file ends with what could not be verified. Research only: no product code.

- B-own-skiko-build.md: building Skiko's natives ourselves with EGL; cost of owning the build; Vulkan in Skiko.
- D1-skia-family-and-2d-engines.md: Skiko, Skija, a C shim over Skia, Blend2D, Cairo, ThorVG, NanoVG, Vello, Pathfinder, tiny-skia.
- D2-gpu-apis-toolkits-text.md: wgpu-native, Dawn, SDL_GPU, GL/Vulkan, JavaFX, SWT, WLToolkit, LWJGL; HarfBuzz, FreeType, ICU, Pango, Rust text crates.

Research C (Vulkan through Skiko 0.153 and 0.154+) is written up in the issue comment itself.
