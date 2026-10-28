package com.serenity.ui.sdl

import org.scalatest.Sequential

/** Every spec that loads the real libSDL3, run one after another: SDL keeps its video state in the process, so two
  * specs initialising, choosing a driver and quitting at once would break each other. Each spec is hidden from
  * discovery and runs only through here.
  */
class SdlNativeSuite
    extends Sequential(
      new SdlBindingSpec,
      new SdlGlBindingSpec,
      new SdlGlWindowSpec,
      new com.serenity.ui.skia.SkiaGlContextNativeSpec,
      new com.serenity.ui.skia.SkiaGlTargetsNativeSpec,
      new com.serenity.ui.skia.SkiaGlFramesNativeSpec,
      new com.serenity.ui.skia.SkiaGlPresenterNativeSpec
    )
