package com.serenity.perf

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.BundledInterFont
import com.serenity.ui.skia.SkiaTextCaches
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class FrameCountPinnedFontSpec extends AnyFlatSpec with Matchers:

  "The frame-count scenarios' Skia caches" should "draw the pinned Inter from the bundled subset, whatever the machine has installed" in {
    val awt = BundledInterFont.plain(14f)

    FrameCountScenarios.pinnedInterFamily shouldBe Right(awt.getFamily)
  }

  it should "give Skia that family for the AWT font the scenarios lay out with" in {
    val owner = RenderCaches.create()
    val _     = FrameCountScenarios.pinnedInterCaches(owner)

    SkiaTextCaches
      .of(owner.backendCaches)
      .fonts
      .faceFor(BundledInterFont.plain(14f))
      .font
      .getTypeface
      .getFamilyName shouldBe
      BundledInterFont.plain(14f).getFamily
  }
