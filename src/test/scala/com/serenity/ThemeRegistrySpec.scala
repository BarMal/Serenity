package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.ui.theme.DefaultThemes
import com.serenity.ui.theme.config.{ThemeConfigLoader, ThemeRegistry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers issue #1429: `ThemeRegistry.getThemesBySource` reuses the module's one `ThemeConfigLoader` instead of
  * constructing new ones inline, its union/sort logic agrees with `getAvailableThemeNames`'s, and
  * `ThemeConfigLoader`'s bundled-themes fallback names track the actual internal theme keys instead of the stale
  * `"dark"`/`"light"` literals.
  */
class ThemeRegistrySpec extends AnyFlatSpec with Matchers:

  "ThemeRegistry.getThemesBySource" should "agree with getAvailableThemeNames on the full theme set" in {
    val bySource = ThemeRegistry.getThemesBySource.unsafeRunSync()
    val flat     = ThemeRegistry.getAvailableThemeNames.unsafeRunSync()

    bySource.all shouldBe flat
  }

  it should "include every internal theme" in {
    val bySource = ThemeRegistry.getThemesBySource.unsafeRunSync()

    bySource.internal should contain theSameElementsAs DefaultThemes.allInternal.keys.toList
  }

  "ThemeConfigLoader.bundledThemesFallback" should "track the actual internal theme keys, not stale literals" in {
    ThemeConfigLoader.bundledThemesFallback shouldBe DefaultThemes.allInternal.keys.toList.sorted
    ThemeConfigLoader.bundledThemesFallback should not contain "dark"
    ThemeConfigLoader.bundledThemesFallback should not contain "light"
  }
