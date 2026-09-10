package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.ui.theme.config.{ThemeConfig, ThemeConfigLoader}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Guards against `themes/dark.conf`/`themes/light.conf` drifting from `ThemeConfig.defaultDark`/`defaultLight`, which
  * they are generated from -- see issue #1410. Only `name` is expected to differ, since the bundled resource themes are
  * loaded under the names "dark"/"light" while the internal constants are "default-dark"/"default-light".
  */
class ThemeResourceSyncSpec extends AnyFlatSpec with Matchers:

  private val loader = new ThemeConfigLoader()

  "themes/dark.conf" should "match ThemeConfig.defaultDark's palette exactly, aside from its name" in {
    val loaded = loader.loadThemeFromResource("themes/dark.conf").unsafeRunSync()
    loaded.copy(name = ThemeConfig.defaultDark.name) shouldBe ThemeConfig.defaultDark
  }

  "themes/light.conf" should "match ThemeConfig.defaultLight's palette exactly, aside from its name" in {
    val loaded = loader.loadThemeFromResource("themes/light.conf").unsafeRunSync()
    loaded.copy(name = ThemeConfig.defaultLight.name) shouldBe ThemeConfig.defaultLight
  }
