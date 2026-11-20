package com.serenity.ui.skia

import java.awt.{Font, GraphicsEnvironment}
import java.nio.file.Paths

import com.serenity.state.manager.RenderCaches
import com.serenity.testkit.TestMeasure
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaTestFontsSpec extends AnyFlatSpec with Matchers:

  private val resource = "/fonts/SerenityFallbackTest.ttf"

  private val fixture: Font =
    val file = Font.createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(resource).toURI).toFile)
    val _    = GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(file)
    new Font(file.getFamily, Font.PLAIN, 12)

  private def familyDrawn(owner: RenderCaches, awt: Font): String =
    SkiaTextCaches.of(owner.backendCaches).fonts.faceFor(FontSpec.fromAwt(awt)).font.getTypeface.getFamilyName

  "A fixture font the system does not hold" should "not be what Skia draws its family with by default" in {
    familyDrawn(TestMeasure.renderCaches(), fixture) should not equal fixture.getFamily
  }

  it should "be what an owner draws its family with once the font is installed for that owner" in {
    val owner = TestMeasure.renderCaches()

    SkiaTestFonts.install(owner.backendCaches, resource) shouldBe Right(fixture.getFamily)

    familyDrawn(owner, fixture) shouldBe fixture.getFamily
  }

  it should "stay the fixture's own to the owner it was installed for" in {
    val (installed, other) = (TestMeasure.renderCaches(), TestMeasure.renderCaches())
    val _                  = SkiaTestFonts.install(installed.backendCaches, resource)

    familyDrawn(other, fixture) should not equal fixture.getFamily
  }

  it should "leave the families the system does hold as they were" in {
    val owner = TestMeasure.renderCaches()
    val _     = SkiaTestFonts.install(owner.backendCaches, resource)
    val plain = new Font(Font.MONOSPACED, Font.PLAIN, 12)

    familyDrawn(owner, plain) shouldBe familyDrawn(TestMeasure.renderCaches(), plain)
  }

  "Installing a font that is not on the classpath" should "say so instead of installing anything" in {
    SkiaTestFonts.install(TestMeasure.renderCaches().backendCaches, "/fonts/NoSuchFont.ttf") shouldBe
      Left("no font resource /fonts/NoSuchFont.ttf")
  }
