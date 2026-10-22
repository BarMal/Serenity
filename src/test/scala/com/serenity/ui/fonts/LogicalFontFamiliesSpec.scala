package com.serenity.ui.fonts

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogicalFontFamiliesSpec extends AnyFlatSpec with Matchers:

  private val noFontconfig: String => Option[String] = _ => None

  private def resolver(
    osName: String,
    installed: Set[String],
    fontconfig: String => Option[String] = noFontconfig
  ): LogicalFontFamilies =
    new LogicalFontFamilies(osName, installed.map(_.toLowerCase), fontconfig)

  "LogicalFontFamilies.documentedFamily" should "name the families AWT's logical fonts are drawn in on Windows" in {
    LogicalFontFamilies.documentedFamily("Serif", "Windows 11") shouldBe Some("Times New Roman")
    LogicalFontFamilies.documentedFamily("SansSerif", "Windows 11") shouldBe Some("Arial")
    LogicalFontFamilies.documentedFamily("Dialog", "Windows 11") shouldBe Some("Arial")
    LogicalFontFamilies.documentedFamily("Monospaced", "Windows 11") shouldBe Some("Courier New")
    LogicalFontFamilies.documentedFamily("DialogInput", "Windows 11") shouldBe Some("Courier New")
  }

  it should "name the families AWT's logical fonts are drawn in on macOS" in {
    LogicalFontFamilies.documentedFamily("Serif", "Mac OS X") shouldBe Some("Times")
    LogicalFontFamilies.documentedFamily("SansSerif", "Mac OS X") shouldBe Some("Helvetica")
    LogicalFontFamilies.documentedFamily("Monospaced", "Mac OS X") shouldBe Some("Courier")
  }

  it should "leave Linux to fontconfig and unknown families alone" in {
    LogicalFontFamilies.documentedFamily("Serif", "Linux") shouldBe None
    LogicalFontFamilies.documentedFamily("Georgia", "Windows 11") shouldBe None
  }

  "LogicalFontFamilies.resolve" should "use the documented family on Windows when it is installed" in {
    resolver("Windows 11", Set("Times New Roman", "Arial")).resolve("Serif") shouldBe "Times New Roman"
    resolver("Windows 11", Set("Times New Roman", "Arial")).resolve("SansSerif") shouldBe "Arial"
  }

  it should "keep the logical name when the documented family is not installed" in {
    resolver("Windows 11", Set("Arial")).resolve("Serif") shouldBe "Serif"
  }

  it should "ask fontconfig for the generic family on Linux" in {
    val asked = scala.collection.mutable.ListBuffer.empty[String]
    val linux = resolver(
      "Linux",
      Set("Inter", "DejaVu Serif", "DejaVu Sans Mono"),
      generic =>
        asked += generic
        Map("sans-serif" -> "Inter", "serif" -> "DejaVu Serif", "monospace" -> "DejaVu Sans Mono").get(generic)
    )
    linux.resolve("SansSerif") shouldBe "Inter"
    linux.resolve("Dialog") shouldBe "Inter"
    linux.resolve("Serif") shouldBe "DejaVu Serif"
    linux.resolve("Monospaced") shouldBe "DejaVu Sans Mono"
    linux.resolve("DialogInput") shouldBe "DejaVu Sans Mono"
    asked.toSet shouldBe Set("sans-serif", "serif", "monospace")
  }

  it should "match the installed family case-insensitively and return it as AWT lists it" in {
    val linux = resolver("Linux", Set("Inter"), _ => Some("inter"))
    linux.resolve("sansserif") shouldBe "inter"
  }

  it should "keep the logical name when fontconfig has no answer or names a family AWT does not list" in {
    resolver("Linux", Set("Inter")).resolve("SansSerif") shouldBe "SansSerif"
    resolver("Linux", Set("Inter"), _ => Some("Not Installed")).resolve("SansSerif") shouldBe "SansSerif"
  }

  it should "pass every other family through unchanged" in {
    val linux = resolver("Linux", Set("Inter", "Georgia"), _ => Some("Inter"))
    linux.resolve("Georgia") shouldBe "Georgia"
    linux.resolve("Monaspace Neon (Bundled)") shouldBe "Monaspace Neon (Bundled)"
    linux.resolve("Inter") shouldBe "Inter"
  }

  it should "ask fontconfig once per generic family, however often a name is resolved" in {
    val calls = new java.util.concurrent.ConcurrentHashMap[String, AtomicInteger]()
    val linux = resolver(
      "Linux",
      Set("Inter"),
      generic =>
        calls.computeIfAbsent(generic, _ => new AtomicInteger(0)).incrementAndGet()
        Some("Inter")
    )
    List.fill(5)(linux.resolve("SansSerif")).distinct shouldBe List("Inter")
    List.fill(5)(linux.resolve("Dialog")).distinct shouldBe List("Inter")
    calls.values.toArray.map(_.asInstanceOf[AtomicInteger].get).toSet shouldBe Set(1)
  }

  it should "not ask fontconfig on Windows or macOS" in {
    val asked = new AtomicInteger(0)
    val probe: String => Option[String] = _ =>
      asked.incrementAndGet()
      None
    resolver("Windows 11", Set("Arial"), probe).resolve("SansSerif")
    resolver("Mac OS X", Set("Helvetica"), probe).resolve("SansSerif")
    asked.get shouldBe 0
  }

  "Fontconfig.parseFamily" should "take the first family of the output" in {
    Fontconfig.parseFamily("Inter\n") shouldBe Some("Inter")
    Fontconfig.parseFamily("  DejaVu Sans Mono  ") shouldBe Some("DejaVu Sans Mono")
    Fontconfig.parseFamily("Inter,Inter Display") shouldBe Some("Inter")
  }

  it should "give nothing for empty output" in {
    Fontconfig.parseFamily("") shouldBe None
    Fontconfig.parseFamily("   \n") shouldBe None
  }
