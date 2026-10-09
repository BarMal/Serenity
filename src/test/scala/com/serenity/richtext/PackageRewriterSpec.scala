package com.serenity.richtext

import java.nio.charset.StandardCharsets
import java.util.zip.ZipEntry

import com.serenity.richtext.RichTextTestPackages.{entries, entry, names}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PackageRewriterSpec extends AnyFlatSpec with Matchers:
  private def utf8(text: String): Array[Byte] = text.getBytes(StandardCharsets.UTF_8)

  private val original = GoldenFixtures.zip(
    List("mimetype" -> utf8("application/x"), "a.xml" -> utf8("<a/>"), "dir/b.bin" -> Array[Byte](1, 2, 3)),
    stored = Set("mimetype")
  )

  "PackageRewriter" should "replace a part in place and copy the rest with their order and methods" in {
    val rewritten = PackageRewriter.rewrite(original, "TEST", Map("a.xml" -> utf8("<a changed='1'/>")), Nil)

    names(rewritten) shouldBe List("mimetype", "a.xml", "dir/b.bin")
    entry(rewritten, "a.xml").text shouldBe "<a changed='1'/>"
    entry(rewritten, "mimetype") shouldBe entry(original, "mimetype")
    entry(rewritten, "mimetype").method shouldBe ZipEntry.STORED
    entry(rewritten, "dir/b.bin").bytes shouldBe Seq[Byte](1, 2, 3)
  }

  it should "append added parts after the existing ones" in {
    val rewritten = PackageRewriter.rewrite(original, "TEST", Map.empty, List("new.xml" -> utf8("<n/>")))

    names(rewritten) shouldBe List("mimetype", "a.xml", "dir/b.bin", "new.xml")
  }

  it should "leave a part alone when an addition names one that exists" in {
    val rewritten = PackageRewriter.rewrite(original, "TEST", Map.empty, List("a.xml" -> utf8("<other/>")))

    entries(rewritten).map(entry => entry.name -> entry.bytes) shouldBe
      entries(original).map(entry => entry.name -> entry.bytes)
  }
