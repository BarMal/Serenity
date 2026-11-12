package com.serenity.manuscript.epub

import com.serenity.richtext.{InlineMark, RichTextRun, RichTextStyle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** EPUB is the HTML the app writes from rich text, so it is where a shadow becomes CSS `text-shadow`. */
class ShadowMarkEpubSpec extends AnyFlatSpec with Matchers:

  private def run(text: String, marks: InlineMark*): RichTextRun =
    RichTextRun(text, RichTextStyle(marks = marks.toSet))

  "EpubXml" should "wrap a shadowed run in a span of the shadow class" in {
    EpubXml.runs(List(run("shaded", InlineMark.Shadow))) shouldBe """<span class="shadow">shaded</span>"""
  }

  it should "nest the shadow inside the other marks" in {
    EpubXml.runs(List(run("all", InlineMark.Bold, InlineMark.Italic, InlineMark.Underline, InlineMark.Shadow))) shouldBe
      """<strong><em><span class="underline"><span class="shadow">all</span></span></em></strong>"""
  }

  it should "leave a run without the mark exactly as it was" in {
    EpubXml.runs(List(run("plain"))) shouldBe "plain"
    EpubXml.runs(List(run("both", InlineMark.Bold, InlineMark.Underline))) shouldBe
      """<strong><span class="underline">both</span></strong>"""
  }

  "The EPUB stylesheet" should "define the shadow class as a text-shadow" in {
    EpubPackage.stylesheet should include(".shadow {")
    EpubPackage.stylesheet should include("text-shadow:")
  }
end ShadowMarkEpubSpec
