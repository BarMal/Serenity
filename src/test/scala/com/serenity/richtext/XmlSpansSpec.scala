package com.serenity.richtext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class XmlSpansSpec extends AnyFlatSpec with Matchers:

  "XmlSpans" should "locate elements by character offset in document order" in {
    val text  = """<a x="1"><b>one</b><c/><d>two</d></a>"""
    val spans = XmlSpans.scan(text)

    spans.spans.map(_.qualifiedName) shouldBe Vector("a", "b", "c", "d")
    spans.raw(1) shouldBe "<b>one</b>"
    spans.raw(2) shouldBe "<c/>"
    spans.raw(0) shouldBe text
    spans.children(0) shouldBe Vector(1, 2, 3)
    spans.inner(3) shouldBe "two"
    spans.openTag(0) shouldBe """<a x="1">"""
    spans.closeTag(0) shouldBe "</a>"
  }

  it should "not be misled by markup inside comments, CDATA, processing instructions or attribute values" in {
    val text =
      """<?xml version="1.0"?><r><!-- <fake> --><p a=">" b='<x/>'><![CDATA[<notanelement>]]></p><?pi <x?><q/></r>"""
    val spans = XmlSpans.scan(text)

    spans.spans.map(_.qualifiedName) shouldBe Vector("r", "p", "q")
    spans.raw(1) shouldBe """<p a=">" b='<x/>'><![CDATA[<notanelement>]]></p>"""
    spans.raw(2) shouldBe "<q/>"
  }

  it should "keep prefixes in names and find the nesting of same-named elements" in {
    val text  = """<w:p><w:r><w:p/></w:r></w:p>"""
    val spans = XmlSpans.scan(text)

    spans.spans.map(_.qualifiedName) shouldBe Vector("w:p", "w:r", "w:p")
    spans.spans.map(_.parent) shouldBe Vector(-1, 0, 1)
    spans.raw(1) shouldBe "<w:r><w:p/></w:r>"
  }

  it should "treat a self-closing tag with attributes and spaces as empty" in {
    val spans = XmlSpans.scan("""<a><b  k="v" /><c/></a>""")

    spans.raw(1) shouldBe """<b  k="v" />"""
    spans.spans(1).selfClosing shouldBe true
    spans.children(0) shouldBe Vector(1, 2)
  }
