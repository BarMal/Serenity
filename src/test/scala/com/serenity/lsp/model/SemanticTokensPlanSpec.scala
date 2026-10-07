package com.serenity.lsp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SemanticTokensPlanSpec extends AnyFlatSpec with Matchers:

  private val visible = Some(LineRange(10, 40))
  private val small   = 200
  private val large   = SemanticTokensPlan.LargeDocumentLines

  private def features(full: Boolean = true, delta: Boolean = false, range: Boolean = false) =
    SemanticTokensFeatures(full, delta, range)

  "SemanticTokensPlan.choose" should "ask for everything when the server offers only full" in {
    SemanticTokensPlan.choose(features(), None, small, visible) shouldBe
      SemanticTokensPlan(Some(PrimaryRequest.Full), None)
  }

  it should "ask for everything even when a result id is held but the server offers no delta" in {
    SemanticTokensPlan.choose(features(), Some("1"), small, visible) shouldBe
      SemanticTokensPlan(Some(PrimaryRequest.Full), None)
  }

  it should "ask for a delta when the server offers it and a result id is held" in {
    SemanticTokensPlan.choose(features(delta = true, range = true), Some("1"), large, visible) shouldBe
      SemanticTokensPlan(Some(PrimaryRequest.Delta("1")), None)
  }

  it should "ask for everything while a delta-capable server has no result id to be relative to" in {
    SemanticTokensPlan.choose(features(delta = true), None, small, visible) shouldBe
      SemanticTokensPlan(Some(PrimaryRequest.Full), None)
  }

  it should "preview the visible lines when the server offers range but no delta" in {
    SemanticTokensPlan.choose(features(range = true), None, small, visible) shouldBe
      SemanticTokensPlan(Some(PrimaryRequest.Full), visible)
  }

  it should "preview the visible lines of a large document while its first result is pending" in {
    SemanticTokensPlan.choose(features(delta = true, range = true), None, large, visible) shouldBe
      SemanticTokensPlan(Some(PrimaryRequest.Full), visible)
  }

  it should "not preview a small document that a delta-capable server will answer for in full" in {
    SemanticTokensPlan.choose(features(delta = true, range = true), None, small, visible) shouldBe
      SemanticTokensPlan(Some(PrimaryRequest.Full), None)
  }

  it should "send no preview while the visible lines are not known, if there is a request for everything" in {
    SemanticTokensPlan.choose(features(range = true), None, small, None) shouldBe
      SemanticTokensPlan(Some(PrimaryRequest.Full), None)
  }

  it should "ask a range-only server for the visible lines and nothing else" in {
    SemanticTokensPlan.choose(features(full = false, range = true), None, small, visible) shouldBe
      SemanticTokensPlan(None, visible)
  }

  it should "ask a range-only server for the whole document while the visible lines are not known" in {
    SemanticTokensPlan.choose(features(full = false, range = true), None, 50, None) shouldBe
      SemanticTokensPlan(None, Some(LineRange(0, 49)))
  }

  it should "ask a range-only server for line 0 of an empty document" in {
    SemanticTokensPlan.choose(features(full = false, range = true), None, 0, None) shouldBe
      SemanticTokensPlan(None, Some(LineRange(0, 0)))
  }
