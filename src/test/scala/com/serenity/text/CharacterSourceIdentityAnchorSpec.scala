package com.serenity.text

import com.serenity.text.TextEditing.{BreakIteratorCache, CharacterSource, StringCharacterSource}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Whitebox coverage (hence living in `com.serenity.text` rather than `com.serenity`) for the `CharacterSource`
  * identity-anchor mechanism `BreakIteratorCache.forSource` relies on to decide whether `BreakIterator.setText` can be
  * skipped. `StringCharacterSource` is rebuilt fresh on every call to the `String`-based convenience overloads
  * (`TextEditing.nextGraphemeBoundary(text: String, ...)` and friends), so without an identity anchor distinct from the
  * wrapper instance itself, a caller looping over the same `String` with that overload -- exactly what
  * `TextLayoutSnapshot.graphemeBoundaryOffsets` and `CharacterRenderer.computeGraphemeSpans` do -- never gets a cache
  * hit even though the underlying text never changed.
  */
class CharacterSourceIdentityAnchorSpec extends AnyFlatSpec with Matchers:

  "StringCharacterSource" should "expose the wrapped String as its identity anchor" in {
    val text = "hello world"

    StringCharacterSource(text).identityAnchor should be theSameInstanceAs text
  }

  it should "share the same identity anchor across separate wrappers of the same String reference" in {
    val text = "hello world"

    StringCharacterSource(text).identityAnchor should be theSameInstanceAs StringCharacterSource(text).identityAnchor
  }

  it should "have distinct identity anchors for two different String instances with equal content" in {
    // `new String(...)` defeats literal interning, so these are genuinely distinct references despite `==` content
    // equality -- the case a naive fix (e.g. content-based equality) would wrongly conflate with reuse.
    val a = new String("hello world")
    val b = new String("hello world")

    StringCharacterSource(a).identityAnchor should not be theSameInstanceAs(StringCharacterSource(b).identityAnchor)
  }

  "A CharacterSource's default identityAnchor" should "be the source instance itself" in {
    final class PlainSource(text: String) extends CharacterSource:
      override def length: Int              = text.length
      override def charAt(index: Int): Char = text.charAt(index)

    val source = PlainSource("abc")
    source.identityAnchor should be theSameInstanceAs source
  }

  "BreakIteratorCache.forSource" should
    "skip re-setting the BreakIterator's text for a fresh wrapper sharing the previous call's identity anchor" in {
      val text  = "hello world"
      val cache = new BreakIteratorCache

      val firstIterator      = cache.forSource(StringCharacterSource(text))
      val textAfterFirstCall = firstIterator.getText()

      // A brand-new `StringCharacterSource` instance, but wrapping the exact same `text` reference -- precisely what
      // `TextEditing.nextGraphemeBoundary(text, offset)` builds on every call in a loop like
      // `TextLayoutSnapshot.graphemeBoundaryOffsets`.
      val secondIterator = cache.forSource(StringCharacterSource(text))

      secondIterator should be theSameInstanceAs firstIterator
      // `RuleBasedBreakIterator.getText()` returns the exact `CharacterIterator` last passed to `setText` (confirmed
      // by decompiling icu4j-78.3.jar), so this identity check is a precise, non-flaky witness that `setText` was
      // skipped rather than re-invoked with a fresh `CharacterSourceIterator`.
      secondIterator.getText() should be theSameInstanceAs textAfterFirstCall
    }

  it should "still re-set the BreakIterator's text when the identity anchor genuinely changes" in {
    val cache = new BreakIteratorCache

    val firstIterator       = cache.forSource(StringCharacterSource("hello world"))
    val textAfterFirstCall  = firstIterator.getText()
    val secondIterator      = cache.forSource(StringCharacterSource("goodbye world"))
    val textAfterSecondCall = secondIterator.getText()

    textAfterSecondCall should not be theSameInstanceAs(textAfterFirstCall)
  }

  it should "produce correct grapheme boundaries across repeated calls over the same String reference" in {
    // Exercises the real bug pattern end-to-end through the public String-based API: a loop calling
    // `nextGraphemeBoundary` repeatedly with the same `text` reference, as `TextLayoutSnapshot.graphemeBoundaryOffsets`
    // and `CharacterRenderer.computeGraphemeSpans` both do. Correctness must hold regardless of caching.
    val text = "a🙂b🇺🇸c" // "a" + emoji + "b" + US flag (two regional indicators) + "c"

    @annotation.tailrec
    def boundaries(offset: Int, acc: Vector[Int]): Vector[Int] =
      if offset >= text.length then if acc.lastOption.contains(text.length) then acc else acc :+ text.length
      else
        val next = TextEditing.nextGraphemeBoundary(text, offset)
        boundaries(next, acc :+ next)

    boundaries(0, Vector(0)) shouldBe Vector(0, 1, 3, 4, 8, 9)
  }
