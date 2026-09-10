package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.{CompositeTranslator, Translator}
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompositeTranslatorSpec extends AnyFlatSpec with Matchers:

  private val strokeA = KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty)
  private val strokeB = KeyStrokeInfo(InputKey.Character, Some('b'), Set.empty)
  private val strokeC = KeyStrokeInfo(InputKey.Character, Some('c'), Set.empty)

  private def translatorFor(mapping: PartialFunction[KeyStrokeInfo, Event]): Translator[Event] =
    new Translator[Event]:
      override val converters: List[PartialFunction[KeyStrokeInfo, Event]] = List(mapping)

  "CompositeTranslator" should "dispatch a stroke to whichever constituent translator defines it" in {
    val editorish = translatorFor { case `strokeA` => SaveFile }
    val globalish = translatorFor { case `strokeB` => Quit }
    val composite = CompositeTranslator(editorish, globalish)

    composite.translate(strokeA) shouldBe SaveFile
    composite.translate(strokeB) shouldBe Quit
  }

  it should "prefer the first translator's converter when both define the same stroke" in {
    val first     = translatorFor { case `strokeA` => SaveFile }
    val second    = translatorFor { case `strokeA` => Quit }
    val composite = CompositeTranslator(first, second)

    composite.translate(strokeA) shouldBe SaveFile
  }

  it should "fall through to a later translator when an earlier one leaves the stroke undefined" in {
    val first     = translatorFor { case `strokeB` => Quit }
    val second    = translatorFor { case `strokeA` => SaveFile }
    val composite = CompositeTranslator(first, second)

    composite.translate(strokeA) shouldBe SaveFile
  }

  it should "report an unhandled event when no constituent translator defines the stroke" in {
    val composite = CompositeTranslator(translatorFor { case `strokeA` => SaveFile })

    composite.translate(strokeC).isInstanceOf[UnhandledEvent[?]] shouldBe true
  }

  it should "handle nothing when built from an empty translator list" in {
    val composite = new CompositeTranslator(Nil)

    composite.translate(strokeA).isInstanceOf[UnhandledEvent[?]] shouldBe true
  }
