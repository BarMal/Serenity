package com.serenity.session

import _root_.io.circe.parser
import _root_.io.circe.syntax.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.openings.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A buffer's chapter-opening overrides survive a session, and a session without them restores to none. */
class SessionOpeningsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val everything = OpeningOverride(
    enabled = Some(true),
    letterMode = Some(LetterMode.Hybrid),
    raisedRow = Some(RaisedRowRule.Clamp),
    hybridSinkLines = Some(4),
    smallCapsFirstLine = Some(true),
    epigraph = Some(EpigraphRule.ExplicitOnly),
    epigraphMarker = Some("motto"),
    sceneBreak = Some(SceneBreakRule.FullOpening),
    handDropCap = Some(HandDropCapRule.HandPlusSmallCaps)
  )

  private def withOverride(overrides: OpeningOverride): Buffer =
    val fresh = Buffer.fromString(BufferId(1), "a\n")
    fresh.copy(document = fresh.document.copy(openings = overrides))

  private def restored(buffer: Buffer): Buffer =
    val encoded = SessionBuffer.fromBuffer(buffer).asJson.noSpaces
    val decoded = parser.decode[SessionBuffer](encoded).getOrElse(fail("a session buffer should decode"))
    SessionBuffer.toBuffer(decoded)

  "A session" should "restore every override a document was given" in {
    restored(withOverride(everything)).document.openings shouldBe everything
  }

  it should "restore an override that sets only some settings as exactly those" in {
    val partial = OpeningOverride(letterMode = Some(LetterMode.Raised), smallCapsFirstLine = Some(false))

    restored(withOverride(partial)).document.openings shouldBe partial
  }

  it should "store nothing for a document with no overrides" in {
    SessionBuffer.fromBuffer(withOverride(OpeningOverride.Empty)).openings shouldBe None
    restored(withOverride(OpeningOverride.Empty)).document.openings shouldBe OpeningOverride.Empty
  }

  it should "restore a session written before openings were recorded" in {
    val legacy = """{"id":1,"filePath":null,"isDirty":false,"language":null,"isNewEmpty":false,
                   |"cursors":[{"line":0,"column":0}],
                   |"viewport":{"leftColumn":0,"topLine":0,"visibleColumns":80,"visibleLines":24}}""".stripMargin

    parser.decode[SessionBuffer](legacy).map(_.openings) shouldBe Right(None)
  }

  it should "ignore a stored setting it cannot read and keep the rest" in {
    val session = SessionBuffer
      .fromBuffer(withOverride(OpeningOverride.Empty))
      .copy(openings =
        Some(
          SessionOpenings(
            enabled = Some(true),
            letter = Some("sideways"),
            hybridSinkLines = Some(4),
            epigraph = Some("sometimes")
          )
        )
      )

    SessionBuffer.toBuffer(session).document.openings shouldBe
      OpeningOverride(enabled = Some(true), hybridSinkLines = Some(4))
  }

  "A document reloaded from disk" should "keep the overrides the user gave it" in {
    val previous = withOverride(everything).document
    val reloaded = Buffer.fromString(BufferId(1), "b\n").document

    reloaded.keepingOpeningsOf(previous).openings shouldBe everything
    reloaded.openings shouldBe OpeningOverride.Empty
  }
