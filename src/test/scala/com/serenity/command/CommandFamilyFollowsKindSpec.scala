package com.serenity.command

import com.serenity.config.AppMode
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A document that says what it is decides whether prose commands are offered; the workspace's mode decides for plain
  * text, which could be either, and for code tooling. A Markdown file in a code workspace gets bold, a Scala file in a
  * writing workspace does not.
  */
class CommandFamilyFollowsKindSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.default

  private def availability(id: String, mode: AppMode, buffer: Option[BufferKind]): Availability =
    val editing = EditingContext(mode, buffer, Shell.Gui, Focus.EditorPane(PaneId(0)), hasSelection = true)
    CommandAvailability.of(
      registry.findCommand(id).getOrElse(fail(s"$id is not registered")),
      CommandRunnerContext(editingContext = Some(editing))
    )

  private val scala = Some(BufferKind.Code(LanguageId.Scala))

  "A prose command" should "be offered for a Markdown or rich text document even in a code workspace" in
    List(BufferKind.Markdown, BufferKind.RichText).foreach { kind =>
      withClue(s"$kind: ")(availability("bold", AppMode.Code, Some(kind)).isOffered shouldBe true)
    }

  it should "follow the workspace's mode for plain text, which could be either" in {
    availability("bold", AppMode.Code, Some(BufferKind.PlainText)) shouldBe Availability.Hidden
    availability("bold", AppMode.Prose, Some(BufferKind.PlainText)).isOffered shouldBe true
  }

  it should "be hidden for a code document even in a prose workspace" in {
    availability("bold", AppMode.Prose, scala) shouldBe Availability.Hidden
  }

  it should "follow the workspace's mode when nothing is open" in {
    availability("bold", AppMode.Prose, None).isOffered shouldBe true
    availability("bold", AppMode.Code, None) shouldBe Availability.Hidden
  }

  "A code command" should "stay on the workspace's mode whatever the document is" in {
    availability("lsp-hover", AppMode.Prose, scala) shouldBe Availability.Hidden
    availability("lsp-hover", AppMode.Code, Some(BufferKind.Markdown)).isOffered shouldBe true
  }
