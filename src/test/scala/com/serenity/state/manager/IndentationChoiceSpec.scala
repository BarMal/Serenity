package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.command.IndentationCommands
import com.serenity.config.StatusSegment
import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.widget.Loadable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1996: choosing how one document is indented, and the status line saying how it is. */
class IndentationChoiceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val path = Paths.get("/notes/draft.txt")

  private def withBuffer(indentation: DocumentIndentation, global: IndentationSettings): (AppState, BufferId) =
    val (opened, bufferId) = EditorState.createNewEmptyBuffer(AppState.initial)
    val buffer             = opened.persisted.buffers(bufferId)
    val updated = opened.copy(persisted =
      opened.persisted.copy(
        buffers = opened.persisted.buffers.updated(
          bufferId,
          buffer.copy(document = buffer.document.copy(filePath = Some(path), indentation = indentation))
        ),
        config = opened.persisted.config.withIndentation(global)
      )
    )
    (EditorState.focusBuffer(EditorState.rebalancePanes(updated, Some(bufferId)), bufferId), bufferId)

  private def pickers(state: AppState): List[ListPicker] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.ListPicker(picker) => picker } ++
      state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.ListPicker(p)) => p }

  private val defaults = IndentationSettings(tabSize = 4)
  private val detected = DocumentIndentation(detected = IndentationHint.of(IndentationSettings(tabSize = 2)))

  "Choosing an indentation" should "make it the buffer's effective indentation" in {
    val (state, bufferId) = withBuffer(detected, defaults)
    val eightTabs         = IndentationSettings(tabSize = 8, insertSpaces = false)

    val buffer = IndentationChoice.withIndentation(state, bufferId, Some(eightTabs)).persisted.buffers(bufferId)

    buffer.effectiveIndentation(defaults) shouldBe eightTabs
  }

  it should "leave the text, and whether the buffer is dirty, alone" in {
    val (state, bufferId) = withBuffer(detected, defaults)
    val before            = state.persisted.buffers(bufferId)

    val after = IndentationChoice
      .withIndentation(state, bufferId, Some(IndentationSettings(tabSize = 3)))
      .persisted
      .buffers(bufferId)

    after.document.content shouldBe before.document.content
    after.document.contentVersion shouldBe before.document.contentVersion
    after.document.isDirty shouldBe before.document.isDirty
  }

  "Choosing automatic indentation" should "hand the buffer back to what its text shows" in {
    val (state, bufferId) = withBuffer(detected, defaults)
    val chosen            = IndentationChoice.withIndentation(state, bufferId, Some(IndentationSettings(tabSize = 8)))

    val automatic = IndentationChoice.withIndentation(chosen, bufferId, None).persisted.buffers(bufferId)

    automatic.effectiveIndentation(defaults) shouldBe IndentationSettings(tabSize = 2)
  }

  "The indentation picker" should "offer every style and width, marking the buffer's own choice" in {
    val (state, bufferId) = withBuffer(detected.withChosen(Some(IndentationSettings(tabSize = 4))), defaults)

    val opened = IndentationChoice.withPickerOpened(state).getOrElse(fail("no picker for a focused buffer"))

    val picker = pickers(opened).headOption.getOrElse(fail("no picker shown"))
    picker.title shouldBe "Change Indentation"
    val choices = picker.items match
      case Loadable.Ready(list) => list.items.toList
      case _                    => Nil
    choices.size shouldBe 1 + 2 * IndentationSettings.MaxTabSize
    choices.map(_.label) should contain allOf (
      "Indent Using Spaces: 4",
      "Indent Using Tabs: 8",
      "Use Automatic Indentation"
    )
    choices.filter(_.detail.contains("current")).map(_.label) shouldBe List("Indent Using Spaces: 4")
    choices.map(_.action) should contain(IndentationCommands.set(bufferId, IndentationSettings(tabSize = 4)))
    choices.map(_.action) should contain(IndentationCommands.useAutomatic(bufferId))
  }

  it should "mark automatic indentation as current when nothing was chosen, and say what it comes to" in {
    val (state, _) = withBuffer(detected, defaults)

    val opened = IndentationChoice.withPickerOpened(state).getOrElse(fail("no picker for a focused buffer"))

    val choices = pickers(opened).headOption.map(_.items) match
      case Some(Loadable.Ready(list)) => list.items.toList
      case _                          => Nil
    choices.headOption.map(choice => choice.label -> choice.detail) shouldBe
      Some("Use Automatic Indentation" -> Some("current, Spaces: 2"))
  }

  "The status line" should "show spaces and their width" in {
    val (state, _) = withBuffer(DocumentIndentation(), IndentationSettings(tabSize = 4))

    StatusLineText.render(state, List(StatusSegment.Indentation)) shouldBe Some("Spaces: 4")
  }

  it should "show the tab size when the buffer is indented with tabs" in {
    val (state, _) = withBuffer(DocumentIndentation(), IndentationSettings(tabSize = 8, insertSpaces = false))

    StatusLineText.render(state, List(StatusSegment.Indentation)) shouldBe Some("Tab Size: 8")
  }

  it should "show the buffer's own indentation rather than the global one" in {
    val (state, _) = withBuffer(detected, IndentationSettings(tabSize = 8, insertSpaces = false))

    StatusLineText.render(state, List(StatusSegment.Indentation)) shouldBe Some("Spaces: 2")
  }

  it should "follow a choice made for the buffer" in {
    val (state, bufferId) = withBuffer(detected, defaults)
    val chosen =
      IndentationChoice.withIndentation(state, bufferId, Some(IndentationSettings(tabSize = 8, insertSpaces = false)))

    StatusLineText.render(chosen, List(StatusSegment.Indentation)) shouldBe Some("Tab Size: 8")
  }

  "Reloading a file" should "keep the indentation the user chose and detect the rest afresh" in {
    val (state, bufferId) = withBuffer(detected, defaults)
    val chosen            = IndentationChoice.withIndentation(state, bufferId, Some(IndentationSettings(tabSize = 8)))
    val current           = chosen.persisted.buffers(bufferId)
    val fromDisk          = Buffer.fromString(bufferId, "a\n\tb\n")
    val disk = fromDisk.copy(document =
      fromDisk.document.copy(indentation = DocumentIndentation(detected = IndentationHint(insertSpaces = Some(false))))
    )

    val reloaded =
      FileResults.reloaded(chosen, bufferId, path, current.document.content, disk).persisted.buffers(bufferId)

    reloaded.document.indentation.chosen shouldBe IndentationHint.of(IndentationSettings(tabSize = 8))
    reloaded.document.indentation.detected shouldBe IndentationHint(insertSpaces = Some(false))
  }
