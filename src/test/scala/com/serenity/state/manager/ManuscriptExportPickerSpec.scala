package com.serenity.state.manager

import com.serenity.command.ManuscriptExportCommands
import com.serenity.manuscript.ManuscriptFileFormat
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.widget.Loadable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ManuscriptExportPickerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def pickers(state: AppState): List[ListPicker] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.ListPicker(picker) => picker } ++
      state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.ListPicker(p)) => p }

  "The manuscript export picker" should "offer DOCX and EPUB, each running its own export command" in {
    val opened = ManuscriptExportPicker.withPickerOpened(AppState.initial).getOrElse(fail("no picker"))

    val choices = pickers(opened).flatMap(_.items match
      case Loadable.Ready(list) => list.items.toList
      case _                    => Nil)
    choices.map(_.detail) shouldBe List(Some(".docx"), Some(".epub"))
    choices.map(_.action) shouldBe ManuscriptFileFormat.values.toList.map(ManuscriptExportCommands.exportAs)
  }

  it should "not open without a document to export" in {
    val empty = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map.empty))

    ManuscriptExportPicker.withPickerOpened(empty) shouldBe None
  }

  it should "name each direct command after its format" in {
    ManuscriptExportCommands.all.map(_.name) shouldBe
      List("export-manuscript", "export-manuscript-docx", "export-manuscript-epub")
  }
