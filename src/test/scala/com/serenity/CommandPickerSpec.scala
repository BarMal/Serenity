package com.serenity

import com.serenity.command.*
import com.serenity.config.{AppMode, HotkeyConfig}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, AppEventReducer, ModalEventReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2577 P1c: Ctrl+P opens the shared list picker over the palette's commands, so the query is a real text field and
  * every picker key works the same here as in the file, theme and session pickers.
  */
class CommandPickerSpec extends AnyFlatSpec with Matchers:

  given Balance                   = Balance.default
  given registry: CommandRegistry = CommandRegistry.default

  private def picker(state: AppState): Option[ListPicker] =
    state.modalSurface.map(_.content).collect {
      case SurfaceContent.ModalWorkflow(Modal.ListPicker(open)) if open.isCommandPicker => open
    }

  private def opened(state: AppState = AppState.initial): AppState =
    AppEventReducer.reduce(ToggleCommandRunner, state, registry).state

  private def typed(state: AppState, text: String): AppState =
    text.foldLeft(state)((current, char) =>
      ModalEventReducer.reduce(ModalType.ListPicker, InsertChar(char), current).state
    )

  private def choices(state: AppState): List[ListChoice] =
    picker(state).flatMap(_.items.toOption).map(_.items.toList).getOrElse(Nil)

  /** What the palette would list for the same query, as `CommandPaletteResults` builds it. */
  private def paletteLabels(state: AppState, query: String): List[String] =
    val runner = CommandRunner.empty
      .activate(registry, state.persisted.config, state.runtime.capabilities, state.commandRunnerContext)
      .copy(commandUsage = state.persisted.commandUsage)
      .updateSearchTerm(query)
    runner.visibleItems.collect {
      case CommandSurfaceItem.CommandItem(command, _)    => command.label
      case setting: CommandSurfaceItem.SettingSearchItem => setting.label
      case group: CommandSurfaceItem.GroupItem => runner.settingsGroupBreadcrumbLabels(group.id).mkString(" > ")
    }

  "Ctrl+P" should "open a command picker with an empty query and focus it" in {
    val state = opened()

    picker(state).map(_.queryText) shouldBe Some("")
    state.persisted.focus shouldBe Focus.Surface(state.modalSurface.map(_.id).getOrElse(fail("no picker")))
    state.commandRunnerSurface shouldBe None
  }

  it should "keep the palette's key hints under its choices" in {
    picker(opened()).flatMap(_.footer) shouldBe Some(com.serenity.ui.layout.KeyHintVocabulary.browse("run", "close"))
  }

  it should "list what the palette lists, for an empty query and as one is typed" in {
    val empty = opened()
    choices(empty).map(_.label) shouldBe paletteLabels(AppState.initial, "")

    for query <- List("save", "line", "font size", "theme") do
      withClue(s"query '$query': ") {
        choices(typed(opened(), query)).map(_.label) shouldBe paletteLabels(AppState.initial, query)
      }
  }

  it should "close the picker when pressed again" in {
    picker(AppEventReducer.reduce(ToggleCommandRunner, opened(), registry).state) shouldBe None
  }

  private def openedOn(osName: String): AppState =
    val state = AppState.initial
    opened(
      state.copy(persisted =
        state.persisted.copy(config = state.persisted.config.withHotkeyConfig(HotkeyConfig.forOs(osName)))
      )
    )

  "A command choice" should "carry its description and key, and run its command on Enter" in {
    val searched = typed(opened(), "save")
    val save     = choices(searched).headOption.getOrElse(fail("no choices"))

    save.label shouldBe "Save"
    save.detail shouldBe Some("Save the current file.")

    val picked = ModalEventReducer.reduce(ModalType.ListPicker, Enter, searched)
    picked.effects shouldBe List(AppEffect.ExecuteCommand(save.action))
    picker(picked.state) shouldBe None
  }

  it should "show the platform's primary modifier as its key" in
    List("Linux" -> "ctrl+s", "Mac OS X" -> "meta+s").foreach { (osName, expected) =>
      val save = choices(typed(openedOn(osName), "save")).headOption.getOrElse(fail(s"no choices on $osName"))

      save.hint.map(_.toLowerCase) shouldBe Some(expected)
    }

  it should "show why it can't run, and refuse it" in {
    val prose = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(config = AppState.initial.persisted.config.withAppMode(AppMode.Prose))
    )
    val searched = typed(opened(prose), "cut to darlings")
    val darlings = choices(searched).find(_.label == "Cut to Darlings").getOrElse(fail("no Cut to Darlings"))

    darlings.disabledReason shouldBe Some(CommandAvailability.NeedsSelection)
  }

  "A settings hit" should "open the settings view drilled to it" in {
    val searched = typed(opened(), "font size")
    val hit = choices(searched)
      .find(_.action.intent match
        case CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.OpenSettingsAt(_, Some(_), _))) => true
        case _ => false)
      .getOrElse(fail("no settings hit for 'font size'"))

    hit.action.intent match
      case CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.OpenSettingsAt(group, item, query))) =>
        group should not be empty
        item should not be empty
        query shouldBe "font size"
      case other => fail(s"unexpected intent $other")
  }
