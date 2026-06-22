package com.serenity

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.CommandSurfaceItem
import com.serenity.config.ConfigManager
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.state.models.SurfaceContent
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerFontConfigSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val CodeFontSettingsGroupId = "settings-code-font"
  private val UiFontSettingsGroupId   = "settings-ui-font"

  private def openRunner(stateManager: StateManager): Unit =
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

  private def openSettingsSubmenu(stateManager: StateManager, groupId: String): Unit =
    openRunner(stateManager)
    for _ <- 1 to 5 do stateManager.applyEvent(TabKey).unsafeRunSync()
    val movesDown = selectedSettingsGroupIndex(stateManager, groupId)
    for _ <- 1 to movesDown do stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

  private def selectedSettingsGroupIndex(stateManager: StateManager, groupId: String): Int =
    val runner = stateManager.getCurrentState
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected active command runner"))

    runner.visibleItems.collect { case group: CommandSurfaceItem.GroupItem => group.id }.indexOf(groupId) match
      case -1    => fail(s"Expected settings group $groupId")
      case index => index

  "StateManager" should "invoke the runtime font callback when changing code font size from font settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openSettingsSubmenu(stateManager, CodeFontSettingsGroupId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    List('1', '3').foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.codeFontSize shouldBe 13.0f
  }

  it should "invoke the runtime font callback when changing UI font size from UI font settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openSettingsSubmenu(stateManager, UiFontSettingsGroupId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    List('1', '5').foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.uiFontSize shouldBe 15.0f
  }

  it should "invoke the runtime font callback when changing code ligature shaping from font settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openSettingsSubmenu(stateManager, CodeFontSettingsGroupId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveRight).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.codeLigatures shouldBe false
  }

  it should "invoke the runtime font callback when changing UI font family from UI font settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openSettingsSubmenu(stateManager, UiFontSettingsGroupId)
    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.uiFontFamily shouldBe FontLoader.availableUiFamilies.head
  }

  it should "persist font family changes made through UI font settings" in {
    val sessionRoot  = Files.createTempDirectory("font-config-persistence")
    val expectedFont = FontLoader.availableUiFamilies.lift(1).getOrElse(FontLoader.availableUiFamilies.head)
    val stateManager =
      StateManager
        .apply(
          testLogger("StateManagerFontConfigSpec"),
          sessionRootOverride = Some(sessionRoot)
        )
        .unsafeRunSync()

    openSettingsSubmenu(stateManager, UiFontSettingsGroupId)
    stateManager.applyEvent(Enter).unsafeRunSync()
    if FontLoader.availableUiFamilies.size > 1 then stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val loaded = stateManager.loadSession().unsafeRunSync()

    loaded.map(_.config.fontConfig.uiFontFamily) shouldBe Some(expectedFont)
  }

  it should "persist font size changes made through code font settings to the config file" in {
    val configFile = Files.createTempDirectory("font-config-file").resolve("config.conf")
    val stateManager =
      StateManager
        .apply(
          testLogger("StateManagerFontConfigSpec"),
          configPersistencePath = Some(configFile)
        )
        .unsafeRunSync()

    openSettingsSubmenu(stateManager, CodeFontSettingsGroupId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    List('1', '6').foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val saved = ConfigManager.loadConfig(Some(configFile.toString))
    saved.fontConfig.codeFontSize shouldBe 16.0f
  }

  it should "persist font family changes made through UI font settings to the config file" in {
    val configFile   = Files.createTempDirectory("font-family-config-file").resolve("config.conf")
    val expectedFont = FontLoader.availableUiFamilies.lift(1).getOrElse(FontLoader.availableUiFamilies.head)
    val stateManager =
      StateManager
        .apply(
          testLogger("StateManagerFontConfigSpec"),
          configPersistencePath = Some(configFile)
        )
        .unsafeRunSync()

    openSettingsSubmenu(stateManager, UiFontSettingsGroupId)
    stateManager.applyEvent(Enter).unsafeRunSync()
    if FontLoader.availableUiFamilies.size > 1 then stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val saved = ConfigManager.loadConfig(Some(configFile.toString))
    saved.fontConfig.uiFontFamily shouldBe expectedFont
  }
