package com.serenity

import java.awt.Color

import _root_.io.circe.syntax.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.AnimationConfig
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.session.given
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CursorModeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def makeStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("CursorModeSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def settingsItems(runner: CommandRunner): List[CommandSurfaceItem] =
    def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
      group.children.flatMap {
        case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
        case child                               => List(child)
      }

    runner.settingsGroups.flatMap(group => group :: descendants(group))

  // ── AppConfig ────────────────────────────────────────────────────────────

  "AppConfig" should "default cursorMode to Blink" in {
    AppConfig.default.cursorMode shouldBe CursorMode.Blink
  }

  it should "change cursorMode via withCursorMode" in {
    AppConfig.default.withCursorMode(CursorMode.Breathe).cursorMode shouldBe CursorMode.Breathe
  }

  it should "store cursor settings inside the cursor sub-config" in {
    val active   = new Color(0x22, 0x44, 0x88)
    val inactive = new Color(0x88, 0x44, 0x22, 0x99)
    val config = AppConfig.default
      .withCursorMode(CursorMode.Breathe)
      .withCursorColors(CursorColorConfig(Some(active), Some(inactive)))
      .withCursorInfoBarMode(CursorInfoBarMode.Detailed)
      .withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)

    config.cursorConfig shouldBe CursorConfig(
      mode = CursorMode.Breathe,
      colors = CursorColorConfig(Some(active), Some(inactive)),
      infoBarMode = CursorInfoBarMode.Detailed,
      infoBarPlacement = CursorInfoBarPlacement.PinnedBottom
    )
  }

  it should "leave other fields unchanged when changing cursorMode" in {
    val config = AppConfig(
      editorConfig = EditorConfig(characterAnimation = AnimationConfig.quick),
      showLineNumbers = false,
      blurRadius = 0.5f
    ).withCursorMode(CursorMode.Breathe)
    config.editorConfig.characterAnimation shouldBe AnimationConfig.quick
    config.showLineNumbers shouldBe false
    config.blurRadius shouldBe 0.5f
  }

  // ── CommandRunner settings ───────────────────────────────────────────────

  "CommandRunner Settings category" should "include a cursor mode option item" in {
    val runner = CommandRunner.empty.copy(
      isActive = true,
      activeCategory = CommandCategory.Settings
    )
    settingsItems(runner).collect {
      case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o
    } should not be empty
  }

  it should "offer Blink and Breathe choices on the cursor mode option" in {
    val runner = CommandRunner.empty.copy(
      isActive = true,
      activeCategory = CommandCategory.Settings
    )
    val item = settingsItems(runner).collectFirst {
      case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o
    }.get
    item.options.map(_.label) should contain allOf ("Blink", "Breathe")
  }

  it should "map Blink option to SetCursorMode(Blink) intent" in {
    val runner = CommandRunner.empty.copy(isActive = true, activeCategory = CommandCategory.Settings)
    val item = settingsItems(runner).collectFirst {
      case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o
    }.get
    item.options.find(_.label == "Blink").get.intent shouldBe CommandIntent.Settings(
      SettingsIntent.Cursor(CursorIntent.SetCursorMode(CursorMode.Blink))
    )
  }

  it should "map Breathe option to SetCursorMode(Breathe) intent" in {
    val runner = CommandRunner.empty.copy(isActive = true, activeCategory = CommandCategory.Settings)
    val item = settingsItems(runner).collectFirst {
      case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o
    }.get
    item.options.find(_.label == "Breathe").get.intent shouldBe CommandIntent.Settings(
      SettingsIntent.Cursor(CursorIntent.SetCursorMode(CursorMode.Breathe))
    )
  }

  // ── StateManager ─────────────────────────────────────────────────────────

  "SetCursorMode" should "update config.cursorMode to Breathe via command runner navigation" in {
    val sm = makeStateManager()
    // Open runner, navigate to Settings (5 tabs), move down to cursor mode option, press Right (Blink → Breathe)
    openSettingsGroup(sm, "cursor")
    sm.applyEvent(MoveRight).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorMode shouldBe CursorMode.Breathe
  }

  it should "restore Blink by pressing Right again (wraps around)" in {
    val sm = makeStateManager()
    openSettingsGroup(sm, "cursor")
    sm.applyEvent(MoveRight).unsafeRunSync() // Blink → Breathe
    sm.applyEvent(MoveRight).unsafeRunSync() // Breathe → Blink (wrap)

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorMode shouldBe CursorMode.Blink
  }

  "SetBackgroundStyle" should "update config.backgroundStyle via command runner navigation" in {
    val sm = makeStateManager()
    openSettingsGroup(sm, "surface")
    sm.applyEvent(MoveRight).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().persisted.config.backgroundStyle shouldBe BackgroundStyle.GlassLike
  }

  "SetPostProcessingEffect" should "update the effect via command runner navigation" in {
    val sm = makeStateManager()
    openSettingsGroup(sm, "post-processing")
    sm.applyEvent(MoveRight).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().persisted.config.postProcessingEffect shouldBe PostProcessingEffect.Scanlines
  }

  // ── SessionState JSON round-trip ──────────────────────────────────────────

  "AppConfig JSON decoder" should "default cursorMode to Blink when key is missing" in {
    import _root_.io.circe.Encoder
    val enc                   = summon[Encoder[AppConfig]]
    val dec                   = summon[_root_.io.circe.Decoder[AppConfig]]
    val jsonWithoutCursorMode = enc(AppConfig.default).mapObject(_.remove("cursorMode"))
    val decoded               = jsonWithoutCursorMode.as[AppConfig](using dec)
    decoded.isRight shouldBe true
    decoded.toOption.get.cursorMode shouldBe CursorMode.Blink
  }

  it should "round-trip CursorMode.Breathe through JSON" in {
    val initialState = AppState.initial
    val appState = initialState.copy(persisted =
      initialState.persisted.copy(config = AppConfig.default.withCursorMode(CursorMode.Breathe))
    )
    val decoded = SessionState.fromAppState(appState).asJson.as[SessionState]
    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorMode shouldBe CursorMode.Breathe
  }

  it should "round-trip CursorMode.Blink through JSON" in {
    val initialState = AppState.initial
    val appState = initialState.copy(persisted =
      initialState.persisted.copy(config = AppConfig.default.withCursorMode(CursorMode.Blink))
    )
    val decoded = SessionState.fromAppState(appState).asJson.as[SessionState]
    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorMode shouldBe CursorMode.Blink
  }

  it should "default cursor colour overrides to empty when JSON keys are missing" in {
    import _root_.io.circe.Encoder
    val enc                     = summon[Encoder[AppConfig]]
    val dec                     = summon[_root_.io.circe.Decoder[AppConfig]]
    val jsonWithoutCursorColors = enc(AppConfig.default).mapObject(_.remove("cursorColors"))
    val decoded                 = jsonWithoutCursorColors.as[AppConfig](using dec)
    decoded.isRight shouldBe true
    decoded.toOption.get.cursorColors shouldBe CursorColorConfig()
  }

  it should "round-trip configured cursor colours through JSON" in {
    val active = new Color(0x22, 0x44, 0x88)
    val inactive = new Color(
      0x88,
      0x44,
      0x22,
      0x99
    )
    val initialState = AppState.initial
    val appState = initialState.copy(persisted =
      initialState.persisted.copy(config =
        AppConfig.default.withCursorColors(CursorColorConfig(Some(active), Some(inactive)))
      )
    )

    val decoded = SessionState.fromAppState(appState).asJson.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorColors shouldBe CursorColorConfig(Some(active), Some(inactive))
  }

  // ── Renderer cursor color override ───────────────────────────────────────

  "Renderer" should "render the cursor using theme.cursor when no override is given" in {
    val state   = AppState.initial
    val surface = new MockRenderSurface(80, 24)
    Renderer.render(state, cursorVisible = true, surface, ViewportSize(80, 24))

    val (cx, cy) = cursorScreenPos(state)
    surface.getBg(cx, cy) shouldBe Theme.default.cursor
  }

  it should "render the cursor using cursorColor override instead of theme.cursor" in {
    val state        = AppState.initial
    val surface      = new MockRenderSurface(80, 24)
    val breatheColor = new Color(255, 128, 0, 128)
    Renderer.render(state, cursorVisible = true, surface, ViewportSize(80, 24), cursorColor = Some(breatheColor))

    val (cx, cy) = cursorScreenPos(state)
    surface.getBg(cx, cy) shouldBe breatheColor
  }

  it should "hide cursor when cursorVisible is false regardless of override" in {
    val state        = AppState.initial
    val surface      = new MockRenderSurface(80, 24)
    val breatheColor = new Color(255, 128, 0, 128)
    Renderer.render(state, cursorVisible = false, surface, ViewportSize(80, 24), cursorColor = Some(breatheColor))

    val (cx, cy) = cursorScreenPos(state)
    surface.getBg(cx, cy) should not be breatheColor
  }

  private def cursorScreenPos(state: AppState): (Int, Int) =
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect = LayoutEngine.calculatePaneLayouts(state, layout).get(PaneId(0)).get
    (paneRect.x, paneRect.y + 1) // header row at paneRect.y, content starts at +1

  private def openSettingsGroup(sm: StateManager, search: String): Unit =
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    for _ <- 1 to 5 do sm.applyEvent(TabKey).unsafeRunSync()
    search.foreach(char => sm.applyEvent(InsertChar(char)).unsafeRunSync())
    sm.applyEvent(Enter).unsafeRunSync()
