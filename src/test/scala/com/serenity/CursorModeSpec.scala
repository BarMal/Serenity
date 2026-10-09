package com.serenity

import java.awt.Color

import _root_.io.circe.syntax.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.session.given
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.RendererEntryPoints
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
    StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()

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
    AppConfig.default.withCursorMode(CursorMode.Blink).cursorMode shouldBe CursorMode.Blink
  }

  it should "store cursor settings inside the cursor sub-config" in {
    val active   = new Color(0x22, 0x44, 0x88)
    val inactive = new Color(0x88, 0x44, 0x22, 0x99)
    val config = AppConfig.default
      .withCursorMode(CursorMode.Blink)
      .withCursorColors(CursorColorConfig(Some(active), Some(inactive)))

    config.cursorConfig shouldBe CursorConfig(
      mode = CursorMode.Blink,
      colors = CursorColorConfig(Some(active), Some(inactive))
    )
  }

  it should "leave other fields unchanged when changing cursorMode" in {
    val config = AppConfig(
      editorConfig = EditorConfig(minimumPaneWidth = 40),
      surfaceConfig = SurfaceConfig(showLineNumbers = false, diagnosticHighlightBlendWeight = 0.5)
    ).withCursorMode(CursorMode.Blink)
    config.editorConfig.minimumPaneWidth shouldBe 40
    config.surfaceConfig.showLineNumbers shouldBe false
    config.surfaceConfig.diagnosticHighlightBlendWeight shouldBe 0.5
  }

  // ── CommandRunner settings ───────────────────────────────────────────────

  "CommandRunner Settings category" should "include a cursor mode option item" in {
    val runner = CommandRunner.empty.copy(isActive = true)
    settingsItems(runner).collect {
      case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o
    } should not be empty
  }

  it should "offer only the Blink choice on the cursor mode option" in {
    val runner = CommandRunner.empty.copy(isActive = true)
    val item = settingsItems(runner).collectFirst {
      case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o
    }.get
    item.options.map(_.label) shouldBe List("Blink")
  }

  it should "map Blink option to SetCursorMode(Blink) intent" in {
    val runner = CommandRunner.empty.copy(isActive = true)
    val item = settingsItems(runner).collectFirst {
      case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o
    }.get
    item.options.find(_.label == "Blink").get.intent shouldBe CommandIntent.Settings(
      SettingsIntent.Cursor(CursorIntent.SetCursorMode(CursorMode.Blink))
    )
  }

  // ── StateManager ─────────────────────────────────────────────────────────

  "SetCursorMode" should "keep Blink when cycling the cursor mode option, which has no other choice" in {
    val sm = makeStateManager()
    openSettingsGroup(sm, "cursor")
    sm.applyEvent(MoveRight).unsafeRunSync()
    sm.applyEvent(MoveRight).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorMode shouldBe CursorMode.Blink
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

  it should "load a stored breathe cursor mode as Blink" in {
    import _root_.io.circe.Json
    val enc = summon[_root_.io.circe.Encoder[AppConfig]]
    val dec = summon[_root_.io.circe.Decoder[AppConfig]]
    List("breathe", "Breathe", "breathing").foreach { stored =>
      val json    = enc(AppConfig.default).mapObject(_.add("cursorMode", Json.fromString(stored)))
      val decoded = json.as[AppConfig](using dec)
      withClue(s"stored cursor mode '$stored': ") {
        decoded.map(_.cursorMode) shouldBe Right(CursorMode.Blink)
      }
    }
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
    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      com.serenity.state.manager.RenderCaches.create()
    )

    val (cx, cy) = cursorScreenPos(state)
    surface.getBg(cx, cy) shouldBe Theme.default.cursor
  }

  it should "render the cursor using cursorColor override instead of theme.cursor" in {
    val state         = AppState.initial
    val surface       = new MockRenderSurface(80, 24)
    val overrideColor = RenderColor.fromRgba(255, 128, 0, 128)
    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      cursorColor = Some(overrideColor),
      com.serenity.state.manager.RenderCaches.create()
    )

    val (cx, cy) = cursorScreenPos(state)
    surface.getBg(cx, cy) shouldBe overrideColor
  }

  it should "hide cursor when cursorVisible is false regardless of override" in {
    val state         = AppState.initial
    val surface       = new MockRenderSurface(80, 24)
    val overrideColor = RenderColor.fromRgba(255, 128, 0, 128)
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      ViewportSize(80, 24),
      cursorColor = Some(overrideColor),
      com.serenity.state.manager.RenderCaches.create()
    )

    val (cx, cy) = cursorScreenPos(state)
    surface.getBg(cx, cy) should not be overrideColor
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
