package com.serenity

import java.awt.Color

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.AnimationConfig
import com.serenity.command.{CommandCategory, CommandIntent, CommandRunner, CommandSurfaceItem}
import com.serenity.config.{AppConfig, BackgroundStyle, CursorMode}
import com.serenity.keystroke.events.{MoveDown, MoveRight, TabKey, ToggleCommandRunner}
import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.session.given
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import _root_.io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CursorModeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def makeStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("CursorModeSpec"))
    StateManager.apply(logger).unsafeRunSync()

  // ── AppConfig ────────────────────────────────────────────────────────────

  "AppConfig" should "default cursorMode to Blink" in {
    AppConfig.default.cursorMode shouldBe CursorMode.Blink
  }

  it should "change cursorMode via withCursorMode" in {
    AppConfig.default.withCursorMode(CursorMode.Breathe).cursorMode shouldBe CursorMode.Breathe
  }

  it should "leave other fields unchanged when changing cursorMode" in {
    val config = AppConfig(
      characterAnimation = AnimationConfig.quick,
      showLineNumbers = false,
      blurRadius = 0.5f
    ).withCursorMode(CursorMode.Breathe)
    config.characterAnimation shouldBe AnimationConfig.quick
    config.showLineNumbers shouldBe false
    config.blurRadius shouldBe 0.5f
  }

  // ── CommandRunner settings ───────────────────────────────────────────────

  "CommandRunner Settings category" should "include a cursor mode option item" in {
    val runner = CommandRunner.empty.copy(
      isActive = true,
      activeCategory = CommandCategory.Settings
    )
    runner.visibleItems.collect { case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o } should not be empty
  }

  it should "offer Blink and Breathe choices on the cursor mode option" in {
    val runner = CommandRunner.empty.copy(
      isActive = true,
      activeCategory = CommandCategory.Settings
    )
    val item = runner.visibleItems
      .collectFirst { case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o }
      .get
    item.options.map(_.label) should contain allOf ("Blink", "Breathe")
  }

  it should "map Blink option to SetCursorMode(Blink) intent" in {
    val runner = CommandRunner.empty.copy(isActive = true, activeCategory = CommandCategory.Settings)
    val item = runner.visibleItems
      .collectFirst { case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o }.get
    item.options.find(_.label == "Blink").get.intent shouldBe CommandIntent.SetCursorMode(CursorMode.Blink)
  }

  it should "map Breathe option to SetCursorMode(Breathe) intent" in {
    val runner = CommandRunner.empty.copy(isActive = true, activeCategory = CommandCategory.Settings)
    val item = runner.visibleItems
      .collectFirst { case o: CommandSurfaceItem.OptionItem if o.id == "cursor-mode" => o }.get
    item.options.find(_.label == "Breathe").get.intent shouldBe CommandIntent.SetCursorMode(CursorMode.Breathe)
  }

  // ── StateManager ─────────────────────────────────────────────────────────

  "SetCursorMode" should "update config.cursorMode to Breathe via command runner navigation" in {
    val sm = makeStateManager()
    // Open runner, navigate to Settings (4 tabs), move down to cursor mode option, press Right (Blink → Breathe)
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    for _ <- 1 to 4 do sm.applyEvent(TabKey).unsafeRunSync()
    for _ <- 1 to 4 do sm.applyEvent(MoveDown).unsafeRunSync()
    sm.applyEvent(MoveRight).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().config.cursorMode shouldBe CursorMode.Breathe
  }

  it should "restore Blink by pressing Right again (wraps around)" in {
    val sm = makeStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    for _ <- 1 to 4 do sm.applyEvent(TabKey).unsafeRunSync()
    for _ <- 1 to 4 do sm.applyEvent(MoveDown).unsafeRunSync()
    sm.applyEvent(MoveRight).unsafeRunSync()  // Blink → Breathe
    sm.applyEvent(MoveRight).unsafeRunSync()  // Breathe → Blink (wrap)

    sm.getCurrentState.unsafeRunSync().config.cursorMode shouldBe CursorMode.Blink
  }

  "SetBackgroundStyle" should "update config.backgroundStyle via command runner navigation" in {
    val sm = makeStateManager()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    for _ <- 1 to 4 do sm.applyEvent(TabKey).unsafeRunSync()
    for _ <- 1 to 5 do sm.applyEvent(MoveDown).unsafeRunSync()
    sm.applyEvent(MoveRight).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().config.backgroundStyle shouldBe BackgroundStyle.GlassLike
  }

  // ── SessionState JSON round-trip ──────────────────────────────────────────

  "AppConfig JSON decoder" should "default cursorMode to Blink when key is missing" in {
    import _root_.io.circe.Encoder
    val enc = summon[Encoder[AppConfig]]
    val dec = summon[_root_.io.circe.Decoder[AppConfig]]
    val jsonWithoutCursorMode = enc(AppConfig.default).mapObject(_.remove("cursorMode"))
    val decoded = jsonWithoutCursorMode.as[AppConfig](using dec)
    decoded.isRight shouldBe true
    decoded.toOption.get.cursorMode shouldBe CursorMode.Blink
  }

  it should "round-trip CursorMode.Breathe through JSON" in {
    val appState = AppState.initial.copy(config = AppConfig.default.withCursorMode(CursorMode.Breathe))
    val decoded  = SessionState.fromAppState(appState).asJson.as[SessionState]
    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorMode shouldBe CursorMode.Breathe
  }

  it should "round-trip CursorMode.Blink through JSON" in {
    val appState = AppState.initial.copy(config = AppConfig.default.withCursorMode(CursorMode.Blink))
    val decoded  = SessionState.fromAppState(appState).asJson.as[SessionState]
    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorMode shouldBe CursorMode.Blink
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
    (paneRect.x, paneRect.y + 1)  // header row at paneRect.y, content starts at +1
