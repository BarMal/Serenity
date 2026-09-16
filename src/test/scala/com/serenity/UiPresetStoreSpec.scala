package com.serenity

import java.nio.file.Files

import _root_.io.circe.parser.decode
import _root_.io.circe.syntax.*
import _root_.io.circe.{Json, JsonObject}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Persistence and concurrency behaviour of `UiPresetStore`, plus editor-pane-layout restore behaviour for `UiPreset`.
  * Split out of `UiPresetSpec` (which keeps the `UiPreset` model/capture/apply tests) to keep both files under the
  * architecture size target.
  */
class UiPresetStoreSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "UiPreset" should "collapse editor panes when a preset targets one editor pane" in {
    val primaryBufferId   = BufferId(0)
    val secondaryBufferId = BufferId(1)
    val pane0             = PaneId(0)
    val pane1             = PaneId(1)
    val secondaryBuffer   = Buffer.newEmpty(secondaryBufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers + (secondaryBufferId -> secondaryBuffer),
        bufferOrder = List(primaryBufferId, secondaryBufferId),
        layout = Layout(
          editorPanes = Map(
            pane0 -> EditorPane.withBuffer(pane0, primaryBufferId),
            pane1 -> EditorPane.withBuffer(pane1, secondaryBufferId)
          ),
          activeEditorPaneId = Some(pane1),
          workspaceTree = Some(TestWorkspaceTrees.linear(pane0, pane1))
        ),
        focus = Focus.EditorPane(pane1)
      ),
      runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(2), nextPaneId = PaneId(2))
    )
    val preset = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))

    val restored = UiPreset.applyToState(preset, state, Theme.dark)

    restored.persisted.layout.editorPanes should have size 1
    restored.persisted.layout.activeEditorPaneId shouldBe Some(pane1)
    restored.persisted.layout.orderedPaneIds shouldBe List(pane1)
    restored.persisted.layout.editorPanes(pane1).bufferId shouldBe Some(secondaryBufferId)
    restored.persisted.buffers.keySet should contain allOf (primaryBufferId, secondaryBufferId)
    restored.persisted.bufferOrder shouldBe List(primaryBufferId, secondaryBufferId)
  }

  it should "restore editor pane count targets above one pane" in {
    val primaryBufferId   = BufferId(0)
    val secondaryBufferId = BufferId(1)
    val pane0             = PaneId(0)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers + (secondaryBufferId -> Buffer.newEmpty(secondaryBufferId)),
        bufferOrder = List(primaryBufferId, secondaryBufferId),
        layout = Layout(
          editorPanes = Map(pane0 -> EditorPane.withBuffer(pane0, primaryBufferId)),
          activeEditorPaneId = Some(pane0),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${pane0.value}"), pane0)))
        )
      ),
      runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(2), nextPaneId = PaneId(1))
    )
    val preset = UiPreset(
      name = "Two Pane Drafting",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      dockedPanels = Nil,
      targetEditorPaneCount = Some(2)
    )

    val restored = UiPreset.applyToState(preset, state, Theme.dark)

    restored.persisted.layout.editorPanes should have size 2
    restored.persisted.layout.orderedPaneIds shouldBe List(PaneId(0), PaneId(1))
    restored.persisted.layout.editorPanes(PaneId(0)).bufferId shouldBe Some(primaryBufferId)
    restored.persisted.layout.editorPanes(PaneId(1)).bufferId shouldBe Some(secondaryBufferId)
    restored.runtime.nextPaneId shouldBe PaneId(2)
    restored.persisted.buffers.keySet should contain allOf (primaryBufferId, secondaryBufferId)
  }

  it should "decode saved presets that do not include editor pane layout intent" in {
    import UiPreset.given

    val preset = UiPreset(
      name = "Legacy",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      dockedPanels = Nil
    )
    val legacyJson = preset.asJson.hcursor
      .downField("targetEditorPaneCount")
      .delete
      .top
      .getOrElse(fail("expected preset json"))
      .noSpaces

    val decoded = decode[UiPreset](legacyJson).getOrElse(fail("legacy preset should decode"))

    decoded.targetEditorPaneCount shouldBe None
  }

  "UiPresetStore" should "persist named presets to disk and replace an existing preset by name" in {
    val path  = Files.createTempDirectory("ui-preset-store").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val first = UiPreset(
      name = "Focus",
      config = AppConfig.default.withPreferredWindowSize(PreferredWindowSize(1000, 700)),
      themeName = "dark",
      dockedPanels = Nil
    )
    val second = first.copy(config = AppConfig.default.withPreferredWindowSize(PreferredWindowSize(1200, 900)))

    (for
      _       <- store.upsert(first)
      _       <- store.upsert(second)
      loaded  <- store.load()
      matched <- store.find("Focus")
    yield
      loaded.presets.map(_.name) shouldBe List("Focus")
      matched.flatMap(_.config.preferredWindowSize) shouldBe Some(PreferredWindowSize(1200, 900))
    ).unsafeRunSync()
  }

  it should "delete, rename, and duplicate custom presets" in {
    val path  = Files.createTempDirectory("ui-preset-store-management").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val focus = UiPreset(
      name = "Focus",
      config = AppConfig.default.withPreferredWindowSize(PreferredWindowSize(1000, 700)),
      themeName = "dark",
      dockedPanels = Nil
    )
    val review = focus.copy(name = "Review Notes")

    (for
      _       <- store.upsert(focus)
      _       <- store.upsert(review)
      _       <- store.duplicate("Focus", "Focus Copy")
      _       <- store.rename("Review Notes", "Review Archive")
      _       <- store.delete("Focus")
      loaded  <- store.load()
      copied  <- store.find("Focus Copy")
      renamed <- store.find("Review Archive")
    yield
      loaded.names.sorted shouldBe List("Focus Copy", "Review Archive")
      copied.flatMap(_.config.preferredWindowSize) shouldBe Some(PreferredWindowSize(1000, 700))
      renamed.map(_.themeName) shouldBe Some("dark")
    ).unsafeRunSync()
  }

  it should "reject built-in and path-like preset names before writing" in {
    val path   = Files.createTempDirectory("ui-preset-store-validation").resolve("ui-presets.json")
    val store  = UiPresetStore(path)
    val preset = UiPreset("Writing", AppConfig.default, Theme.dark.name, Nil)

    store.upsert(preset).attempt.unsafeRunSync().isLeft shouldBe true
    store.upsert(preset.copy(name = "../escape")).attempt.unsafeRunSync().isLeft shouldBe true
    Files.exists(path) shouldBe false
  }

  it should "preserve unknown compatible preset and index fields when saving" in {
    val path   = Files.createTempDirectory("ui-preset-store-future-fields").resolve("ui-presets.json")
    val store  = UiPresetStore(path)
    val preset = UiPreset("Future", AppConfig.default, Theme.dark.name, Nil)
    val input = Json.obj(
      "presets"          -> Json.arr(preset.asJson.mapObject(_.add("futurePresetField", Json.fromString("keep")))),
      "futureIndexField" -> Json.fromString("keep")
    )

    Files.writeString(path, input.noSpaces)
    store.upsert(preset.copy(config = AppConfig.default.withLineNumbers(false))).unsafeRunSync()
    val saved = _root_.io.circe.parser.parse(Files.readString(path)).getOrElse(fail("saved JSON should parse"))

    saved.hcursor.downField("futureIndexField").as[String] shouldBe Right("keep")
    saved.hcursor.downField("presets").downArray.downField("futurePresetField").as[String] shouldBe Right("keep")
  }

  it should "preserve unknown compatible config fields when saving" in {
    val path   = Files.createTempDirectory("ui-preset-store-future-config-fields").resolve("ui-presets.json")
    val store  = UiPresetStore(path)
    val preset = UiPreset("Future", AppConfig.default, Theme.dark.name, Nil)
    val config = preset.asJson.hcursor.downField("config").focus.getOrElse(fail("preset config should encode"))
    val input = Json.obj(
      "presets" -> Json.arr(
        preset.asJson.mapObject(
          _.add(
            "config",
            config.mapObject(
              _.add("futureConfigField", Json.fromString("keep"))
                .add(
                  "fontConfig",
                  config.hcursor
                    .downField("fontConfig")
                    .focus
                    .getOrElse(fail("font config should encode"))
                    .mapObject(
                      _.add("futureFontField", Json.fromString("keep"))
                    )
                )
            )
          )
        )
      )
    )

    Files.writeString(path, input.noSpaces)
    store.upsert(preset.copy(config = AppConfig.default.withLineNumbers(false))).unsafeRunSync()
    val saved       = _root_.io.circe.parser.parse(Files.readString(path)).getOrElse(fail("saved JSON should parse"))
    val savedConfig = saved.hcursor.downField("presets").downArray.downField("config")

    savedConfig.downField("futureConfigField").as[String] shouldBe Right("keep")
    savedConfig.downField("fontConfig").downField("futureFontField").as[String] shouldBe Right("keep")
  }

  it should "reject renaming a preset to an existing normalized name" in {
    val path  = Files.createTempDirectory("ui-preset-store-rename-collision").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val foo   = UiPreset("Foo", AppConfig.default, Theme.dark.name, Nil)
    val bar   = UiPreset("Bar", AppConfig.default.withLineNumbers(false), Theme.dark.name, Nil)

    store.upsert(foo).unsafeRunSync()
    store.upsert(bar).unsafeRunSync()
    store.rename("Foo", "Bar").attempt.unsafeRunSync().isLeft shouldBe true
    store.load().unsafeRunSync().presets should contain theSameElementsInOrderAs List(foo, bar)
  }

  it should "treat canonically equivalent Unicode names as one preset identity" in {
    val path       = Files.createTempDirectory("ui-preset-store-unicode").resolve("ui-presets.json")
    val store      = UiPresetStore(path)
    val composed   = UiPreset("Caf\u00e9", AppConfig.default, Theme.dark.name, Nil)
    val decomposed = composed.copy(name = "Cafe\u0301")

    store.upsert(composed).unsafeRunSync()
    store.upsert(decomposed).attempt.unsafeRunSync().isLeft shouldBe true
    store.find("Cafe\u0301").unsafeRunSync().map(_.name) shouldBe Some("Caf\u00e9")
  }

  it should "preserve compatible unknown fields when overwriting a preset" in {
    val path  = Files.createTempDirectory("ui-preset-store-overwrite-unknown").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val source = UiPreset(
      "Focus",
      AppConfig.default,
      Theme.dark.name,
      Nil,
      unknownFields = JsonObject("futurePresetField" -> Json.fromString("keep")),
      configUnknownFields = JsonObject("futureConfigField" -> Json.fromString("keep"))
    )
    val replacement = source.copy(
      config = AppConfig.default.withLineNumbers(false),
      unknownFields = JsonObject.empty,
      configUnknownFields = JsonObject.empty
    )

    store.upsert(source).unsafeRunSync()
    store.upsert(replacement).unsafeRunSync()

    val saved = store.find("Focus").unsafeRunSync().getOrElse(fail("replacement should be saved"))
    saved.config.surfaceConfig.showLineNumbers shouldBe false
    saved.unknownFields("futurePresetField") shouldBe Some(Json.fromString("keep"))
    saved.configUnknownFields("futureConfigField") shouldBe Some(Json.fromString("keep"))
  }

  it should "keep the last write when two overwrites race for the same preset" in
    (1 to 20).foreach { attempt =>
      val path = Files.createTempDirectory(s"ui-preset-store-concurrent-overwrite-$attempt").resolve("ui-presets.json")
      val source = UiPreset("Focus", AppConfig.default, Theme.dark.name, Nil)
      val storeA = UiPresetStore(path)
      val storeB = UiPresetStore(path)
      val first  = source.copy(config = AppConfig.default.withLineNumbers(false))
      val second = source.copy(config = AppConfig.default.withWordWrap(false))

      storeA.upsert(source).unsafeRunSync()
      val results = (storeA.upsert(first).attempt, storeB.upsert(second).attempt).parTupled.unsafeRunSync()

      results._1 shouldBe Right(())
      results._2 shouldBe Right(())
      List(first, second) should contain(storeA.find("Focus").unsafeRunSync().getOrElse(fail("preset should exist")))
    }

  it should "serialize an overwrite with a concurrent deletion" in
    (1 to 20).foreach { attempt =>
      val path   = Files.createTempDirectory(s"ui-preset-store-overwrite-delete-$attempt").resolve("ui-presets.json")
      val source = UiPreset("Focus", AppConfig.default, Theme.dark.name, Nil)
      val storeA = UiPresetStore(path)
      val storeB = UiPresetStore(path)

      storeA.upsert(source).unsafeRunSync()
      val results = (
        storeA.upsert(source.copy(config = AppConfig.default.withLineNumbers(false))).attempt,
        storeB.delete("Focus").attempt
      ).parTupled.unsafeRunSync()

      results._1 shouldBe Right(())
      results._2 shouldBe Right(())
      storeA.load().unsafeRunSync().names should contain theSameElementsAs
        storeA.find("Focus").unsafeRunSync().map(_.name).toList
    }

  it should "serialize an overwrite with a concurrent rename" in
    (1 to 20).foreach { attempt =>
      val path   = Files.createTempDirectory(s"ui-preset-store-overwrite-rename-$attempt").resolve("ui-presets.json")
      val source = UiPreset("Focus", AppConfig.default, Theme.dark.name, Nil)
      val storeA = UiPresetStore(path)
      val storeB = UiPresetStore(path)

      storeA.upsert(source).unsafeRunSync()
      val results = (
        storeA.upsert(source.copy(config = AppConfig.default.withLineNumbers(false))).attempt,
        storeB.rename("Focus", "Renamed").attempt
      ).parTupled.unsafeRunSync()

      results._2 shouldBe Right(())
      val names = storeA.load().unsafeRunSync().names
      names should contain("Renamed")
      names.distinct shouldBe names
    }
