package com.serenity

import _root_.io.circe.Json
import _root_.io.circe.syntax.*
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.rope.Balance
import com.serenity.session.given
import com.serenity.session.{SessionBuffer, SessionFindResult, SessionFindState, SessionState}
import com.serenity.state.models.*
import com.serenity.ui.layout.{
  Layout,
  PanelContent,
  PanelPosition,
  SessionWorkspaceNode,
  SplitAxis,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SessionStateRestorationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "SessionState" should "restore legacy session find state that only stored result lines" in {
    val decoded = _root_.io.circe.parser
      .parse("""{"query":"legacy","resultLines":[2,4],"currentIndex":1}""")
      .flatMap(_.as[SessionFindState])

    decoded shouldBe Right(
      SessionFindState(
        query = "legacy",
        results = List(SessionFindResult(2, 0), SessionFindResult(4, 0)),
        currentIndex = 1
      )
    )
    decoded.toOption.map(SessionFindState.toFindState) shouldBe Some(
      FindState("legacy", List(FindResult(2, 0), FindResult(4, 0)), 1)
    )
  }

  it should "restore legacy session viewports without a wrapped visual offset" in {
    val decoded = _root_.io.circe.parser
      .parse("""{"leftColumn":1,"topLine":2,"visibleColumns":80,"visibleLines":24}""")
      .flatMap(_.as[com.serenity.session.SessionViewport])

    decoded.toOption.map(com.serenity.session.SessionViewport.toViewport) shouldBe
      Some(Viewport(leftColumn = 1, topLine = 2, visibleColumns = 80, visibleLines = 24, topVisualLine = 0))
  }

  it should "restore a legacy session buffer missing every field added since (bookmarks, documentComments, " +
    "richTextDocument, richTextFidelity, findState, unsavedContent)" in {
      val decoded = _root_.io.circe.parser
        .parse(
          """{"id":1,"filePath":null,"isDirty":false,"language":null,"isNewEmpty":false,
          |"cursors":[{"line":0,"column":0}],
          |"viewport":{"leftColumn":0,"topLine":0,"visibleColumns":80,"visibleLines":24}}""".stripMargin
        )
        .flatMap(_.as[SessionBuffer])

      decoded shouldBe Right(
        SessionBuffer(
          id = 1,
          filePath = None,
          isDirty = false,
          language = None,
          isNewEmpty = false,
          cursors = List(com.serenity.session.SessionCursorPosition(0, 0)),
          viewport = com.serenity.session.SessionViewport(0, 0, 80, 24, 0),
          unsavedContent = None,
          richTextDocument = None,
          richTextFidelity = None,
          findState = None,
          bookmarks = Nil,
          documentComments = Nil
        )
      )
    }

  it should "migrate schema-v1 flat pane layouts into an equivalent (horizontal) workspace tree" in {
    val pane0 = PaneId(0)
    val pane1 = PaneId(1)
    val sourceTree = WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("editors"),
        SplitAxis.Vertical,
        0.5,
        WorkspaceNode.Leaf(WorkspaceNodeId("editor-1"), pane1),
        WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), pane0)
      )
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = Map(pane0 -> EditorPane.empty(pane0), pane1 -> EditorPane.empty(pane1)),
          activeEditorPaneId = Some(pane1),
          workspaceTree = Some(sourceTree)
        ),
        focus = Focus.EditorPane(pane1)
      ),
      runtime = AppState.initial.runtime.copy(nextPaneId = PaneId(2))
    )
    val currentJson = SessionState.fromAppState(state).asJson
    val legacyJson = currentJson.mapObject(
      _.add("schemaVersion", Json.fromInt(1))
        .add(
          "layout",
          currentJson.hcursor
            .downField("layout")
            .focus
            .getOrElse(fail("layout should encode"))
            .mapObject(
              _.remove("workspaceTree")
                .remove("maximizedWorkspaceNodeId")
                .remove("dockedPanels")
            )
        )
    )

    val restored = legacyJson
      .as[SessionState]
      .map(SessionState.toAppState(_, Theme.default))
      .getOrElse(fail("schema-v1 session should decode"))

    // #821 retired PaneSplitDirection: order is preserved, but the fallback tree always builds horizontal now.
    restored.persisted.layout.workspaceTree.map(_.paneIds) shouldBe Some(List(pane1, pane0))
    restored.persisted.layout.workspaceTree.map(_.root.axis) shouldBe Some(Some(SplitAxis.Horizontal))
    restored.persisted.layout.maximizedWorkspaceNodeId shouldBe None
    restored.isValid shouldBe true
  }

  it should "fall back safely from invalid persisted workspace trees without losing buffers" in {
    val state       = AppState.initial
    val session     = SessionState.fromAppState(state)
    val invalidTree = SessionWorkspaceNode.EditorLeaf("missing-pane", 999)
    val invalid = session.copy(
      layout = session.layout.copy(workspaceTree = Some(invalidTree)),
      schemaVersion = SessionState.CurrentSchemaVersion
    )

    val restored = SessionState.toAppState(invalid, Theme.default)

    restored.persisted.buffers shouldBe state.persisted.buffers
    restored.persisted.bufferOrder shouldBe state.persisted.bufferOrder
    restored.persisted.layout.workspaceTree.map(_.paneIds) shouldBe Some(List(PaneId(0)))
    restored.persisted.layout.maximizedWorkspaceNodeId shouldBe None
    restored.isValid shouldBe true
  }

  it should "recover stale pane, focus, active-pane, and buffer-order references without losing buffers" in {
    val state   = AppState.initial
    val session = SessionState.fromAppState(state)
    val corrupt = session.copy(
      layout = session.layout.copy(
        editorPanes = session.layout.editorPanes.map(_.copy(bufferId = Some(999))),
        activeEditorPaneId = Some(999)
      ),
      focus = Some(com.serenity.session.SessionFocus.EditorPane(999)),
      bufferOrder = List(999, BufferId(0).value, BufferId(0).value)
    )

    val restored = SessionState.toAppState(corrupt, Theme.default)

    restored.persisted.buffers shouldBe state.persisted.buffers
    restored.persisted.layout.editorPanes(PaneId(0)).bufferId shouldBe None
    restored.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(0))
    restored.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    restored.persisted.bufferOrder shouldBe List(BufferId(0))
    restored.isValid shouldBe true
  }

  it should "restore one empty editor fallback when persisted layout panes are empty" in {
    val state   = AppState.initial
    val session = SessionState.fromAppState(state)
    val corrupt = session.copy(
      layout = session.layout.copy(
        editorPanes = Nil,
        activeEditorPaneId = None,
        workspaceTree = None
      ),
      focus = None
    )

    val restored = SessionState.toAppState(corrupt, Theme.default)

    restored.persisted.buffers shouldBe state.persisted.buffers
    restored.persisted.layout.editorPanes.keySet shouldBe Set(PaneId(0))
    restored.persisted.layout.editorPanes(PaneId(0)).bufferId shouldBe None
    restored.persisted.layout.workspaceTree.map(_.paneIds) shouldBe Some(List(PaneId(0)))
    restored.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(0))
    restored.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    restored.isValid shouldBe true
  }

  it should "ignore unsupported persisted panel content without losing buffers" in {
    val panel = UiSurface.fromPanelContent(SurfaceId("surface-0"), PanelContent.Diagnostics(Nil))
    val tree = WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0)))
      .dock(
        panel.id,
        PanelPosition.Left,
        WorkspaceNodeId("dock-split"),
        WorkspaceNodeId("dock-leaf")
      )
      .getOrElse(fail("Panel should dock"))
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = AppState.initial.persisted.layout.copy(workspaceTree = Some(tree))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(panel),
        nextSurfaceId = 1
      )
    )
    val encoded = SessionState.fromAppState(state).asJson
    val unsupportedPanel = Json.obj(
      "surfaceId" -> Json.fromString(panel.id.value),
      "panel" -> Json.obj(
        "position" -> Json.fromString("Left"),
        "size"     -> Json.fromInt(8),
        "content"  -> Json.obj("FuturePanel" -> Json.obj())
      )
    )
    val futureJson = encoded.mapObject(
      _.add(
        "layout",
        encoded.hcursor
          .downField("layout")
          .focus
          .getOrElse(fail("layout should encode"))
          .mapObject(_.add("dockedPanels", Json.arr(unsupportedPanel)))
      )
    )

    val restored = futureJson
      .as[SessionState]
      .map(SessionState.toAppState(_, Theme.default))
      .getOrElse(fail("unsupported panel content should not reject the session"))

    restored.persisted.buffers shouldBe state.persisted.buffers
    restored.pinnedSurfaces shouldBe Nil
    restored.persisted.layout.workspaceTree.map(_.paneIds) shouldBe Some(List(PaneId(0)))
    restored.isValid shouldBe true
  }

  it should "ignore a legacy UI preset draft field when restoring a session" in {
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withBackgroundStyle(BackgroundStyle.GlassLike)
      )
    )
    val legacy = SessionState.fromAppState(state).asJson.mapObject { session =>
      session.add(
        "uiPresetEditSession",
        Json.obj(
          "id"                -> Json.fromString("draft-1"),
          "draftName"         -> Json.fromString("Drafting"),
          "sourceName"        -> Json.fromString("Drafting"),
          "baselineThemeName" -> Json.fromString(Theme.dark.name),
          "dirty"             -> Json.True
        )
      )
    }

    val decoded  = legacy.as[SessionState].toOption.getOrElse(fail("legacy session should decode"))
    val restored = SessionState.toAppState(decoded, Theme.dark)

    restored.persisted.config.surfaceConfig.backgroundStyle shouldBe BackgroundStyle.GlassLike
  }

  it should "decode a session file written by the current release using old toString enum spellings" in {
    val config = AppConfig.default
      .withCursorMode(CursorMode.Breathe)
      .withCursorInfoBarSegments(List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title))
      .withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
      .withWindowChromeMode(WindowChromeMode.NativeThemed)
      .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      .withDefaultDocumentMode(DefaultDocumentMode.RichText)
      .withInterfaceDensity(InterfaceDensity.Spacious)
      .withMaterialPreset(MaterialPreset.Crystal)
      .withMotionPreset(MotionPreset.Expressive)
      .withMotionConfiguration(
        MotionConfig(
          accessibility = MotionAccessibility.Reduced,
          baseline = MotionPreset.Expressive,
          families = Map(
            MotionFamily.Cursor -> MotionFamilyConfig(
              transitionKind = TransitionKind.Fade,
              animation = None,
              speedScale = 1.0
            )
          )
        )
      )

    val currentJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = config)))
      .asJson
    val configObject =
      currentJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val motionConfigObject =
      configObject("motionConfiguration").flatMap(_.asObject).getOrElse(fail("Expected motionConfiguration object"))
    val familiesObject =
      motionConfigObject("families").flatMap(_.asObject).getOrElse(fail("Expected families object"))
    val cursorFamilyJson = familiesObject("cursor").getOrElse(fail("Expected cursor family entry"))

    // The pre-#1008 release wrote every one of these fields using the enum's `toString` spelling instead of
    // `configKey` (e.g. "NativeThemed" instead of "native-themed"). This rebuilds that old shape from a
    // current-format encode so the fixture stays in sync with the schema instead of being hand-typed JSON.
    val legacyConfigObject = configObject
      .remove("cursorInfoBarSegments")
      .add("cursorMode", Json.fromString("Breathe"))
      .add("cursorInfoBarMode", Json.fromString("Detailed"))
      .add("cursorInfoBarPlacement", Json.fromString("PinnedBottom"))
      .add("windowChromeMode", Json.fromString("NativeThemed"))
      .add("markdownViewMode", Json.fromString("InlineLens"))
      .add("defaultDocumentMode", Json.fromString("RichText"))
      .add("interfaceDensity", Json.fromString("Spacious"))
      .add("materialPreset", Json.fromString("Crystal"))
      .add("motionPreset", Json.fromString("Expressive"))
      .add(
        "motionConfiguration",
        Json.fromJsonObject(
          motionConfigObject
            .add("accessibility", Json.fromString("Reduced"))
            .add("baseline", Json.fromString("Expressive"))
            .add("families", Json.obj("Cursor" -> cursorFamilyJson))
        )
      )
    val legacyJson =
      currentJson.mapObject(_.add("config", Json.fromJsonObject(legacyConfigObject)))

    val decoded = legacyJson.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorMode shouldBe CursorMode.Breathe
    decoded.toOption.get.config.cursorInfoBarSegments shouldBe
      List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title)
    decoded.toOption.get.config.cursorInfoBarPlacement shouldBe CursorInfoBarPlacement.PinnedBottom
    decoded.toOption.get.config.windowChromeMode shouldBe WindowChromeMode.NativeThemed
    decoded.toOption.get.config.markdownViewMode shouldBe MarkdownViewMode.InlineLens
    decoded.toOption.get.config.defaultDocumentMode shouldBe DefaultDocumentMode.RichText
    decoded.toOption.get.config.interfaceDensity shouldBe InterfaceDensity.Spacious
    decoded.toOption.get.config.surfaceConfig.materialPreset shouldBe MaterialPreset.Crystal
    decoded.toOption.get.config.surfaceConfig.motionPreset shouldBe MotionPreset.Expressive
    decoded.toOption.get.config.surfaceConfig.motionConfiguration shouldBe Some(
      MotionConfig(
        accessibility = MotionAccessibility.Reduced,
        baseline = MotionPreset.Expressive,
        families = Map(
          MotionFamily.Cursor -> MotionFamilyConfig(
            transitionKind = TransitionKind.Fade,
            animation = None,
            speedScale = 1.0
          )
        )
      )
    )

    // Writing the same config back out must use the new spelling exclusively -- new writes never regress to
    // toString, even for a session decoded from an old-spelling file.
    val rewrittenJson = decoded.toOption.get.asJson
    val rewrittenConfigObject =
      rewrittenJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))

    rewrittenConfigObject("cursorMode") shouldBe Some(Json.fromString("breathe"))
    rewrittenConfigObject("cursorInfoBarSegments") shouldBe Some(
      Json.arr(Json.fromString("position"), Json.fromString("title"))
    )
    rewrittenConfigObject("cursorInfoBarPlacement") shouldBe Some(Json.fromString("pinned-bottom"))
    rewrittenConfigObject("windowChromeMode") shouldBe Some(Json.fromString("native-themed"))
    rewrittenConfigObject("markdownViewMode") shouldBe Some(Json.fromString("inline-lens"))
    rewrittenConfigObject("defaultDocumentMode") shouldBe Some(Json.fromString("rich-text"))
    rewrittenConfigObject("interfaceDensity") shouldBe Some(Json.fromString("spacious"))
    rewrittenConfigObject("materialPreset") shouldBe Some(Json.fromString("crystal"))
    rewrittenConfigObject("motionPreset") shouldBe Some(Json.fromString("expressive"))
  }
