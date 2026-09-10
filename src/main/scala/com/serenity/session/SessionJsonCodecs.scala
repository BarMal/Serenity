package com.serenity.session

import com.serenity.config.AppConfig
import com.serenity.richtext.*
import com.serenity.ui.layout.given
import com.serenity.ui.layout.{PaneSplitDirection, SessionDockedPanel, SessionWorkspaceNode}
import io.circe.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

given Encoder[SessionState] = deriveEncoder

given Encoder[SessionLayout] = deriveEncoder

given Encoder[SessionEditorPane] = deriveEncoder
given Decoder[SessionEditorPane] = deriveDecoder

given Decoder[SessionLayout] = Decoder.instance { cursor =>
  for
    editorPanes              <- cursor.get[List[SessionEditorPane]]("editorPanes")
    activeEditorPaneId       <- cursor.get[Option[Int]]("activeEditorPaneId")
    paneOrder                <- cursor.getOrElse[List[Int]]("paneOrder")(Nil)
    splitDirection           <- cursor.getOrElse[String]("splitDirection")(PaneSplitDirection.Horizontal.toString)
    workspaceTree            <- cursor.getOrElse[Option[SessionWorkspaceNode]]("workspaceTree")(None)
    maximizedWorkspaceNodeId <- cursor.getOrElse[Option[String]]("maximizedWorkspaceNodeId")(None)
    persistedPanels          <- cursor.getOrElse[List[Json]]("dockedPanels")(Nil)
  yield SessionLayout(
    editorPanes,
    activeEditorPaneId,
    paneOrder,
    splitDirection,
    workspaceTree,
    maximizedWorkspaceNodeId,
    persistedPanels.flatMap(_.as[SessionDockedPanel].toOption)
  )
}

given Encoder[SessionFocus] = deriveEncoder
given Decoder[SessionFocus] = deriveDecoder

given Encoder[SessionBuffer] = deriveEncoder

given Encoder[RichTextFidelity] = deriveEncoder
given Decoder[RichTextFidelity] = deriveDecoder

given Encoder[SessionCursorPosition] = deriveEncoder
given Decoder[SessionCursorPosition] = deriveDecoder

given Encoder[SessionViewport] = deriveEncoder

given Decoder[SessionViewport] = Decoder.instance { cursor =>
  for
    leftColumn    <- cursor.downField("leftColumn").as[Int]
    topLine       <- cursor.downField("topLine").as[Int]
    visibleCols   <- cursor.downField("visibleColumns").as[Int]
    visibleLines  <- cursor.downField("visibleLines").as[Int]
    topVisualLine <- cursor.downField("topVisualLine").as[Option[Int]]
  yield SessionViewport(leftColumn, topLine, visibleCols, visibleLines, topVisualLine.getOrElse(0))
}

given Encoder[SessionFindResult] = deriveEncoder
given Decoder[SessionFindResult] = deriveDecoder

given Encoder[SessionDocumentComment] = deriveEncoder
given Decoder[SessionDocumentComment] = deriveDecoder

given Encoder[SessionFindState] = deriveEncoder

given Decoder[SessionFindState] = Decoder.instance { cursor =>
  for
    query        <- cursor.downField("query").as[String]
    currentIndex <- cursor.downField("currentIndex").as[Int]
    results      <- cursor.downField("results").as[Option[List[SessionFindResult]]]
    resultLines  <- cursor.downField("resultLines").as[Option[List[Int]]]
  yield SessionFindState(
    query = query,
    results = results.getOrElse(resultLines.getOrElse(Nil).map(line => SessionFindResult(line, 0))),
    currentIndex = currentIndex
  )
}

given Decoder[SessionBuffer] = Decoder.instance { cursor =>
  for
    id               <- cursor.get[Int]("id")
    filePath         <- cursor.get[Option[String]]("filePath")
    isDirty          <- cursor.get[Boolean]("isDirty")
    language         <- cursor.get[Option[String]]("language")
    isNewEmpty       <- cursor.get[Boolean]("isNewEmpty")
    cursors          <- cursor.get[List[SessionCursorPosition]]("cursors")
    viewport         <- cursor.get[SessionViewport]("viewport")
    unsavedContent   <- cursor.getOrElse[Option[String]]("unsavedContent")(None)
    richTextDocument <- cursor.getOrElse[Option[RichTextDocument]]("richTextDocument")(None)
    richTextFidelity <- cursor.getOrElse[Option[RichTextFidelity]]("richTextFidelity")(None)
    findState        <- cursor.getOrElse[Option[SessionFindState]]("findState")(None)
    bookmarks        <- cursor.getOrElse[List[SessionCursorPosition]]("bookmarks")(Nil)
    documentComments <- cursor.getOrElse[List[SessionDocumentComment]]("documentComments")(Nil)
  yield SessionBuffer(
    id,
    filePath,
    isDirty,
    language,
    isNewEmpty,
    cursors,
    viewport,
    unsavedContent,
    richTextDocument,
    richTextFidelity,
    findState,
    bookmarks,
    documentComments
  )
}

given Decoder[SessionState] = Decoder.instance { cursor =>
  for
    schemaVersion <- cursor.getOrElse[Int]("schemaVersion")(1)
    _ <- Either.cond(
      schemaVersion <= SessionState.CurrentSchemaVersion,
      (),
      DecodingFailure(
        s"Unsupported session schema version: $schemaVersion (current: ${SessionState.CurrentSchemaVersion})",
        cursor.history
      )
    )
    buffers           <- cursor.get[List[SessionBuffer]]("buffers")
    layout            <- cursor.get[SessionLayout]("layout")
    focus             <- cursor.get[Option[SessionFocus]]("focus")
    bufferOrder       <- cursor.get[List[Int]]("bufferOrder")
    config            <- cursor.get[AppConfig]("config")
    themeName         <- cursor.get[String]("themeName")
    recentFiles       <- cursor.getOrElse[List[String]]("recentFiles")(Nil)
    recentFilesByMode <- cursor.getOrElse[Map[String, List[String]]]("recentFilesByMode")(Map.empty)
  yield SessionState(
    buffers = buffers,
    layout = layout,
    focus = focus,
    bufferOrder = bufferOrder,
    config = config,
    themeName = themeName,
    recentFiles = recentFiles,
    recentFilesByMode = recentFilesByMode,
    schemaVersion = schemaVersion
  )
}
