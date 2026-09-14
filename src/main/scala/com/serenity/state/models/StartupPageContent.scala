package com.serenity.state.models

import java.nio.file.Path

import com.serenity.command.{Command, CommandIntent, FileIntent, SessionIntent, UiPresetsIntent}

/** Pure construction of the startup/splash [[StartupPage]], shared by launch (`AppStartup.startPageState`) and the
  * in-session "return to start page" command (`StateManagerWorkflowCapability`). Kept in the state layer -- rather than
  * in `com.serenity.app` -- so the state manager can rebuild the page without a package cycle back into `app` (which
  * already depends on `state.manager`). Does no filesystem access: callers filter `recentFiles` to existing, readable
  * files first.
  */
object StartupPageContent:

  def createStartPage(
    sessionExists: Boolean,
    recentFiles: List[Path] = Nil,
    configNotice: Option[String] = None,
    resumeIdentifier: Option[String] = None
  ): StartupPage =
    val statusMessage =
      configNotice.orElse(Option.when(!sessionExists)("No previous session found"))

    val primaryActions = List(
      StartupAction(
        "new-session",
        "New document",
        Command
          .typed("startup.new-session", "Start a new session", CommandIntent.Session(SessionIntent.StartupNewSession)),
        Some('1'),
        Some("Enter")
      ),
      StartupAction(
        "open-file",
        "Open file or folder",
        Command.typed(
          "startup.open-file",
          "Open an existing file or directory",
          CommandIntent.Session(SessionIntent.StartupOpenFile)
        ),
        Some('2'),
        Some("Enter")
      )
    )
    val resume = Option.when(sessionExists)(
      StartupResumeHint(
        resumeIdentifier.getOrElse("previous session"),
        Command.typed(
          "startup.restore-session",
          "Restore an existing session",
          CommandIntent.Session(SessionIntent.StartupRestoreSession)
        )
      )
    )
    val recentActions = recentFiles
      .map(path => path.toAbsolutePath.normalize())
      .distinct
      .take(5)
      .map { path =>
        StartupAction(
          s"recent:${path.toString}",
          path.toString,
          Command.typed(
            s"startup.open-recent.${path.getFileName}",
            s"Open recent file $path",
            CommandIntent.File(FileIntent.OpenRecentFile(path))
          ),
          detail = Some("Recent")
        )
      }
    val workflowActions = List(("Writing", 'W'), ("Code", 'C'), ("Compact", 'M')).map { (name, key) =>
      StartupAction(
        s"workflow-${name.toLowerCase}",
        name,
        Command
          .typed(
            s"startup.workflow.${name.toLowerCase}",
            s"Use the $name workflow",
            CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(name))
          ),
        shortcut = Some(key),
        section = StartupActionSection.Workflow
      )
    }
    val actions = primaryActions ++ recentActions
    StartupPage(
      "Welcome to Serenity",
      options = actions.map(_.renderedLabel),
      statusMessage = statusMessage,
      actions = actions,
      workflows = workflowActions,
      resume = resume
    )

  /** The session's main file name (active pane's buffer, else the first buffered file), for the quick-resume hint -- so
    * the user recognises what Tab would resume. Falls back to a generic label for an empty/file-less session.
    */
  def sessionResumeIdentifier(session: AppState): String =
    val persisted = session.persisted
    val activeFile = persisted.layout.activeEditorPaneId
      .flatMap(persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(persisted.buffers.get)
      .flatMap(_.document.filePath)
    val fallbackFile =
      persisted.bufferOrder.flatMap(persisted.buffers.get).flatMap(_.document.filePath).headOption
    activeFile
      .orElse(fallbackFile)
      .flatMap(path => Option(path.getFileName).map(_.toString))
      .getOrElse("previous session")
