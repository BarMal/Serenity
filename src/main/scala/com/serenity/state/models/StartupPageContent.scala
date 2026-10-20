package com.serenity.state.models

import java.nio.file.Path

import com.serenity.command.{Command, CommandIntent, FileIntent, SessionIntent, UiPresetsIntent}

/** Pure construction of the startup/splash [[StartupPage]], shared by launch (`AppStartup.startPageState`) and the
  * in-session "return to start page" command (`StateManagerWorkflowCapability`). Kept in the state layer -- rather than
  * in `com.serenity.app` -- so the state manager can rebuild the page without a package cycle back into `app` (which
  * already depends on `state.manager`). Does no filesystem access: callers filter `recentFiles` to existing, readable
  * files first and `recentFolders` to existing folders.
  */
object StartupPageContent:

  val RecentFilesLimit: Int = 5

  private def distinctRecent(paths: List[Path]): List[Path] =
    paths.map(_.toAbsolutePath.normalize()).distinct.take(RecentFilesLimit)

  def createStartPage(
    sessionExists: Boolean,
    recentFiles: List[Path] = Nil,
    configNotice: Option[String] = None,
    resumeIdentifier: Option[String] = None,
    fileOrFolderOpen: Boolean = false,
    recentFolders: List[Path] = Nil
  ): StartupPage =
    val statusMessage =
      configNotice.orElse(Option.when(!sessionExists)("No previous session found"))

    val newSession = StartupAction(
      "new-session",
      "New document",
      Command
        .typed("startup.new-session", "Start a new session", CommandIntent.Session(SessionIntent.StartupNewSession)),
      Some('1'),
      Some("Enter")
    )
    val openActions =
      if fileOrFolderOpen then
        List(
          StartupAction(
            "open",
            "Open...",
            Command.typed(
              "startup.open",
              "Open a file or a folder",
              CommandIntent.Session(SessionIntent.StartupOpenFileOrFolder)
            ),
            Some('2'),
            Some("Enter")
          )
        )
      else
        List(
          StartupAction(
            "open-file",
            "Open file",
            Command.typed(
              "startup.open-file",
              "Open an existing file",
              CommandIntent.Session(SessionIntent.StartupOpenFile)
            ),
            Some('2'),
            Some("Enter")
          ),
          StartupAction(
            "open-folder",
            "Open folder",
            Command.typed(
              "startup.open-folder",
              "Open a folder and show it in the Explorer",
              CommandIntent.Session(SessionIntent.StartupOpenFolder)
            ),
            Some('3'),
            Some("Enter")
          )
        )
    val primaryActions = newSession :: openActions
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
    val recentActions = distinctRecent(recentFiles).map { path =>
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
    val recentFolderActions = distinctRecent(recentFolders).map { path =>
      StartupAction(
        s"recent-folder:${path.toString}",
        path.toString,
        Command.typed(
          s"startup.open-recent-folder.${Option(path.getFileName).getOrElse(path)}",
          s"Open recent folder $path",
          CommandIntent.File(FileIntent.OpenRecentFolder(path))
        ),
        detail = Some("Recent folder")
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
    val actions = primaryActions ++ recentActions ++ recentFolderActions
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
