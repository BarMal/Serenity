package com.serenity.app

import java.nio.file.Path

import cats.effect.IO
import com.serenity.app.LaunchReset.Moved
import com.serenity.app.StartupCrashGuard.Decision
import com.serenity.diagnostics.PreviousRun
import com.serenity.state.manager.StartupRecoveryState
import com.serenity.state.models.AppState
import com.serenity.ui.presets.UiPresetStore

/** What this launch does about recovery, decided once from the command line, the crash guard and the resets already
  * made, so the rest of startup only reads it.
  */
object StartupRecovery:

  val AppName: String = "Serenity"

  type CrashRecorder = (String, Throwable) => IO[Unit]

  val ignoreCrashes: CrashRecorder = (_, _) => IO.unit

  /** @param notices
    *   Shown on the start page, since a terminal launch has nowhere else to say them.
    * @param crashLoopAfter
    *   The unfinished starts that made this launch start in safe mode by itself, which also earns the prompt offering a
    *   normal restart.
    * @param unexpectedExit
    *   The report of a previous run that did not end cleanly, offered to the user once. A crash loop's prompt speaks
    *   first, since it is the more actionable of the two.
    * @param crashRecorder
    *   How a failure of this run is left for the next launch to report.
    */
  final case class Plan(
      safeMode: Boolean,
      notices: List[String],
      crashLoopAfter: Option[Int] = None,
      unexpectedExit: Option[String] = None,
      crashRecorder: CrashRecorder = ignoreCrashes
  ):

    def windowTitle: String =
      if safeMode then SafeMode.windowTitle(AppName) else AppName

    def noticeWith(configNotice: Option[String]): Option[String] =
      Option(notices ++ configNotice).filter(_.nonEmpty).map(_.mkString(" "))

    def configPersistencePath(path: Path): Option[Path] =
      Option.unless(safeMode)(path)

    /** Safe mode keeps its presets beside its scratch session, so saving one never touches the user's file. Any other
      * launch keeps them where they always were, even on a session root of its own.
      */
    def uiPresetStore(sessionRoot: Option[Path]): UiPresetStore =
      sessionRoot
        .filter(_ => safeMode)
        .fold(UiPresetStore.default)(root => UiPresetStore(root.resolve("ui-presets.json")))

    def appliedTo(state: AppState): AppState =
      StartupRecoveryState.applied(state, safeMode, crashLoopAfter, unexpectedExit)

  object Plan:
    val normal: Plan = Plan(safeMode = false, notices = Nil)

  def plan(
    options: LaunchOptions,
    decision: Decision,
    configMoved: List[Moved],
    sessionMoved: List[Moved],
    previousRun: PreviousRun = PreviousRun.Clean
  ): Plan =
    val crashLoopAfter = decision match
      case Decision.StartSafeMode(unfinished) => Some(unfinished)
      case Decision.Proceed                   => None
    Plan(
      safeMode = options.safeMode || crashLoopAfter.isDefined,
      notices = List(
        Option.when(options.safeMode || crashLoopAfter.isDefined)(SafeMode.Notice),
        crashLoopAfter.map(unfinished =>
          s"Serenity did not finish starting the last $unfinished times, so it started in safe mode."
        ),
        configMoved.headOption.map(moved => s"Settings were reset; the old file is kept at ${moved.to}."),
        sessionMoved.headOption.map(moved => s"The session was reset; the old one is kept in ${moved.to.getParent}.")
      ).flatten,
      crashLoopAfter = crashLoopAfter,
      unexpectedExit = previousRun match
        case PreviousRun.Abnormal(report) => Some(report)
        case PreviousRun.Clean            => None
    )
