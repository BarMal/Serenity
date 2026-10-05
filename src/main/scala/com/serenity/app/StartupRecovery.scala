package com.serenity.app

import java.nio.file.Path

import com.serenity.app.LaunchReset.Moved
import com.serenity.app.StartupCrashGuard.Decision
import com.serenity.state.manager.StartupRecoveryState
import com.serenity.state.models.AppState

/** What this launch does about recovery, decided once from the command line, the crash guard and the resets already
  * made, so the rest of startup only reads it.
  */
object StartupRecovery:

  val AppName: String = "Serenity"

  /** @param notices
    *   Shown on the start page, since a terminal launch has nowhere else to say them.
    * @param offeredAfter
    *   The unfinished starts that earn a prompt offering safe mode, when this launch is not already in it.
    */
  final case class Plan(safeMode: Boolean, notices: List[String], offeredAfter: Option[Int] = None):

    def windowTitle: String =
      if safeMode then SafeMode.windowTitle(AppName) else AppName

    def noticeWith(configNotice: Option[String]): Option[String] =
      Option(notices ++ configNotice).filter(_.nonEmpty).map(_.mkString(" "))

    def configPersistencePath(path: Path): Option[Path] =
      Option.unless(safeMode)(path)

    def appliedTo(state: AppState): AppState =
      StartupRecoveryState.applied(state, safeMode, offeredAfter)

  object Plan:
    val normal: Plan = Plan(safeMode = false, notices = Nil)

  def plan(
    options: LaunchOptions,
    decision: Decision,
    configMoved: List[Moved],
    sessionMoved: List[Moved]
  ): Plan =
    Plan(
      safeMode = options.safeMode,
      notices = List(
        Option.when(options.safeMode)(SafeMode.Notice),
        configMoved.headOption.map(moved => s"Settings were reset; the old file is kept at ${moved.to}."),
        sessionMoved.headOption.map(moved => s"The session was reset; the old one is kept in ${moved.to.getParent}.")
      ).flatten,
      offeredAfter = decision match
        case Decision.OfferSafeMode(unfinished) => Some(unfinished)
        case Decision.Proceed                   => None
    )
