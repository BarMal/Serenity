package com.serenity.notices

import scala.concurrent.duration.*

enum NoticeSource:
  case Lsp, Git, TreeSitter, Process, Config, Session, File, Render, Runtime

enum NoticeSubject:
  case Global
  case Language(id: String)
  case Buffer(id: Int)
  case Path(value: String)
  case ConfigKey(key: String)
  case Owner(name: String)

/** Ordered from least to most serious. */
enum NoticeSeverity:
  case Info, Warning, Error

enum NoticeKind:

  /** Something that happened; it is acknowledged, never resolved. */
  case Event

  /** Something that stays true until its raiser sees it stop being true. */
  case Condition

/** What a notice is about. Raising the same key again is the same notice, not a new one. */
final case class NoticeKey(source: NoticeSource, subject: NoticeSubject, code: String)

/** An action a notice offers, naming the command that runs when it is chosen. */
final case class NoticeAction(label: String, command: String)

/** What a raiser says; the centre decides what becomes of it. */
final case class NoticeSpec(
    key: NoticeKey,
    kind: NoticeKind,
    severity: NoticeSeverity,
    message: String,
    actions: List[NoticeAction] = Nil
)

/** Guesses to tune by hand-test (#2523): the limits on entries, on a resolved condition returning, and on toasts. */
final case class NoticePolicy(
    maxEntries: Int = 200,
    reopenCooldown: FiniteDuration = 60.seconds,
    toastInterval: FiniteDuration = 20.seconds,
    toastBurst: Int = 3
)

final case class NoticeCounts(info: Int = 0, warning: Int = 0, error: Int = 0):

  def total: Int = info + warning + error

  def plus(severity: NoticeSeverity): NoticeCounts = adjusted(severity, 1)

  def minus(severity: NoticeSeverity): NoticeCounts = adjusted(severity, -1)

  private def adjusted(severity: NoticeSeverity, by: Int): NoticeCounts =
    severity match
      case NoticeSeverity.Info    => copy(info = info + by)
      case NoticeSeverity.Warning => copy(warning = warning + by)
      case NoticeSeverity.Error   => copy(error = error + by)

/** One notice as the centre holds it: seen by the user (`acknowledged`), or a condition that stopped (`resolvedAt`). */
final case class NoticeEntry(
    key: NoticeKey,
    kind: NoticeKind,
    severity: NoticeSeverity,
    message: String,
    actions: List[NoticeAction],
    count: Int,
    firstAt: Long,
    lastAt: Long,
    acknowledged: Boolean,
    resolvedAt: Option[Long]
):

  def isResolved: Boolean = resolvedAt.isDefined

  def isUnseen: Boolean = !acknowledged && !isResolved

  /** Whether raising `spec` changes anything the user would want to look at again. */
  def sameWordsAs(spec: NoticeSpec): Boolean = severity == spec.severity && message == spec.message
