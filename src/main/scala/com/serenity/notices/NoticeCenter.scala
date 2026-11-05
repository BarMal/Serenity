package com.serenity.notices

import com.serenity.notices.NoticeCenter.Raised

/** Everything raised and not yet cleared, and how much of it the user has not seen (#2523). */
final case class NoticeCenter(
    entries: Map[NoticeKey, NoticeEntry] = Map.empty,
    unseen: NoticeCounts = NoticeCounts(),
    dropped: Long = 0L,
    toastTat: Long = NoticeCenter.NeverToasted,
    policy: NoticePolicy = NoticePolicy()
):

  /** `observedAt` is when the raiser saw what it reports, so a slow probe cannot undo a newer resolution. */
  def raise(spec: NoticeSpec, observedAt: Long): Raised =
    entries.get(spec.key).fold(admitted(spec, observedAt)) { held =>
      if held.resolvedAt.exists(observedAt < _) then Raised(this, toast = false)
      else if held.isResolved then reopened(held, spec, observedAt)
      else coalesced(held, spec, observedAt)
    }

  /** Only a condition that is still true can be resolved, and only by someone who saw it after it was last raised. */
  def resolve(key: NoticeKey, observedAt: Long): NoticeCenter =
    entries
      .get(key)
      .filter(held => held.kind == NoticeKind.Condition && !held.isResolved && observedAt >= held.lastAt)
      .fold(this)(held => stored(held.copy(resolvedAt = Some(observedAt))))

  def acknowledge(key: NoticeKey): NoticeCenter =
    entries.get(key).filterNot(_.acknowledged).fold(this)(held => stored(held.copy(acknowledged = true)))

  def acknowledgeAll: NoticeCenter =
    entries.values.foldLeft(this)((current, held) => current.acknowledge(held.key))

  /** Forgets resolved conditions and acknowledged events. */
  def clearInactive: NoticeCenter =
    entries.values
      .filter(held => held.isResolved || (held.kind == NoticeKind.Event && held.acknowledged))
      .foldLeft(this)((current, held) => current.removed(held))

  /** What `unseen` should be, counted from the entries; for checking that it is kept right. */
  def recounted: NoticeCounts =
    entries.values.filter(_.isUnseen).foldLeft(NoticeCounts())((counts, held) => counts.plus(held.severity))

  private def admitted(spec: NoticeSpec, now: Long): Raised =
    val room = if entries.size < policy.maxEntries then Some(this) else evictable(spec.kind).map(removed)
    room.fold(Raised(copy(dropped = dropped + 1), toast = false)) { roomy =>
      val (billed, toast) = roomy.toastFor(spec.severity, now)
      Raised(billed.stored(fresh(spec, now)), toast)
    }

  private def reopened(held: NoticeEntry, spec: NoticeSpec, now: Long): Raised =
    val quiet = held.resolvedAt.exists(resolvedAt => now - resolvedAt < policy.reopenCooldown.toNanos)
    if quiet then Raised(stored(refreshed(held, spec, now).copy(resolvedAt = None)), toast = false)
    else
      val (billed, toast) = toastFor(spec.severity, now)
      Raised(billed.stored(fresh(spec, now)), toast)

  private def coalesced(held: NoticeEntry, spec: NoticeSpec, now: Long): Raised =
    if held.sameWordsAs(spec) then Raised(stored(refreshed(held, spec, now)), toast = false)
    else
      val (billed, toast) = toastFor(spec.severity, now)
      Raised(billed.stored(refreshed(held, spec, now)), toast)

  private def fresh(spec: NoticeSpec, now: Long): NoticeEntry =
    NoticeEntry(spec.key, spec.kind, spec.severity, spec.message, spec.actions, 1, now, now, false, None)

  private def refreshed(held: NoticeEntry, spec: NoticeSpec, now: Long): NoticeEntry =
    held.copy(
      kind = spec.kind,
      severity = spec.severity,
      message = spec.message,
      actions = spec.actions,
      count = held.count + 1,
      lastAt = held.lastAt.max(now),
      acknowledged = held.acknowledged && held.sameWordsAs(spec)
    )

  /** Only an error toasts, and no faster than the policy's burst and interval allow (a generic cell rate algorithm). */
  private def toastFor(severity: NoticeSeverity, now: Long): (NoticeCenter, Boolean) =
    if severity != NoticeSeverity.Error then (this, false)
    else
      val interval = policy.toastInterval.toNanos

      val tolerance = interval * (policy.toastBurst - 1).max(0)

      if now >= toastTat - tolerance then (copy(toastTat = toastTat.max(now) + interval), true) else (this, false)

  /** What to forget to make room: resolved, then acknowledged events, then the oldest event; a live condition last. */
  private def evictable(kind: NoticeKind): Option[NoticeEntry] =
    val forEvents: List[NoticeEntry => Boolean] = List(
      _.isResolved,
      held => held.kind == NoticeKind.Event && held.acknowledged,
      _.kind == NoticeKind.Event
    )

    val forConditions: List[NoticeEntry => Boolean] = List(_.acknowledged, _ => true)

    val tiers = if kind == NoticeKind.Condition then forEvents ++ forConditions else forEvents

    tiers.view.flatMap(tier => entries.values.filter(tier).minByOption(_.lastAt)).headOption

  private def stored(entry: NoticeEntry): NoticeCenter =
    val base = entries.get(entry.key).fold(this)(removed)
    base.copy(
      entries = base.entries.updated(entry.key, entry),
      unseen = if entry.isUnseen then base.unseen.plus(entry.severity) else base.unseen
    )

  private def removed(entry: NoticeEntry): NoticeCenter =
    copy(entries = entries - entry.key, unseen = if entry.isUnseen then unseen.minus(entry.severity) else unseen)

object NoticeCenter:

  /** What `raise` made of a notice, and whether to interrupt the user with it. */
  final case class Raised(center: NoticeCenter, toast: Boolean)

  /** Far enough back that the first toast is always allowed, yet the burst can be subtracted without overflow. */
  val NeverToasted: Long = Long.MinValue / 2

  val empty: NoticeCenter = NoticeCenter()
