package com.serenity.ui.accessibility

import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.CellMetrics

/** Admits a snapshot for publishing only when it, or the metrics placing its nodes, differ from the last one admitted.
  * Every frame syncs accessibility, but the snapshot changes only when its state does; republishing an unchanged one
  * would post toolkit work every frame and repeat its announcements.
  */
final class AccessibilityPublishGate:
  private val lastAdmitted = AtomicReference[Option[(AccessibilitySnapshot, CellMetrics)]](None)

  def admit(snapshot: AccessibilitySnapshot, metrics: CellMetrics): Boolean =
    val previous = lastAdmitted.getAndSet(Some((snapshot, metrics)))
    !previous.exists((admitted, admittedMetrics) =>
      admittedMetrics == metrics && ((admitted eq snapshot) || admitted == snapshot)
    )
