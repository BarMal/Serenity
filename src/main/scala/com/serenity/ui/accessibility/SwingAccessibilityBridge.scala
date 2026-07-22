package com.serenity.ui.accessibility

import java.util.concurrent.atomic.AtomicReference
import javax.accessibility.AccessibleContext
import javax.swing.JComponent

/** Publishes canvas semantics through Swing's native accessibility context. */
final class SwingAccessibilityBridge(canvas: JComponent):
  private val previous = AtomicReference[Option[AccessibilitySnapshot]](None)

  /** Update the canvas identity and announce only semantic focus or status changes. */
  def publish(snapshot: AccessibilitySnapshot): Unit =
    val context          = canvas.getAccessibleContext
    val name             = snapshot.focused.map(_.name).getOrElse("Serenity editor")
    val description      = describe(snapshot)
    val priorDescription = previous.get.map(describe).orNull
    context.setAccessibleName(name)
    context.setAccessibleDescription(description)
    announcements(previous.get, snapshot).foreach { announcement =>
      context.firePropertyChange(
        AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY,
        priorDescription,
        announcement.message
      )
    }
    previous.set(Some(snapshot))

  private def describe(snapshot: AccessibilitySnapshot): String =
    snapshot.focused match
      case Some(node) =>
        val value = node.value.filter(_.nonEmpty).fold("")(current => s": $current")
        s"${node.role.toString.toLowerCase} ${node.name}$value"
      case None => "Canvas-rendered Serenity editor"

  private def announcements(
    prior: Option[AccessibilitySnapshot],
    current: AccessibilitySnapshot
  ): List[AccessibilityAnnouncement] =
    val previousNodes = prior.map(_.nodes.map(node => node.id -> node).toMap).getOrElse(Map.empty)
    current.nodes.flatMap { node =>
      val focus =
        Option.when(node.focused && !previousNodes.get(node.id).exists(_.focused))(AccessibilityAnnouncement(node.name))
      val status = Option
        .when(
          node.role == AccessibilityRole.Status && !previousNodes
            .get(node.id)
            .flatMap(_.value)
            .contains(node.value.getOrElse(""))
        )(node.value.map(AccessibilityAnnouncement.apply))
        .flatten
      List(focus, status).flatten
    }
