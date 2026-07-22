package com.serenity.ui.accessibility

import java.awt.Graphics
import java.util.concurrent.atomic.AtomicReference
import javax.accessibility.AccessibleContext
import javax.swing.{JComponent, JLabel, JPanel, JTextArea, JTextField, JToggleButton}

import com.serenity.ui.layout.CellMetrics

/** Publishes canvas semantics as non-intercepting native Swing accessibility children. */
final class SwingAccessibilityBridge(canvas: JComponent):
  private val previous = AtomicReference[Option[AccessibilitySnapshot]](None)
  private val proxies  = AtomicReference[List[JComponent]](Nil)

  canvas.setLayout(null)

  /** Update native children while keeping the canvas as the sole input target. */
  def publish(snapshot: AccessibilitySnapshot, metrics: CellMetrics = CellMetrics(1, 1, 1)): Unit =
    val context          = canvas.getAccessibleContext
    val name             = snapshot.focused.map(_.name).getOrElse("Serenity editor")
    val description      = describe(snapshot)
    val priorDescription = previous.get.map(describe).orNull
    context.setAccessibleName(name)
    context.setAccessibleDescription(description)
    replaceChildren(snapshot.nodes, metrics)
    announcements(previous.get, snapshot).foreach { announcement =>
      context.firePropertyChange(
        AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY,
        priorDescription,
        announcement.message
      )
    }
    previous.set(Some(snapshot))

  private def replaceChildren(nodes: List[AccessibleNode], metrics: CellMetrics): Unit =
    proxies.getAndSet(Nil).foreach(canvas.remove)
    val next = nodes.map { node =>
      val component = proxyFor(node)
      component.setName(node.id)
      component.setBounds(
        node.bounds.x * metrics.charWidth,
        node.bounds.y * metrics.lineHeight,
        node.bounds.width * metrics.charWidth,
        node.bounds.height * metrics.lineHeight
      )
      component.getAccessibleContext.setAccessibleName(node.name)
      component.getAccessibleContext.setAccessibleDescription(nodeDescription(node))
      canvas.add(component)
      component
    }
    proxies.set(next)
    canvas.revalidate()
    canvas.repaint()

  private def proxyFor(node: AccessibleNode): JComponent =
    val component: JComponent =
      node.role match
        case AccessibilityRole.Document =>
          val document = new TransparentTextArea
          document.setText(node.value.getOrElse(""))
          document
        case AccessibilityRole.Button =>
          val button = new TransparentToggleButton
          button.setText(node.name)
          button.setSelected(node.selected)
          button
        case AccessibilityRole.TextField =>
          val field = new TransparentTextField
          field.setText(node.value.getOrElse(""))
          field
        case AccessibilityRole.Status =>
          val status = new TransparentLabel
          status.setText(node.value.getOrElse(node.name))
          status
        case AccessibilityRole.Dialog | AccessibilityRole.Panel => new TransparentPanel
    component.setFocusable(false)
    component.setOpaque(false)
    component

  private def nodeDescription(node: AccessibleNode): String =
    val value = node.value.filter(_.nonEmpty).fold("")(current => s"; value=$current")
    s"id=${node.id}; role=${node.role.toString}; selected=${node.selected}; focused=${node.focused}$value"

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

  private class TransparentPanel extends JPanel:
    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()

  private class TransparentLabel extends JLabel:
    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()

  private class TransparentTextArea extends JTextArea:
    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()

  private class TransparentTextField extends JTextField:
    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()

  private class TransparentToggleButton extends JToggleButton:
    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()
