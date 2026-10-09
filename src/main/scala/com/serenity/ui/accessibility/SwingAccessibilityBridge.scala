package com.serenity.ui.accessibility

import java.awt.Graphics
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.accessibility.{
  AccessibleContext,
  AccessibleEditableText,
  AccessibleState,
  AccessibleStateSet,
  AccessibleText
}
import javax.swing.{JButton, JComponent, JLabel, JPanel, JTextArea, JTextField}

import com.serenity.ui.layout.CellMetrics

/** Publishes canvas semantics as non-intercepting native Swing accessibility children.
  *
  * A document's text is not copied into its proxy on publish. The proxy keeps the document's `Rope` and fills the Swing
  * text model only once assistive technology first asks for the text (or the node's description), so a screen reader
  * that is not running costs no per-edit work proportional to the document. From that first read on, each publish
  * updates the text model eagerly, exactly as before, so the reader sees every change and its events.
  */
final class SwingAccessibilityBridge(canvas: JComponent):
  // Holds the last *published* snapshot purely for `describe`'s before/after text in `firePropertyChange`; the
  // focus/value diff itself lives only in `AccessibilityModel` (`AccessibilitySnapshot.announcements`) and is
  // relayed here, not recomputed.
  private val previous     = AtomicReference[Option[AccessibilitySnapshot]](None)
  private val materialized = AtomicReference[Option[(List[AccessibleNode], CellMetrics)]](None)
  private val proxies      = AtomicReference[List[JComponent & SemanticFocusProxy]](Nil)

  canvas.setLayout(null)

  /** Update native children while keeping the canvas as the sole input target. */
  def publish(snapshot: AccessibilitySnapshot, metrics: CellMetrics = CellMetrics(1, 1, 1)): Unit =
    val context          = canvas.getAccessibleContext
    val name             = snapshot.focused.map(_.name).getOrElse("Serenity editor")
    val description      = describe(snapshot)
    val priorDescription = previous.get.map(describe).orNull
    context.setAccessibleName(name)
    context.setAccessibleDescription(description)
    materialized.get match
      case Some((nodes, materializedMetrics)) if materializedMetrics == metrics && nodes == snapshot.nodes => ()
      case Some((nodes, materializedMetrics)) if materializedMetrics == metrics && sameLayout(nodes, snapshot.nodes) =>
        updateChildren(nodes, snapshot.nodes)
        materialized.set(Some((snapshot.nodes, metrics)))
      case _ =>
        replaceChildren(snapshot.nodes, metrics)
        materialized.set(Some((snapshot.nodes, metrics)))
    snapshot.announcements.foreach { announcement =>
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
      describeProxy(component, node)
      canvas.add(component)
      component
    }
    proxies.set(next)
    canvas.revalidate()
    canvas.repaint()

  private def sameLayout(materializedNodes: List[AccessibleNode], nodes: List[AccessibleNode]): Boolean =
    materializedNodes.sizeCompare(nodes) == 0 &&
      materializedNodes.lazyZip(nodes).forall { (before, after) =>
        before.id == after.id && before.role == after.role && before.bounds == after.bounds
      }

  /** Same nodes in the same places: refresh the existing proxies rather than rebuilding them, so a keystroke changing
    * the document's text costs no child removal, relayout or canvas repaint -- the proxies paint nothing.
    */
  private def updateChildren(materializedNodes: List[AccessibleNode], nodes: List[AccessibleNode]): Unit =
    proxies.get.lazyZip(materializedNodes).lazyZip(nodes).foreach { (component, before, after) =>
      val content = proxyContent(after)
      if content != proxyContent(before) then content.foreach(component.setProxyContent)
      describeProxy(component, after)
    }

  private def describeProxy(component: JComponent & SemanticFocusProxy, node: AccessibleNode): Unit =
    component.setSemanticFocused(node.focused)
    component.getAccessibleContext.setAccessibleName(node.name)
    component.setDescriptionSource(() => nodeDescription(node))

  private def proxyContent(node: AccessibleNode): Option[AccessibleValue] =
    node.role match
      case AccessibilityRole.Document | AccessibilityRole.TextField =>
        Some(node.value.getOrElse(AccessibleValue.Plain("")))
      case AccessibilityRole.Button | AccessibilityRole.Heading => Some(AccessibleValue.Plain(node.name))
      case AccessibilityRole.Status => Some(node.value.getOrElse(AccessibleValue.Plain(node.name)))
      case AccessibilityRole.Dialog | AccessibilityRole.Panel => None

  private def proxyFor(node: AccessibleNode): JComponent & SemanticFocusProxy =
    val component: JComponent & SemanticFocusProxy =
      node.role match
        case AccessibilityRole.Document                           => new TransparentTextArea
        case AccessibilityRole.Button                             => new TransparentButton
        case AccessibilityRole.TextField                          => new TransparentTextField
        case AccessibilityRole.Status | AccessibilityRole.Heading => new TransparentLabel
        case AccessibilityRole.Dialog | AccessibilityRole.Panel   => new TransparentPanel
    proxyContent(node).foreach(component.setProxyContent)
    component.setFocusable(false)
    component.setOpaque(false)
    component

  private def nodeDescription(node: AccessibleNode): String =
    val value = node.value.map(_.text).filter(_.nonEmpty).fold("")(current => s"; value=$current")
    s"id=${node.id}; role=${node.role.toString}; selected=${node.selected}; focused=${node.focused}$value"

  // A document's text stays out of the canvas description: it would have to be read on every publish, and the text
  // is what the document's own proxy exposes to assistive technology.
  private def describe(snapshot: AccessibilitySnapshot): String =
    snapshot.focused match
      case Some(node) =>
        val value = node.value
          .filter(_ => node.role != AccessibilityRole.Document)
          .map(_.text)
          .filter(_.nonEmpty)
          .fold("")(current => s": $current")
        s"${node.role.toString.toLowerCase} ${node.name}$value"
      case None => "Canvas-rendered Serenity editor"

  private trait SemanticFocusProxy:
    this: JComponent =>

    private val semanticallyFocused = AtomicBoolean(false)

    final def setSemanticFocused(focused: Boolean): Unit = semanticallyFocused.set(focused)

    def setProxyContent(content: AccessibleValue): Unit

    def setDescriptionSource(description: () => String): Unit =
      getAccessibleContext.setAccessibleDescription(description())

    final protected def withSemanticFocus(states: AccessibleStateSet): AccessibleStateSet =
      if semanticallyFocused.get then
        states.add(AccessibleState.FOCUSED)
        ()
      states

  private class TransparentPanel extends JPanel with SemanticFocusProxy:

    def setProxyContent(content: AccessibleValue): Unit = ()

    override def getAccessibleContext: AccessibleContext =
      if accessibleContext == null then
        accessibleContext = new AccessibleJPanel:
          override def getAccessibleStateSet: AccessibleStateSet =
            TransparentPanel.this.withSemanticFocus(super.getAccessibleStateSet)
      accessibleContext

    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()

  private class TransparentLabel extends JLabel with SemanticFocusProxy:

    def setProxyContent(content: AccessibleValue): Unit = setText(content.text)

    override def getAccessibleContext: AccessibleContext =
      if accessibleContext == null then
        accessibleContext = new AccessibleJLabel:
          override def getAccessibleStateSet: AccessibleStateSet =
            TransparentLabel.this.withSemanticFocus(super.getAccessibleStateSet)
      accessibleContext

    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()

  private class TransparentTextArea extends JTextArea with SemanticFocusProxy:
    private val unread            = AtomicReference[Option[AccessibleValue]](None)
    private val beingRead         = AtomicBoolean(false)
    private val descriptionSource = AtomicReference[() => String](() => "")

    def setProxyContent(content: AccessibleValue): Unit =
      if beingRead.get then
        unread.set(None)
        setText(content.text)
      else unread.set(Some(content))

    override def setDescriptionSource(source: () => String): Unit = descriptionSource.set(source)

    private def fillTextModel(): Unit =
      unread.getAndSet(None).foreach(content => setText(content.text))

    private def startBeingRead(): Unit =
      beingRead.set(true)
      fillTextModel()

    override def getText(): String =
      fillTextModel()
      super.getText()

    override def getText(offs: Int, len: Int): String =
      fillTextModel()
      super.getText(offs, len)

    override def getAccessibleContext: AccessibleContext =
      if accessibleContext == null then
        accessibleContext = new AccessibleJTextArea:
          override def getAccessibleStateSet: AccessibleStateSet =
            TransparentTextArea.this.withSemanticFocus(super.getAccessibleStateSet)

          override def getAccessibleDescription: String = descriptionSource.get.apply()

          override def getAccessibleText: AccessibleText =
            startBeingRead()
            super.getAccessibleText

          override def getAccessibleEditableText: AccessibleEditableText =
            startBeingRead()
            super.getAccessibleEditableText
      accessibleContext

    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()

  private class TransparentTextField extends JTextField with SemanticFocusProxy:

    def setProxyContent(content: AccessibleValue): Unit = setText(content.text)

    override def getAccessibleContext: AccessibleContext =
      if accessibleContext == null then
        accessibleContext = new AccessibleJTextField:
          override def getAccessibleStateSet: AccessibleStateSet =
            TransparentTextField.this.withSemanticFocus(super.getAccessibleStateSet)
      accessibleContext

    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()

  private class TransparentButton extends JButton with SemanticFocusProxy:

    def setProxyContent(content: AccessibleValue): Unit = setText(content.text)

    override def getAccessibleContext: AccessibleContext =
      if accessibleContext == null then
        accessibleContext = new AccessibleJButton:
          override def getAccessibleStateSet: AccessibleStateSet =
            TransparentButton.this.withSemanticFocus(super.getAccessibleStateSet)
      accessibleContext

    override def contains(x: Int, y: Int): Boolean                  = false
    override protected def paintComponent(graphics: Graphics): Unit = ()
    override protected def paintBorder(graphics: Graphics): Unit    = ()
