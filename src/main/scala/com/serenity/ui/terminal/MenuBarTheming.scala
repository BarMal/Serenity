package com.serenity.ui.terminal

import java.awt.{Color, Component}
import javax.swing.plaf.basic.{BasicCheckBoxMenuItemUI, BasicMenuItemUI, BasicMenuUI}
import javax.swing.{BorderFactory, JCheckBoxMenuItem, JComponent, JMenu, JMenuBar, JMenuItem, JPopupMenu, JSeparator}

/** Applies a [[MenuBarPalette]] to a menu bar and everything under it, component by component. `UIManager` is left
  * alone on purpose: its defaults would also restyle the file chooser. Everything here runs on the event-dispatch
  * thread, and applying is idempotent, so a theme change is just another call with the new palette.
  *
  * Background and foreground are plain component setters. The look and feel reads the selected, disabled and
  * accelerator colours from its own `UIManager` defaults when a delegate is installed, so those go in by installing a
  * delegate that overrides them. Opening a popup does not reinstall delegates, so a popup that is already open is
  * restyled by the same call.
  */
object MenuBarTheming:

  /** `selection` is the background and foreground an item takes while highlighted. */
  final case class UiColours(selection: (Color, Color), disabled: Color, accelerator: Color)

  private val PaletteKey = "serenity.menuBarPalette"

  /** The colours of the delegate installed on `item`, if one of ours is. */
  def uiColours(item: JMenuItem): Option[UiColours] =
    item.getUI match
      case themed: ThemedUi => Some(themed.colours)
      case _                => None

  def apply(bar: JMenuBar, palette: MenuBarPalette): Unit =
    bar.setOpaque(true)
    bar.setBackground(palette.barBackground.toAwt)
    bar.setForeground(palette.barForeground.toAwt)
    bar.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, palette.border.toAwt))
    bar.putClientProperty(PaletteKey, palette)
    menusOf(bar).foreach: menu =>
      menu.putClientProperty(PaletteKey, palette)
      applyTo(menu)
    bar.repaint()

  /** Styles `menu`, a top-level menu of a bar already themed by [[apply]], with its popup, for items added since. */
  def applyTo(menu: JMenu): Unit =
    paletteOf(menu).foreach: palette =>
      styleMenu(menu, palette, topLevel = true)

  private def paletteOf(menu: JMenu): Option[MenuBarPalette] =
    Option(menu.getClientProperty(PaletteKey)).collect { case palette: MenuBarPalette => palette }

  private def menusOf(bar: JMenuBar): List[JMenu] =
    bar.getComponents.toList.collect { case menu: JMenu => menu }

  private def styleMenu(menu: JMenu, palette: MenuBarPalette, topLevel: Boolean): Unit =
    val (background, foreground) =
      if topLevel then (palette.barBackground, palette.barForeground)
      else (palette.popupBackground, palette.popupForeground)
    paint(menu, background.toAwt, foreground.toAwt)
    menu.setUI(new ThemedMenuUi(coloursOf(palette, topLevel)))
    stylePopup(menu.getPopupMenu, palette)

  private def stylePopup(popup: JPopupMenu, palette: MenuBarPalette): Unit =
    paint(popup, palette.popupBackground.toAwt, palette.popupForeground.toAwt)
    popup.setBorder(BorderFactory.createLineBorder(palette.border.toAwt))
    popup.getComponents.foreach(styleEntry(_, palette))
    popup.revalidate()
    popup.repaint()

  private def styleEntry(entry: Component, palette: MenuBarPalette): Unit =
    entry match
      case submenu: JMenu =>
        styleMenu(submenu, palette, topLevel = false)
      case check: JCheckBoxMenuItem =>
        paint(check, palette.popupBackground.toAwt, palette.popupForeground.toAwt)
        check.setUI(new ThemedCheckBoxUi(coloursOf(palette, topLevel = false)))
      case item: JMenuItem =>
        paint(item, palette.popupBackground.toAwt, palette.popupForeground.toAwt)
        item.setUI(new ThemedItemUi(coloursOf(palette, topLevel = false)))
      case separator: JSeparator =>
        paint(separator, palette.popupBackground.toAwt, palette.separator.toAwt)
      case _ => ()

  private def paint(component: JComponent, background: Color, foreground: Color): Unit =
    component.setOpaque(true)
    component.setBackground(background)
    component.setForeground(foreground)

  /** A top-level menu is lit by hover, a popup item by selection: the bar stays quieter than the list under it. */
  private def coloursOf(palette: MenuBarPalette, topLevel: Boolean): UiColours =
    val lit =
      if topLevel then (palette.hoverBackground.toAwt, palette.hoverForeground.toAwt)
      else (palette.selectionBackground.toAwt, palette.selectionForeground.toAwt)
    UiColours(lit, palette.disabledForeground.toAwt, palette.acceleratorForeground.toAwt)

  sealed private trait ThemedUi:
    def colours: UiColours

  final private class ThemedItemUi(val colours: UiColours) extends BasicMenuItemUI with ThemedUi:

    override protected def installDefaults(): Unit =
      super.installDefaults()
      selectionBackground = colours.selection._1
      selectionForeground = colours.selection._2
      disabledForeground = colours.disabled
      acceleratorForeground = colours.accelerator
      acceleratorSelectionForeground = colours.selection._2

  final private class ThemedCheckBoxUi(val colours: UiColours) extends BasicCheckBoxMenuItemUI with ThemedUi:

    override protected def installDefaults(): Unit =
      super.installDefaults()
      selectionBackground = colours.selection._1
      selectionForeground = colours.selection._2
      disabledForeground = colours.disabled
      acceleratorForeground = colours.accelerator
      acceleratorSelectionForeground = colours.selection._2

  final private class ThemedMenuUi(val colours: UiColours) extends BasicMenuUI with ThemedUi:

    override protected def installDefaults(): Unit =
      super.installDefaults()
      selectionBackground = colours.selection._1
      selectionForeground = colours.selection._2
      disabledForeground = colours.disabled
      acceleratorForeground = colours.accelerator
      acceleratorSelectionForeground = colours.selection._2
