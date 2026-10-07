package com.serenity.ui.terminal

import java.awt.Component
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JCheckBoxMenuItem, JMenu, JMenuBar, JMenuItem, JPopupMenu, SwingUtilities}

import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Lightweight components only, so this runs without a display: a menu opens by selecting its model, as in
  * SwingMenuBarSpec.
  */
class MenuBarThemingSpec extends AnyFlatSpec with Matchers:

  private val dark  = MenuBarPalette.fromTheme(Theme.dark)
  private val light = MenuBarPalette.fromTheme(Theme.light)

  final private class Fixture:
    val bar     = new JMenuBar
    val file    = new JMenu("File")
    val open    = new JMenuItem("Open")
    val toggle  = new JCheckBoxMenuItem("Line numbers")
    val recent  = new JMenu("Recent")
    val nested  = new JMenuItem("notes.md")
    val missing = new JMenuItem("Save")
    missing.setEnabled(false)
    file.add(open)
    file.addSeparator()
    file.add(toggle)
    file.add(missing)
    recent.add(nested)
    file.add(recent)
    bar.add(file)

  private def onEdt(body: => Unit): Unit = SwingUtilities.invokeAndWait(() => body)

  private def popupItems(menu: JMenu): List[Component] = menu.getPopupMenu.getComponents.toList

  "MenuBarTheming.apply" should "restyle the bar and every top-level menu" in {
    val f = Fixture()
    onEdt(MenuBarTheming.apply(f.bar, dark))
    f.bar.getBackground shouldBe dark.barBackground.toAwt
    f.file.getBackground shouldBe dark.barBackground.toAwt
    f.file.getForeground shouldBe dark.barForeground.toAwt
  }

  it should "restyle the items of a popup that is already open" in {
    val f = Fixture()
    onEdt(MenuBarTheming.apply(f.bar, dark))
    onEdt(f.file.setSelected(true))
    onEdt(MenuBarTheming.apply(f.bar, light))
    f.file.getPopupMenu.getBackground shouldBe light.popupBackground.toAwt
    f.open.getBackground shouldBe light.popupBackground.toAwt
    f.open.getForeground shouldBe light.popupForeground.toAwt
    f.toggle.getBackground shouldBe light.popupBackground.toAwt
    f.recent.getForeground shouldBe light.popupForeground.toAwt
    f.nested.getBackground shouldBe light.popupBackground.toAwt
  }

  it should "restyle separators and the popup border" in {
    val f = Fixture()
    onEdt(MenuBarTheming.apply(f.bar, dark))
    onEdt(MenuBarTheming.apply(f.bar, light))
    val separators = popupItems(f.file).collect { case s: JPopupMenu.Separator => s }
    separators should not be empty
    separators.foreach(_.getForeground shouldBe light.separator.toAwt)
    f.file.getPopupMenu.getBorder should not be null
  }

  it should "restyle items added to a menu after the bar was themed" in {
    val f    = Fixture()
    val late = new JMenuItem("Late")
    onEdt(MenuBarTheming.apply(f.bar, dark))
    onEdt:
      f.file.add(late)
      MenuBarTheming.applyTo(f.file)
    late.getBackground shouldBe dark.popupBackground.toAwt
  }

  it should "give the item and menu UIs their selection, hover, accelerator and disabled colours" in {
    val f      = Fixture()
    val colour = new AtomicReference(Option.empty[MenuBarTheming.UiColours])
    onEdt(MenuBarTheming.apply(f.bar, dark))
    val selection = (dark.selectionBackground.toAwt, dark.selectionForeground.toAwt)
    val hover     = (dark.hoverBackground.toAwt, dark.hoverForeground.toAwt)
    MenuBarTheming.uiColours(f.open).map(_.selection) shouldBe Some(selection)
    MenuBarTheming.uiColours(f.toggle).map(_.selection) shouldBe Some(selection)
    MenuBarTheming.uiColours(f.recent).map(_.selection) shouldBe Some(selection)
    MenuBarTheming.uiColours(f.file).map(_.selection) shouldBe Some(hover)
    colour.set(MenuBarTheming.uiColours(f.open))
    colour.get().map(_.accelerator) shouldBe Some(dark.acceleratorForeground.toAwt)
    colour.get().map(_.disabled) shouldBe Some(dark.disabledForeground.toAwt)
  }

  it should "replace the UI colours when the theme changes" in {
    val f = Fixture()
    onEdt(MenuBarTheming.apply(f.bar, dark))
    onEdt(MenuBarTheming.apply(f.bar, light))
    MenuBarTheming.uiColours(f.open).map(_.disabled) shouldBe Some(light.disabledForeground.toAwt)
  }
