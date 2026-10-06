package com.serenity.ui.terminal

import java.awt.event.{InputEvent, KeyEvent}
import java.nio.file.{Path, Paths}
import java.util.concurrent.ConcurrentLinkedQueue
import javax.accessibility.{AccessibleRole, AccessibleState}
import javax.swing.{JCheckBoxMenuItem, JMenu, JMenuBar, JMenuItem, JPanel, KeyStroke, SwingUtilities}

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.menu.{
  DynamicMenu,
  DynamicSource,
  MenuAccelerators,
  MenuDispatch,
  MenuModel,
  MenuSpec,
  MenuTitle,
  ResolvedEntry
}
import com.serenity.command.{CommandId, CommandRegistry}
import com.serenity.config.{AppConfig, AppMode, HotkeyAction, HotkeyConfig}
import com.serenity.input.{InputRouter, MenuActivationGuard, SwingInputHandler}
import com.serenity.keystroke.events.{ActivateBuffer, Event, OpenRecentPath, RunCommand}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.manager.Model
import com.serenity.state.models.{AppState, Buffer, BufferId, PaneId, Shell}
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, UndoState}
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Lightweight components only, so this runs without a display: no `JFrame` is built, and a menu opens by selecting its
  * model, which fires the same `menuSelected` a click does. Whether a real accelerator and the canvas key listener meet
  * the guard as intended is on the manual checklist in the pull request, because it needs a real window.
  */
class SwingMenuBarSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.withToggleUI
  private val linux    = HotkeyConfig.forOs("Linux")

  private def config(lineNumbers: Boolean = true, hotkeys: HotkeyConfig = linux): AppConfig =
    AppConfig.default.withAppMode(AppMode.Code).withLineNumbers(lineNumbers).withHotkeyConfig(hotkeys)

  private def modelOf(config: AppConfig, undo: UndoState = UndoState()): Model = Model(AppState.initial(config), undo)

  private def anEdit(app: AppState): HistoryEntry =
    val buffer = app.activeBuffer.getOrElse(fail("the initial state has no buffer"))
    HistoryEntry.BufferEdit(BufferId(0), PaneId(0), BufferSnapshot.fromBuffer(buffer))

  private def flushEdt(): Unit = SwingUtilities.invokeAndWait(() => ())

  private def label(commandName: String): String =
    registry.findCommand(commandName).getOrElse(fail(s"$commandName is not registered")).label

  final private class Fixture(initial: Model, readsModel: Boolean = true):
    val model  = Ref.unsafe[IO, Model](initial)
    val sent   = new ConcurrentLinkedQueue[Event]()
    val canvas = new JPanel

    val handler =
      new SwingInputHandler[IO, Event](
        canvas,
        InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync(),
        () => CellMetrics(8, 16, 13)
      )

    val sinceLastPress = new java.util.concurrent.atomic.AtomicLong(10L)

    val guard = new MenuActivationGuard(
      () => handler.lastPressed,
      sent.add(_): Unit,
      () => handler.lastPressed.map(_._2).getOrElse(0L) + sinceLastPress.get
    )

    val bar: JMenuBar =
      SwingMenuBar.build(
        SwingMenuBar.Host(model.get, io => if readsModel then io.unsafeRunSync(), guard),
        MenuSpec.forOs("Linux"),
        registry,
        Some(initial)
      )

    def menu(title: MenuTitle): JMenu = bar.getMenu(bar.getComponents.indexWhere(isMenuTitled(title)))

    def open(title: MenuTitle): JMenu =
      val opened = menu(title)
      opened.setSelected(false)
      opened.setSelected(true)
      flushEdt()
      opened

    def itemLabelled(title: MenuTitle, text: String): JMenuItem =
      items(open(title)).find(_.getText == text).getOrElse(fail(s"no '$text' item in ${title.text}"))

    def pressOnCanvas(stroke: KeyStroke): Unit =
      canvas.getKeyListeners.head.keyPressed(
        new KeyEvent(
          canvas,
          KeyEvent.KEY_PRESSED,
          System.currentTimeMillis,
          stroke.getModifiers,
          stroke.getKeyCode,
          0.toChar
        )
      )

    private def isMenuTitled(title: MenuTitle)(component: java.awt.Component): Boolean =
      component match
        case menu: JMenu => menu.getText == title.text
        case _           => false

  private def items(menu: JMenu): List[JMenuItem] =
    menu.getMenuComponents.toList.collect { case item: JMenuItem => item }

  "The menu bar" should "have one empty menu per top-level title until a menu is opened" in {
    val fixture = new Fixture(modelOf(config()))

    fixture.bar.getMenuCount shouldBe 5
    fixture.bar.getComponents.toList.collect { case menu: JMenu => menu.getText } shouldBe
      List("File", "Edit", "View", "Window", "Help")
    fixture.bar.getComponents.toList.collect { case menu: JMenu => menu.getMenuComponentCount }.sum shouldBe 0
  }

  it should "fill a menu from the menu model when it opens, with the registry's own labels" in {
    val fixture  = new Fixture(modelOf(config()))
    val resolved = MenuModel.resolve(MenuSpec.forOs("Linux"), registry, AppMode.Code, Shell.Gui)
    val expected =
      resolved
        .find(_.title == MenuTitle.File)
        .toList
        .flatMap(_.entries)
        .collect:
          case ResolvedEntry.Item(command)                      => command.label
          case ResolvedEntry.Submenu(title, _)                  => title.text
          case ResolvedEntry.Dynamic(DynamicSource.RecentFiles) => DynamicMenu.OpenRecentTitle

    items(fixture.open(MenuTitle.File)).map(_.getText) shouldBe expected
    expected should contain(label("save"))
  }

  it should "nest a submenu with its own items" in {
    val fixture = new Fixture(modelOf(config()))
    val session = items(fixture.open(MenuTitle.File)).collectFirst {
      case menu: JMenu if menu.getText == "Session" => menu
    }

    session.map(items(_).map(_.getText)) shouldBe Some(
      List("save-session", "save-session-as", "open-session", "rename-session", "restore-session", "clear-session")
        .map(label)
    )
  }

  it should "leave out what the current mode does not offer" in {
    val prose    = new Fixture(modelOf(config().withAppMode(AppMode.Prose)))
    val editMenu = items(prose.open(MenuTitle.Edit)).map(_.getText)

    editMenu should contain(label("cut-to-darlings"))
    items(new Fixture(modelOf(config())).open(MenuTitle.Edit)).map(_.getText) should not contain label(
      "cut-to-darlings"
    )
  }

  it should "not leave a separator first, last or doubled" in {
    val fixture = new Fixture(modelOf(config()))

    for title <- List(MenuTitle.File, MenuTitle.Edit, MenuTitle.View, MenuTitle.Window, MenuTitle.Help) do
      val kinds = fixture.open(title).getMenuComponents.toList.map(_.isInstanceOf[javax.swing.JPopupMenu.Separator])
      kinds.headOption should not be Some(true)
      kinds.lastOption should not be Some(true)
      kinds.zip(kinds.drop(1)).count((a, b) => a && b) shouldBe 0
  }

  it should "show its items the moment it first opens, with no model read to wait for" in {
    val fixture = new Fixture(modelOf(config()), readsModel = false)
    val file    = fixture.menu(MenuTitle.File)

    file.getMenuComponentCount shouldBe 0
    file.setSelected(true)

    items(file).map(_.getText) should contain(label("save"))
    items(file).find(_.getText == label("undo")) shouldBe None
    fixture.menu(MenuTitle.Edit).setSelected(true)
    items(fixture.menu(MenuTitle.Edit)).find(_.getText == label("undo")).map(_.isEnabled) shouldBe Some(false)
    Option(items(file).find(_.getText == label("save")).map(_.getAccelerator)).flatten should not be empty
  }

  it should "have its mnemonics settled from the seed without a model read" in {
    val bound   = linux.withBinding(HotkeyAction.Undo, "alt+f")
    val fixture = new Fixture(modelOf(config(hotkeys = bound)), readsModel = false)

    fixture.menu(MenuTitle.File).getMnemonic should not be KeyEvent.VK_F
    fixture.menu(MenuTitle.Edit).getMnemonic shouldBe KeyEvent.VK_E
  }

  "Menu items" should "show the keymap's accelerator" in {
    val fixture = new Fixture(modelOf(config()))
    val expected = for
      trigger <- MenuAccelerators.of(linux).get(CommandId("save"))
      stroke  <- MenuKeyStrokes.of(trigger)
    yield stroke

    expected should not be empty
    Option(fixture.itemLabelled(MenuTitle.File, label("save")).getAccelerator) shouldBe expected
    Option(fixture.itemLabelled(MenuTitle.Edit, label("redo")).getAccelerator) shouldBe
      Some(KeyStroke.getKeyStroke(KeyEvent.VK_Y, InputEvent.CTRL_DOWN_MASK))
  }

  it should "follow a rebound key the next time the menu opens" in {
    val fixture = new Fixture(modelOf(config()))
    val before  = Option(fixture.itemLabelled(MenuTitle.Edit, label("undo")).getAccelerator)

    fixture.model.set(modelOf(config(hotkeys = linux.withBinding(HotkeyAction.Undo, "ctrl+alt+u")))).unsafeRunSync()

    before shouldBe Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_DOWN_MASK))
    Option(fixture.itemLabelled(MenuTitle.Edit, label("undo")).getAccelerator) shouldBe
      Some(KeyStroke.getKeyStroke(KeyEvent.VK_U, InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK))
  }

  it should "show no accelerator for a command with no key" in {
    val fixture = new Fixture(modelOf(config()))
    val bound   = MenuAccelerators.of(linux).keySet
    val unbound =
      MenuModel
        .resolve(MenuSpec.forOs("Linux"), registry, AppMode.Code, Shell.Gui)
        .filter(_.title == MenuTitle.Edit)
        .flatMap(_.entries)
        .collect { case ResolvedEntry.Item(command) if !bound.contains(CommandId(command.name)) => command.label }

    unbound should not be empty
    for text <- unbound do Option(fixture.itemLabelled(MenuTitle.Edit, text).getAccelerator) shouldBe None
  }

  "Mnemonics" should "give each top-level menu the first letter of its title" in {
    val fixture = new Fixture(modelOf(config()))

    List(MenuTitle.File, MenuTitle.Edit, MenuTitle.View, MenuTitle.Window, MenuTitle.Help)
      .map(title => fixture.menu(title).getMnemonic) shouldBe
      List(KeyEvent.VK_F, KeyEvent.VK_E, KeyEvent.VK_V, KeyEvent.VK_W, KeyEvent.VK_H)
  }

  it should "move off a letter the keymap binds under Alt once the model is read" in {
    val bound   = linux.withBinding(HotkeyAction.Undo, "alt+f")
    val fixture = new Fixture(modelOf(config(hotkeys = bound)))
    flushEdt()

    fixture.menu(MenuTitle.File).getMnemonic should not be KeyEvent.VK_F
  }

  it should "be distinct within an opened menu" in {
    val fixture   = new Fixture(modelOf(config()))
    val mnemonics = items(fixture.open(MenuTitle.Edit)).map(_.getMnemonic).filter(_ != 0)

    mnemonics.distinct shouldBe mnemonics
    mnemonics should not be empty
  }

  "Item state" should "be read from the model each time a menu opens" in {
    val fixture = new Fixture(modelOf(config()))
    val undo    = label("undo")

    fixture.itemLabelled(MenuTitle.Edit, undo).isEnabled shouldBe false

    val app = AppState.initial(config())
    fixture.model.set(Model(app, UndoState().pushUndo(anEdit(app)))).unsafeRunSync()

    fixture.itemLabelled(MenuTitle.Edit, undo).isEnabled shouldBe true
  }

  it should "check a toggle that is on and clear one that is off" in {
    val fixture = new Fixture(modelOf(config(lineNumbers = true)))
    val toggle  = label("toggle-line-numbers")

    fixture.itemLabelled(MenuTitle.View, toggle) shouldBe a[JCheckBoxMenuItem]
    fixture.itemLabelled(MenuTitle.View, toggle).asInstanceOf[JCheckBoxMenuItem].isSelected shouldBe true

    fixture.model.set(modelOf(config(lineNumbers = false))).unsafeRunSync()

    fixture.itemLabelled(MenuTitle.View, toggle).asInstanceOf[JCheckBoxMenuItem].isSelected shouldBe false
  }

  it should "not make a plain command a check box" in {
    val fixture = new Fixture(modelOf(config()))

    fixture.itemLabelled(MenuTitle.File, label("save")) should not be a[JCheckBoxMenuItem]
  }

  "A click" should "send exactly the event MenuDispatch names for the command" in {
    val fixture = new Fixture(modelOf(config()))

    fixture.itemLabelled(MenuTitle.File, label("save")).doClick(0)

    fixture.sent.asScala.toList shouldBe List(MenuDispatch.eventFor(CommandId("save")))
  }

  it should "send once more for each further click" in {
    val fixture = new Fixture(modelOf(config()))
    val item    = fixture.itemLabelled(MenuTitle.File, label("save"))

    item.doClick(0)
    item.doClick(0)

    fixture.sent.size shouldBe 2
  }

  it should "send a command with no hotkey action as RunCommand" in {
    val fixture = new Fixture(modelOf(config()))

    fixture.itemLabelled(MenuTitle.File, label("new")).doClick(0)

    fixture.sent.asScala.toList shouldBe List(MenuDispatch.eventFor(CommandId("new")))
  }

  "An accelerator the canvas key listener already handled" should "not send a second event" in {
    val fixture = new Fixture(modelOf(config()))
    val save    = fixture.itemLabelled(MenuTitle.File, label("save"))

    fixture.pressOnCanvas(save.getAccelerator)
    save.doClick(0)

    fixture.sent.size shouldBe 0
  }

  it should "send again once the key press is old enough to be a different action" in {
    val fixture = new Fixture(modelOf(config()))
    val save    = fixture.itemLabelled(MenuTitle.File, label("save"))

    fixture.pressOnCanvas(save.getAccelerator)
    fixture.sinceLastPress.set(MenuActivationGuard.KeyPressWindowMillis + 400)
    save.doClick(0)

    fixture.sent.size shouldBe 1
  }

  it should "not suppress a different command's click" in {
    val fixture = new Fixture(modelOf(config()))

    fixture.pressOnCanvas(fixture.itemLabelled(MenuTitle.File, label("save")).getAccelerator)
    fixture.itemLabelled(MenuTitle.File, label("new")).doClick(0)

    fixture.sent.size shouldBe 1
  }

  "A key event" should "count as the keymap's keystroke only when it is the item's accelerator" in {
    val ctrlS = KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.CTRL_DOWN_MASK)
    def press(code: Int, modifiers: Int) =
      new KeyEvent(new JPanel, KeyEvent.KEY_PRESSED, 0L, modifiers, code, 0.toChar)

    SwingMenuBar.viaAcceleratorKey(Some(ctrlS), press(KeyEvent.VK_S, InputEvent.CTRL_DOWN_MASK)) shouldBe true
    SwingMenuBar.viaAcceleratorKey(Some(ctrlS), press(KeyEvent.VK_ENTER, 0)) shouldBe false
    SwingMenuBar.viaAcceleratorKey(None, press(KeyEvent.VK_S, InputEvent.CTRL_DOWN_MASK)) shouldBe false
    SwingMenuBar.viaAcceleratorKey(Some(ctrlS), null) shouldBe false
  }

  private def withRecent(model: Model, paths: List[Path]): Model =
    model.copy(app = model.app.copy(persisted = model.app.persisted.copy(recentFiles = paths)))

  private def withExtraBuffer(model: Model, id: BufferId, file: String, dirty: Boolean = false): Model =
    val base   = Buffer.fromString(id, "more")
    val buffer = base.copy(document = base.document.copy(filePath = Some(Paths.get(file)), isDirty = dirty))
    val added =
      model.app.copy(persisted = model.app.persisted.copy(buffers = model.app.persisted.buffers.updated(id, buffer)))
    model.copy(app = EditorState.insertBufferInOrder(added, id))

  private def openRecent(fixture: Fixture): JMenu =
    items(fixture.open(MenuTitle.File))
      .collectFirst { case menu: JMenu if menu.getText == "Open Recent" => menu }
      .getOrElse(fail("the File menu has no Open Recent submenu"))

  "The Open Recent submenu" should "list the recent files, most recent first, with the directory as description" in {
    val fixture = new Fixture(withRecent(modelOf(config()), List(Paths.get("/w/new.md"), Paths.get("/h/old.txt"))))
    val recent  = items(openRecent(fixture))

    recent.take(2).map(_.getText) shouldBe List("new.md", "old.txt")
    recent.take(2).map(_.getAccessibleContext.getAccessibleDescription) shouldBe List("/w", "/h")
    recent.take(2).map(_.getToolTipText) shouldBe List("/w", "/h")
    items(openRecent(fixture)).last.getText shouldBe label("clear-recent-files")
  }

  it should "show a disabled placeholder when there are no recent files" in {
    val recent = items(openRecent(new Fixture(modelOf(config()))))

    recent.map(_.getText) shouldBe List("(No recent files)")
    recent.map(_.isEnabled) shouldBe List(false)
  }

  it should "be rebuilt from the model each time the File menu opens" in {
    val fixture = new Fixture(modelOf(config()))

    items(openRecent(fixture)).map(_.getText) shouldBe List("(No recent files)")

    fixture.model.set(withRecent(modelOf(config()), List(Paths.get("/w/fresh.md")))).unsafeRunSync()

    items(openRecent(fixture)).map(_.getText).head shouldBe "fresh.md"

    fixture.model.set(modelOf(config())).unsafeRunSync()

    items(openRecent(fixture)).map(_.getText) shouldBe List("(No recent files)")
  }

  it should "show the seeded recent files at once, with no model read to wait for" in {
    val seeded  = withRecent(modelOf(config()), List(Paths.get("/w/seed.md")))
    val fixture = new Fixture(seeded, readsModel = false)

    items(openRecent(fixture)).map(_.getText).head shouldBe "seed.md"
  }

  it should "send exactly one OpenRecentPath when a file is chosen" in {
    val fixture = new Fixture(withRecent(modelOf(config()), List(Paths.get("/w/new.md"), Paths.get("/h/old.txt"))))

    items(openRecent(fixture)).find(_.getText == "old.txt").foreach(_.doClick(0))

    fixture.sent.asScala.toList shouldBe List(OpenRecentPath(Paths.get("/h/old.txt")))
  }

  it should "send RunCommand for Clear Recent Files" in {
    val fixture = new Fixture(withRecent(modelOf(config()), List(Paths.get("/w/new.md"))))

    items(openRecent(fixture)).find(_.getText == label("clear-recent-files")).foreach(_.doClick(0))

    fixture.sent.asScala.toList shouldBe List(RunCommand("clear-recent-files"))
  }

  "The Window menu" should "list the open buffers after its commands, checking the active one" in {
    val two     = withExtraBuffer(modelOf(config()), BufferId(7), "/p/seven.md", dirty = true)
    val fixture = new Fixture(two)
    val buffers = items(fixture.open(MenuTitle.Window)).filter(_.getText.matches("\\d .*"))

    buffers.map(_.getText.drop(2)) shouldBe List("Buffer 0", "seven.md ●")
    buffers.map(_.getMnemonic) shouldBe List(KeyEvent.VK_1, KeyEvent.VK_2)
    buffers.map(_.isSelected) shouldBe List(true, false)
    buffers.map(_.getToolTipText) shouldBe List(null, "/p/seven.md (unsaved changes)")
  }

  it should "rebuild its buffers each time it opens, as the model changes" in {
    val fixture = new Fixture(modelOf(config()))
    def buffers = items(fixture.open(MenuTitle.Window)).filter(_.getText.matches("\\d .*")).map(_.getText)

    buffers shouldBe List("1 Buffer 0")

    fixture.model.set(withExtraBuffer(modelOf(config()), BufferId(7), "/p/seven.md")).unsafeRunSync()

    buffers shouldBe List("1 Buffer 0", "2 seven.md")
  }

  it should "send exactly one ActivateBuffer when a buffer is chosen" in {
    val fixture = new Fixture(withExtraBuffer(modelOf(config()), BufferId(7), "/p/seven.md"))

    fixture.itemLabelled(MenuTitle.Window, "2 seven.md").doClick(0)

    fixture.sent.asScala.toList shouldBe List(ActivateBuffer(BufferId(7)))
  }

  it should "not leave a separator last when no buffer is open" in {
    val empty  = modelOf(config())
    val noTabs = empty.copy(app = empty.app.copy(persisted = empty.app.persisted.copy(bufferOrder = Nil)))
    val kinds = new Fixture(noTabs)
      .open(MenuTitle.Window)
      .getMenuComponents
      .toList
      .map(_.isInstanceOf[javax.swing.JPopupMenu.Separator])

    kinds.lastOption should not be Some(true)
  }

  "Accessibility" should "give the bar, menus and items their roles" in {
    val fixture = new Fixture(modelOf(config()))
    val toggle  = fixture.itemLabelled(MenuTitle.View, label("toggle-line-numbers"))
    val save    = fixture.itemLabelled(MenuTitle.File, label("save"))

    fixture.bar.getAccessibleContext.getAccessibleRole shouldBe AccessibleRole.MENU_BAR
    fixture.menu(MenuTitle.File).getAccessibleContext.getAccessibleRole shouldBe AccessibleRole.MENU
    save.getAccessibleContext.getAccessibleRole shouldBe AccessibleRole.MENU_ITEM
    toggle.getAccessibleContext.getAccessibleRole shouldBe AccessibleRole.CHECK_BOX
  }

  it should "report a checked toggle as checked state, and a disabled item as not enabled" in {
    val fixture = new Fixture(modelOf(config(lineNumbers = true)))
    val toggle  = fixture.itemLabelled(MenuTitle.View, label("toggle-line-numbers"))
    val undo    = fixture.itemLabelled(MenuTitle.Edit, label("undo"))

    toggle.getAccessibleContext.getAccessibleStateSet.contains(AccessibleState.CHECKED) shouldBe true
    undo.getAccessibleContext.getAccessibleStateSet.contains(AccessibleState.ENABLED) shouldBe false
  }

  it should "describe why an item is disabled" in {
    val fixture = new Fixture(modelOf(config()))
    val undo    = fixture.itemLabelled(MenuTitle.Edit, label("undo"))

    undo.getAccessibleContext.getAccessibleDescription should not be empty
  }

  it should "style a menu filled after the bar was themed, and restyle it when the theme changes" in {
    val fixture = new Fixture(modelOf(config()))
    val dark    = MenuBarPalette.fromTheme(Theme.dark)
    val light   = MenuBarPalette.fromTheme(Theme.light)
    SwingUtilities.invokeAndWait(() => MenuBarTheming.apply(fixture.bar, dark))

    val save = fixture.itemLabelled(MenuTitle.File, label("save"))
    save.getBackground shouldBe dark.popupBackground.toAwt
    save.getForeground shouldBe dark.popupForeground.toAwt

    SwingUtilities.invokeAndWait(() => MenuBarTheming.apply(fixture.bar, light))
    save.getBackground shouldBe light.popupBackground.toAwt
  }
