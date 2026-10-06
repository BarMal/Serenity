package com.serenity.ui.terminal

import java.awt.event.KeyEvent
import java.awt.{AWTEvent, EventQueue}
import java.util.Locale
import java.util.concurrent.atomic.AtomicReference
import javax.swing.event.{MenuEvent, MenuListener}
import javax.swing.{JCheckBoxMenuItem, JMenu, JMenuBar, JMenuItem, KeyStroke, SwingUtilities}

import cats.effect.{IO, Resource}
import com.serenity.command.menu.{
  CommandToggleState,
  MenuAccelerators,
  MenuDispatch,
  MenuEntry,
  MenuItemState,
  MenuMnemonics,
  MenuModel,
  MenuSpec,
  MenuTitle,
  ResolvedEntry
}
import com.serenity.command.{Command, CommandId, CommandRegistry}
import com.serenity.config.{AppMode, HotkeyConfig}
import com.serenity.frontend.FrontendRuntime
import com.serenity.input.{MenuActivationGuard, SwingInputHandler}
import com.serenity.keystroke.events.Event
import com.serenity.state.manager.Model
import com.serenity.state.models.Shell

/** The menu bar of the Swing window on Windows and Linux. The bar starts with one empty menu per title: a menu is
  * filled the first time it opens, and every time it opens its items take their enabled and checked state from the
  * model as it is then, which is read off the event-dispatch thread and applied back on it.
  */
object SwingMenuBar:

  /** `guard` is where a keymap-handled accelerator is told from a click, and where an activation is finally sent. */
  final case class Host(readModel: IO[Model], runAsync: IO[Unit] => Unit, guard: MenuActivationGuard)

  /** macOS has its own screen menu bar and application menu, which are not built here yet. */
  def enabledFor(osName: String): Boolean =
    !osName.toLowerCase(Locale.ROOT).contains("mac")

  /** `seed` is the model as it was when the bar was built. It lets a menu fill the moment it opens, so none is ever
    * shown empty; the read each open makes afterwards only brings it up to date.
    */
  def build(host: Host, spec: MenuSpec, registry: CommandRegistry, seed: Option[Model] = None): JMenuBar =
    val bar    = new JMenuBar
    val latest = new Snapshot(seed)
    val menus  = spec.menus.map((title, entries) => new LazyMenu(title, entries, Env(host, registry, latest)))
    menus.foreach(menu => bar.add(menu.menu))
    applyTopLevelMnemonics(menus.map(_.menu), seed.fold(Set.empty[Char])(model => altLettersOf(model)))
    host.runAsync(
      host.readModel.flatMap(model =>
        IO(
          SwingUtilities.invokeLater { () =>
            latest.set(Some(model))
            applyTopLevelMnemonics(menus.map(_.menu), altLettersOf(model))
          }
        )
      )
    )
    bar

  /** Installs the bar in the window once the application is running. The input handler arrives through `handler`
    * because the runtime builds it after this hook is chosen.
    */
  def resource(
    window: SwingWindow,
    handler: IO[SwingInputHandler[IO, Event]]
  ): FrontendRuntime.MenuHost => Resource[IO, Unit] =
    menuHost =>
      Resource.eval(
        handler.flatMap: input =>
          val guard = new MenuActivationGuard(() => input.lastPressed, input.submit(_))
          val host  = Host(menuHost.readModel, menuHost.runAsync, guard)
          menuHost.readModel.flatMap(seed =>
            IO.blocking(
              SwingUtilities.invokeAndWait(() =>
                window.installMenuBar(build(host, MenuSpec.forOs(osName), CommandRegistry.withToggleUI, Some(seed)))
              )
            )
          )
      )

  /** True when `current` is the very key press of the item's accelerator. An activation by another key, such as Enter
    * on a highlighted item, is not the keymap's key, even if that key was pressed on the canvas earlier.
    */
  private[terminal] def viaAcceleratorKey(accelerator: Option[KeyStroke], current: AWTEvent): Boolean =
    current match
      case key: KeyEvent => accelerator.contains(KeyStroke.getKeyStrokeForEvent(key))
      case _             => false

  private def osName: String = System.getProperty("os.name", "")

  private def altLettersOf(model: Model): Set[Char] = MenuMnemonics.altLetters(hotkeysOf(model))

  private def hotkeysOf(model: Model): HotkeyConfig = model.app.persisted.config.inputConfig.hotkeyConfig

  private def applyTopLevelMnemonics(menus: List[JMenu], avoid: Set[Char]): Unit =
    menus
      .zip(MenuMnemonics.assign(menus.map(_.getText), avoid))
      .foreach((menu, mnemonic) => applyMnemonic(menu, mnemonic))

  private def applyMnemonic(item: JMenuItem, mnemonic: Option[MenuMnemonics.Mnemonic]): Unit =
    mnemonic match
      case Some(found) =>
        item.setMnemonic(found.char.toUpper)
        item.setDisplayedMnemonicIndex(found.index)
      case None => item.setMnemonic(0)

  final private class Snapshot(seed: Option[Model]):
    private val held                    = new AtomicReference[Option[Model]](seed)
    def get(): Option[Model]            = held.get()
    def set(model: Option[Model]): Unit = held.set(model)

  final private case class Env(host: Host, registry: CommandRegistry, latest: Snapshot)

  final private case class Row(item: JMenuItem, command: Command)

  final private case class Populated(
      mode: AppMode,
      shell: Shell,
      rows: List[Row],
      hotkeys: Option[HotkeyConfig]
  )

  sealed private trait Piece
  final private case class ItemPiece(row: Row)                        extends Piece
  final private case class SubmenuPiece(menu: JMenu, rows: List[Row]) extends Piece
  private case object SeparatorPiece                                  extends Piece

  /** One top-level menu. Only the event-dispatch thread touches it, so `populated` is a holder, not a lock. */
  final private class LazyMenu(title: MenuTitle, entries: List[MenuEntry], env: Env):
    val menu = new JMenu(title.text)

    private val populated = new AtomicReference[Option[Populated]](None)

    menu.addMenuListener(new MenuListener:
      override def menuSelected(e: MenuEvent): Unit =
        env.latest.get().foreach(show)
        env.host.runAsync(
          env.host.readModel.flatMap(model =>
            IO(
              SwingUtilities.invokeLater { () =>
                env.latest.set(Some(model))
                show(model)
              }
            )
          )
        )
      override def menuDeselected(e: MenuEvent): Unit = ()
      override def menuCanceled(e: MenuEvent): Unit   = ())

    private def show(model: Model): Unit =
      val context = model.app.editingContext
      val current =
        populated.get().filter(p => p.mode == context.mode && p.shell == context.shell).getOrElse(rebuild(model))
      val refreshed = refreshAccelerators(current, hotkeysOf(model))
      refreshed.rows.foreach(row => refreshState(row, model))
      populated.set(Some(refreshed))
      if menu.isPopupMenuVisible then menu.getPopupMenu.pack()

    private def rebuild(model: Model): Populated =
      val context  = model.app.editingContext
      val resolved = MenuModel.resolve(MenuSpec(List(title -> entries)), env.registry, context.mode, context.shell)
      menu.removeAll()
      val rows = fill(menu, resolved.flatMap(_.entries), model)
      Populated(context.mode, context.shell, rows, None)

    private def fill(target: JMenu, entries: List[ResolvedEntry], model: Model): List[Row] =
      val pieces    = tidy(entries.flatMap(piece(_, model)))
      val labels    = pieces.flatMap(labelOf)
      val mnemonics = labels.zip(MenuMnemonics.assign(labels, Set.empty)).toMap
      pieces.flatMap:
        case ItemPiece(row) =>
          applyMnemonic(row.item, mnemonics.get(row.item.getText).flatten)
          val _ = target.add(row.item)
          List(row)
        case SubmenuPiece(submenu, rows) =>
          applyMnemonic(submenu, mnemonics.get(submenu.getText).flatten)
          val _ = target.add(submenu)
          rows
        case SeparatorPiece =>
          target.addSeparator()
          Nil

    private def labelOf(piece: Piece): Option[String] =
      piece match
        case ItemPiece(row)           => Some(row.item.getText)
        case SubmenuPiece(submenu, _) => Some(submenu.getText)
        case SeparatorPiece           => None

    private def piece(entry: ResolvedEntry, model: Model): List[Piece] =
      entry match
        case ResolvedEntry.Item(command) => List(ItemPiece(Row(itemFor(command, model), command)))
        case ResolvedEntry.Submenu(subtitle, children) =>
          val submenu = new JMenu(subtitle.text)
          List(SubmenuPiece(submenu, fill(submenu, children, model)))
        case ResolvedEntry.Separator                              => List(SeparatorPiece)
        case ResolvedEntry.Dynamic(_) | ResolvedEntry.Platform(_) => Nil

    private def itemFor(command: Command, model: Model): JMenuItem =
      val item =
        if CommandToggleState.of(command.intent, model.app).isDefined then new JCheckBoxMenuItem(command.label)
        else new JMenuItem(command.label)
      item.addActionListener(_ => activate(item, command))
      item

    private def activate(item: JMenuItem, command: Command): Unit =
      val accelerator = Option(item.getAccelerator)
      env.host.guard.activate(
        MenuDispatch.eventFor(CommandId(command.name)),
        accelerator,
        viaAcceleratorKey(accelerator, EventQueue.getCurrentEvent)
      )

    private def refreshAccelerators(current: Populated, hotkeys: HotkeyConfig): Populated =
      if current.hotkeys.exists(_ eq hotkeys) then current
      else
        val strokes = MenuAccelerators.of(hotkeys).flatMap((id, trigger) => MenuKeyStrokes.of(trigger).map(id -> _))
        current.rows.foreach(row => row.item.setAccelerator(strokes.get(CommandId(row.command.name)).orNull))
        current.copy(hotkeys = Some(hotkeys))

    private def refreshState(row: Row, model: Model): Unit =
      val state = MenuItemState.of(row.command, model.app, model.undo.canUndo, model.undo.canRedo)
      row.item.setEnabled(state.enabled)
      row.item.getAccessibleContext.setAccessibleDescription(state.disabledReason.orNull)
      row.item match
        case toggle: JCheckBoxMenuItem => state.checked.foreach(toggle.setSelected)
        case _                         => ()

    private def tidy(pieces: List[Piece]): List[Piece] =
      val collapsed = pieces.foldLeft(List.empty[Piece]): (kept, next) =>
        (kept, next) match
          case (Nil, SeparatorPiece) | (SeparatorPiece :: _, SeparatorPiece) => kept
          case _                                                             => next :: kept
      collapsed.dropWhile(_ == SeparatorPiece).reverse
