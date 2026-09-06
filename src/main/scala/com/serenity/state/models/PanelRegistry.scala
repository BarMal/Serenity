package com.serenity.state.models

/** Stable identity for a panel registered once with [[PanelRegistry]] (issue #1310), addressed by every display mode
  * that shows it (the command palette today; corner overlays and docking once those modes gain a registration-driven
  * entry point).
  */
opaque type PanelId = String

object PanelId:
  def apply(value: String): PanelId = value

  extension (id: PanelId) def value: String = id

/** A display mode a registered panel can be shown through (issue #1310). Shortcut-summoned (mode 2) is deliberately
  * absent until #1311's chord system exposes a `Command`-typed completion to register against -- adding a case nothing
  * can use yet would be exactly the kind of stub CLAUDE.md rules out.
  */
enum PanelDisplayMode:
  case Palette
  case Corner
  case Dock

/** Everything needed to show a registered panel through any [[PanelDisplayMode]] it declares support for.
  * `buildContent` defers to the panel's own existing [[SurfaceContent]] -- this framework does not introduce a new
  * content representation, only the register-once wiring around whichever content a feature already produces.
  */
final case class PanelRegistration(
    id: PanelId,
    label: String,
    description: String,
    buildContent: AppState => SurfaceContent,
    supportedModes: Set[PanelDisplayMode]
)

/** Panels registered once by feature code, consulted by every display mode instead of each hand-adding a case to
  * `ViewIntent`/`Command`/`GlobalAppEvent` per panel -- the cost #1307 paid twice for `TabList`/`RecentFilesInMode`
  * (ten hand-edited files for two near-identical panels). Mirrors `CommandRegistry`: a plain value built once from a
  * list of registrations, not a mutable store.
  */
final case class PanelRegistry(registrations: Map[PanelId, PanelRegistration]):

  def get(id: PanelId): Option[PanelRegistration] = registrations.get(id)

  def all: List[PanelRegistration] = registrations.values.toList

  def supporting(mode: PanelDisplayMode): List[PanelRegistration] = all.filter(_.supportedModes.contains(mode))

object PanelRegistry:

  def apply(registrations: List[PanelRegistration]): PanelRegistry =
    PanelRegistry(registrations.map(registration => registration.id -> registration).toMap)

  val empty: PanelRegistry = PanelRegistry(Map.empty[PanelId, PanelRegistration])
