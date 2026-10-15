package com.serenity.config

import com.typesafe.config.ConfigValue

/** The chrome each [[UiLayout]] starts from.
  *
  * A layout is a base, not an override: the config file is read over [[base]], so a key the user writes wins. Changing
  * the layout of a running config does the same through [[rebase]], moving only the chrome that was still the old
  * layout's own.
  */
object LayoutPreset:

  /** One piece of chrome a layout decides, and the value it has under each layout. */
  final private case class Element[A](
      key: String,
      legacyKeys: Set[String],
      read: AppConfig => A,
      write: (AppConfig, A) => AppConfig,
      minimal: A,
      conventional: A
  ):

    def valueIn(layout: UiLayout): A =
      layout match
        case UiLayout.Minimal      => minimal
        case UiLayout.Conventional => conventional

    def setTo(config: AppConfig, layout: UiLayout): AppConfig = write(config, valueIn(layout))

    def isLayoutsOwn(config: AppConfig): Boolean = read(config) == valueIn(config.uiLayout)

    def spellings: Set[String] = ConfigRegistry.find(key).fold(Set(key))(_.spellings) ++ legacyKeys

  private val elements: List[Element[?]] = List(
    Element[Boolean](
      "editor.line_numbers",
      Set.empty,
      _.surfaceConfig.showLineNumbers,
      (config, value) => config.withLineNumbers(value),
      minimal = false,
      conventional = true
    ),
    Element[Boolean](
      "editor.pane_headers",
      Set.empty,
      _.surfaceConfig.showPaneHeaders,
      (config, value) => config.withPaneHeaders(value),
      minimal = false,
      conventional = true
    ),
    Element[StatusLinePlacement](
      "status.placement",
      LegacyStatusLineKeys.Keys,
      _.statusLine.placement,
      (config, value) => config.withStatusLinePlacement(value),
      minimal = StatusLinePlacement.Off,
      conventional = StatusLinePlacement.Pinned
    ),
    Element[TabStripMode](
      "editor.tab_strip",
      Set.empty,
      _.surfaceConfig.tabStrip,
      (config, value) => config.withTabStrip(value),
      minimal = TabStripMode.Off,
      conventional = TabStripMode.Auto
    ),
    Element[Boolean](
      "editor.scrollbar",
      Set.empty,
      _.surfaceConfig.scrollbarEnabled,
      (config, value) => config.withScrollbar(value),
      minimal = false,
      conventional = true
    )
  )

  /** Every setting at its default, with the chrome of `layout`. */
  def base(layout: UiLayout): AppConfig =
    elements.foldLeft(AppConfig().withUiLayout(layout))((config, element) => element.setTo(config, layout))

  /** `config` moved onto `layout`: each piece of chrome still at its current layout's value takes the new layout's.
    *
    * `explicitKeys` are the config keys the user's file sets; chrome they name is the user's and stays, even when it
    * happens to match the layout being left. Chrome that differs from the current layout's value was changed in this
    * session and stays too.
    */
  def rebase(config: AppConfig, layout: UiLayout, explicitKeys: Set[String]): AppConfig =
    elements.foldLeft(config.withUiLayout(layout)) { (rebased, element) =>
      val keeps = element.spellings.exists(explicitKeys.contains) || !element.isLayoutsOwn(config)
      if keeps then rebased else element.setTo(rebased, layout)
    }

  /** The keys whose value in `config` is the layout's own, so a file that never mentions them says the same. `setKeys`
    * are the keys the file does set, under any spelling: those are the user's even when they match the layout.
    */
  def inheritedKeys(config: AppConfig, setKeys: Set[String] = Set.empty): Set[String] =
    elements
      .filter(element => element.isLayoutsOwn(config) && !element.spellings.exists(setKeys.contains))
      .map(_.key)
      .toSet

  /** The layout a config file asks for, minimal when it asks for none or one that does not exist. */
  def chosenBy(entries: Iterable[(String, ConfigValue)]): UiLayout =
    entries
      .collectFirst { case (key, value) if ConfigRegistry.find(key).exists(_.key == "ui.layout") => key -> value }
      .flatMap((key, value) => ConfigRegistry.find(key).flatMap(_.readValue(AppConfig.default, value)))
      .fold(UiLayout.Minimal)(_.uiLayout)

  /** Every spelling the config file can set the chrome setting `key` with. */
  def chromeKeys(key: String): Set[String] =
    elements.find(_.key == key).fold(Set.empty[String])(_.spellings)
