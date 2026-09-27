package com.serenity.frontend

/** Whether the current [[Frontend]] can offer a spawned Markdown preview window at all (issue #1113's TUI-only
  * feature). Lives here rather than in `com.serenity.ui.tui` -- where the real `MarkdownPreviewWindow` implementation
  * and its `.resource` builder still do -- because `state.manager` (`StateManagerPanelEffects` and the `StateManager`
  * constructor parameters that reach it) needs to hold a value of this type without importing a concrete frontend's own
  * implementation package, which `ArchitectureChecks`' `com/serenity/state` rule (issue #1669) forbids. Moving just
  * this ADT out of `ui.tui` is the fix the `architecture-baseline.tsv` entries it used to require (grandfathered by
  * #1739) called for, closing that follow-up debt.
  *
  * `GuiFrontend` always reports [[Unavailable]] -- the GUI's Markdown preview is the in-app split panel
  * (`StateManagerPanelEffects.setPanelPin`), never a spawned window. `TuiFrontend` carries whatever `TuiRuntime.run`
  * resolved from `LaunchOptions.isDisplayReachable` at startup.
  */
sealed trait MarkdownPreviewWindowAvailability

object MarkdownPreviewWindowAvailability:
  case object Unavailable extends MarkdownPreviewWindowAvailability
  final case class Available(window: com.serenity.ui.tui.MarkdownPreviewWindow)
      extends MarkdownPreviewWindowAvailability
