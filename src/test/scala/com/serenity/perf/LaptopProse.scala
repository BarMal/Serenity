package com.serenity.perf

import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize

/** The state every `laptop.*` benchmark starts from: `text` in one buffer, sized to the laptop grid and under the app's
  * own default config, with the cursor well below the first screenful.
  */
private[perf] object LaptopProse:

  def stateOf(text: String, viewport: ViewportSize, bufferId: BufferId): AppState =
    val base   = BenchmarkFixtures.editorState(text, None)
    val buffer = base.persisted.buffers(bufferId)
    val sized = buffer.copy(
      viewport = buffer.viewport.copy(visibleColumns = viewport.width, visibleLines = viewport.height),
      editing = EditingState(List(CursorPosition(150, 40)))
    )
    base.copy(
      persisted = base.persisted.copy(
        config = AppConfig.conventional,
        buffers = Map(bufferId -> sized),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = base.runtime.copy(viewportSize = Some(viewport))
    )
