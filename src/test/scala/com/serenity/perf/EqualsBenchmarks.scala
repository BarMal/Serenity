package com.serenity.perf

import com.serenity.perf.BenchmarkFixtures.{editorState, largeMultilineDocument}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout

private[perf] object EqualsBenchmarks:

  given Balance = Balance.default

  /** Measures `Buffer.equals`/`AppState.equals` under the three shapes of comparison the reducers actually perform: the
    * same instance (the hand-rolled `eq` fast path), a `.copy()` of it (a different instance whose fields -- including
    * the `Rope` content -- are still the same shared references), and an independently built value with equal content
    * but no shared references anywhere in the tree. Only the last case forces a full structural walk of the `Rope`,
    * which has no custom `equals` of its own -- this is #1002's deferred "removed, or their retention is justified by
    * measurement" acceptance criterion.
    */
  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    val content     = largeMultilineDocument(lines = 15_000)
    val stateA      = editorState(content, None)
    val stateB      = editorState(content, None)
    val stateACopy  = stateA.copy()
    val bufferA     = stateA.persisted.buffers(BufferId(1))
    val bufferB     = stateB.persisted.buffers(BufferId(1))
    val bufferACopy = bufferA.copy()

    List(
      BenchmarkRunner.Benchmark(
        "equals.appstate.same_reference",
        3,
        20,
        () => assert(stateA == stateA),
        () => stateA == stateA
      ),
      BenchmarkRunner.Benchmark(
        "equals.appstate.shared_fields_different_instance",
        3,
        20,
        () => assert(stateA == stateACopy),
        () => stateA == stateACopy
      ),
      BenchmarkRunner.Benchmark(
        "equals.appstate.independent_equal_content",
        3,
        20,
        () => assert(stateA == stateB),
        () => stateA == stateB
      ),
      BenchmarkRunner.Benchmark(
        "equals.buffer.same_reference",
        3,
        20,
        () => assert(bufferA == bufferA),
        () => bufferA == bufferA
      ),
      BenchmarkRunner.Benchmark(
        "equals.buffer.shared_fields_different_instance",
        3,
        20,
        () => assert(bufferA == bufferACopy),
        () => bufferA == bufferACopy
      ),
      BenchmarkRunner.Benchmark(
        "equals.buffer.independent_equal_content",
        3,
        20,
        () => assert(bufferA == bufferB),
        () => bufferA == bufferB
      )
    ) ++ multiBufferEqualsBenchmarks()

  /** The equals-benchmark scenarios above compare a single buffer in isolation, but `AppState.equals` walks the whole
    * `persisted.buffers` map on every dispatched event. A real session has many open buffers, and the common case is
    * one buffer edited while the rest are untouched -- so this measures a 30-buffer session under that exact shape,
    * where 29 of 30 map entries are the same `Buffer` references across both sides of the comparison and only one
    * differs, alongside the same-reference and no-shared-references extremes for contrast.
    */
  private def multiBufferEqualsBenchmarks(): List[BenchmarkRunner.Benchmark] =
    val bufferCount    = 30
    val linesPerBuffer = 2_000
    val paneId         = PaneId(0)
    def session(): AppState =
      val buffers = (0 until bufferCount).map { i =>
        val id = BufferId(i)
        id -> Buffer.fromString(id, largeMultilineDocument(lines = linesPerBuffer))
      }.toMap
      AppState.initial.copy(persisted =
        AppState.initial.persisted.copy(
          buffers = buffers,
          bufferOrder = buffers.keys.toList,
          layout = Layout(
            editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, BufferId(0))),
            activeEditorPaneId = Some(paneId),
            workspaceTree = Some(com.serenity.TestWorkspaceTrees.linear(paneId))
          )
        )
      )

    val baseSession        = session()
    val independentSession = session()

    val editedBufferId = BufferId(bufferCount - 1)
    val editedBuffer   = baseSession.persisted.buffers(editedBufferId)
    val oneEditedBuffers = baseSession.persisted.buffers.updated(
      editedBufferId,
      editedBuffer.copy(editing = EditingState(List(CursorPosition(0, 1))))
    )
    val oneBufferEditedSession = baseSession.copy(persisted = baseSession.persisted.copy(buffers = oneEditedBuffers))

    List(
      BenchmarkRunner.Benchmark(
        "equals.appstate.multi_buffer_session.same_reference",
        3,
        20,
        () => assert(baseSession == baseSession),
        () => baseSession == baseSession
      ),
      BenchmarkRunner.Benchmark(
        "equals.appstate.multi_buffer_session.one_buffer_edited",
        3,
        20,
        () => assert(baseSession != oneBufferEditedSession),
        () => baseSession == oneBufferEditedSession
      ),
      BenchmarkRunner.Benchmark(
        "equals.appstate.multi_buffer_session.independent_equal_content",
        3,
        20,
        () => assert(baseSession == independentSession),
        () => baseSession == independentSession
      )
    )

end EqualsBenchmarks
