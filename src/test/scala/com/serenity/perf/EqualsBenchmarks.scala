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
  final private case class SingleBuffer(
      stateA: AppState,
      stateB: AppState,
      stateACopy: AppState,
      bufferA: Buffer,
      bufferB: Buffer,
      bufferACopy: Buffer
  )

  private def singleBuffer(): SingleBuffer =
    val content = largeMultilineDocument(lines = 15_000)
    val stateA  = editorState(content, None)
    val stateB  = editorState(content, None)
    val bufferA = stateA.persisted.buffers(BufferId(1))
    SingleBuffer(
      stateA,
      stateB,
      stateA.copy(),
      bufferA,
      stateB.persisted.buffers(BufferId(1)),
      bufferA.copy()
    )

  // A comparison here costs a few nanoseconds, so each body binds what it compares to locals of its own: reading it
  // back through the shared fixture on every call would be a visible part of what is timed.
  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    lazy val fixtures = singleBuffer()

    List(
      BenchmarkRunner.Benchmark.lazily("equals.appstate.same_reference", 3, 20) {
        val stateA = fixtures.stateA
        BenchmarkRunner.Body(() => assert(stateA == stateA), () => stateA == stateA)
      },
      BenchmarkRunner.Benchmark.lazily("equals.appstate.shared_fields_different_instance", 3, 20) {
        val (stateA, stateACopy) = (fixtures.stateA, fixtures.stateACopy)
        BenchmarkRunner.Body(() => assert(stateA == stateACopy), () => stateA == stateACopy)
      },
      BenchmarkRunner.Benchmark.lazily("equals.appstate.independent_equal_content", 3, 20) {
        val (stateA, stateB) = (fixtures.stateA, fixtures.stateB)
        BenchmarkRunner.Body(() => assert(stateA == stateB), () => stateA == stateB)
      },
      BenchmarkRunner.Benchmark.lazily("equals.buffer.same_reference", 3, 20) {
        val bufferA = fixtures.bufferA
        BenchmarkRunner.Body(() => assert(bufferA == bufferA), () => bufferA == bufferA)
      },
      BenchmarkRunner.Benchmark.lazily("equals.buffer.shared_fields_different_instance", 3, 20) {
        val (bufferA, bufferACopy) = (fixtures.bufferA, fixtures.bufferACopy)
        BenchmarkRunner.Body(() => assert(bufferA == bufferACopy), () => bufferA == bufferACopy)
      },
      BenchmarkRunner.Benchmark.lazily("equals.buffer.independent_equal_content", 3, 20) {
        val (bufferA, bufferB) = (fixtures.bufferA, fixtures.bufferB)
        BenchmarkRunner.Body(() => assert(bufferA == bufferB), () => bufferA == bufferB)
      }
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

    lazy val baseSession        = session()
    lazy val independentSession = session()

    val editedBufferId = BufferId(bufferCount - 1)
    lazy val oneBufferEditedSession =
      val editedBuffer = baseSession.persisted.buffers(editedBufferId)
      val oneEditedBuffers = baseSession.persisted.buffers.updated(
        editedBufferId,
        editedBuffer.copy(editing = EditingState(List(CursorPosition(0, 1))))
      )
      baseSession.copy(persisted = baseSession.persisted.copy(buffers = oneEditedBuffers))

    List(
      BenchmarkRunner.Benchmark.lazily("equals.appstate.multi_buffer_session.same_reference", 3, 20) {
        val built = baseSession
        BenchmarkRunner.Body(() => assert(built == built), () => built == built)
      },
      BenchmarkRunner.Benchmark.lazily("equals.appstate.multi_buffer_session.one_buffer_edited", 3, 20) {
        val (built, edited) = (baseSession, oneBufferEditedSession)
        BenchmarkRunner.Body(() => assert(built != edited), () => built == edited)
      },
      BenchmarkRunner.Benchmark.lazily("equals.appstate.multi_buffer_session.independent_equal_content", 3, 20) {
        val (built, independent) = (baseSession, independentSession)
        BenchmarkRunner.Body(() => assert(built == independent), () => built == independent)
      }
    )

end EqualsBenchmarks
