package com.serenity.state.models

import cats.effect.IO
import cats.effect.syntax.all.*
import cats.effect.unsafe.implicits.global
import com.serenity.TestWorkspaceTrees
import com.serenity.rope.Balance
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1677: `annotationIndex`/`markdownFenceIndex`/`semanticTokensAvailability` used to be backed by
  * `AtomicReference[Map[BufferId, _]]` fields living directly on the `AppState` case class -- a JVM-level mutable
  * field hiding inside a type the rest of the codebase treats as pure, immutable data. That field was always
  * instance-scoped in practice (a fresh `AtomicReference` is constructed by every `copy()`), so it never literally
  * leaked a value from one `AppState` into another -- but its mere presence broke the case-class contract
  * (`AppState` no longer "describes data", per `ArchitectureChecks.ForbiddenImports`'s reason string for
  * `state/models`) and made two `AppState`s impossible to reason about as independent, referentially transparent
  * values, which is exactly what running two of them concurrently in tests requires.
  *
  * This spec pins the property directly: two independently constructed `AppState` instances, holding different
  * buffers under the same `BufferId`, must never let one instance's derived index answer for the other's -- run
  * both in parallel (interleaved on purpose, not just sequentially) against a single JVM, the way two independent
  * test suites or two editor instances would.
  */
class AppStateCacheIsolationSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private def stateWithComment(text: String, comment: DocumentComment): AppState =
    val buffer = Buffer.fromString(bufferId, text).copy(annotations = Annotations(documentComments = List(comment)))
    val base   = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        )
      )
    )

  "AppState" should "hold no mutable field: two instances with the same BufferId never share a computed index" in {
    val commentA = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 0), "state A's own comment")
    val commentB = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 0), "state B's own comment")

    val stateA = stateWithComment("hello", commentA)
    val stateB = stateWithComment("world", commentB)

    // Read from each state repeatedly and in an interleaved order, as two independent editors (or two parallel
    // test suites sharing one JVM) would -- a cache keyed only by BufferId and shared at the JVM/class level would
    // let the second read observe the first state's value.
    (1 to 5).foreach { _ =>
      stateA.annotationIndex(bufferId).map(_.comments) shouldBe Some(Vector(commentA))
      stateB.annotationIndex(bufferId).map(_.comments) shouldBe Some(Vector(commentB))
    }

    // Mutating-looking access on one instance must not perturb the other's answer for the same key.
    stateA.annotationIndex(bufferId).map(_.comments) shouldBe Some(Vector(commentA))
    stateB.annotationIndex(bufferId).map(_.comments) shouldBe Some(Vector(commentB))
  }

  it should "run two independent AppState instances concurrently without cross-contaminating their derived indexes" in {
    def checkOne(i: Int): IO[(Option[Vector[DocumentComment]], Option[Vector[DocumentComment]])] = IO {
      val comment = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 0), s"comment-$i")
      val state   = stateWithComment(s"content-$i", comment)
      state.annotationIndex(bufferId).map(_.comments) -> Some(Vector(comment))
    }

    val results = (0 until 50).toList.parTraverseN(16)(checkOne).unsafeRunSync()

    results.foreach { case (actual, expected) => actual shouldBe expected }
  }

  it should "carry no AtomicReference (or other mutable) field on the AppState instance" in {
    val mutableFields = classOf[AppState].getDeclaredFields.filter { field =>
      val typeName = field.getType.getName
      typeName.startsWith("java.util.concurrent.atomic.") || typeName.startsWith("scala.collection.mutable.")
    }

    mutableFields shouldBe empty
  }
