package com.serenity.perf

import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import cats.syntax.semigroup.*
import com.serenity.keystroke.events.{
  DeleteBackward,
  Enter,
  Event,
  InsertChar,
  PageDown,
  PageUp,
  ResizeEvent,
  ScrollDown,
  ScrollUp
}
import com.serenity.setCursorPosition
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.{BufferId, Damage}
import com.serenity.ui.layout.{PixelRect, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, RendererFrameState, ScreenIdentity, SurfaceContentIdentity}
import com.serenity.MockRenderSurface
import org.scalacheck.Gen
import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/** A pixel buffer that sat out frames must be told everything those frames changed, not only the last. Windows
  * alternate between images, so the planner repaints from the damage it accumulated for the buffer since it was last
  * drawn into. This is the structural half of [[StaleBackBufferFrameSpec]]: that spec notices a buffer that missed
  * damage as differing pixels, and this one names the damage that went missing, without drawing anything.
  *
  * Damage is made by real transitions of a real editor, so the facts are the ones [[DamageProducer]] reports for
  * typing, paging, scrolling and cursor moves, and is fed to the accumulation [[RendererFramePlanner]] reads, in the
  * order it does: fold the frame's damage into every buffer, then drain the one the frame draws into.
  */
class BufferDamageCoverageSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 20, sizeRange = 40)

  private val screens           = 2
  private val buffersPerScreen  = 3
  private val documentLineCount = 120
  private val grid              = ViewportSize(60, 20)

  private val document =
    (1 to documentLineCount).map(line => s"line $line " + "lorem ipsum dolor sit amet " * (1 + line % 5)).mkString("\n")

  /** What accumulation the property exercises; the second implementation below is deliberately broken. */
  trait DamageLedger:
    def accumulate(output: FrameOutput, damage: Damage): Unit
    def drain(output: FrameOutput, buffer: SurfaceContentIdentity): Damage

  private def realLedger(): DamageLedger =
    val frameState = RendererFrameState(64)
    new DamageLedger:
      def accumulate(output: FrameOutput, damage: Damage): Unit =
        frameState.accumulateBufferDamage(Some(output), damage)
      def drain(output: FrameOutput, buffer: SurfaceContentIdentity): Damage =
        frameState.drainBufferDamage(Some(output), buffer)

  /** Forgets every second frame's damage, as a back buffer that missed an update would. */
  private def forgetfulLedger(): DamageLedger =
    val real = realLedger()
    val seen = new java.util.concurrent.atomic.AtomicInteger(0)
    new DamageLedger:
      def accumulate(output: FrameOutput, damage: Damage): Unit =
        if seen.incrementAndGet() % 2 == 1 then real.accumulate(output, damage)
      def drain(output: FrameOutput, buffer: SurfaceContentIdentity): Damage = real.drain(output, buffer)

  enum Transition:
    case Edit(event: Event)
    case MoveCursor(line: Int, column: Int)
    case Resize(columns: Int, rows: Int)

  /** One frame: a transition, the screen showing it, and the buffer of that screen it is drawn into (none when the
    * frame is skipped, as a stood-down frame or a frame that draws elsewhere is).
    */
  final case class Frame(transition: Transition, screen: Int, drawnInto: Option[Int])

  private val edits: Gen[Event] = Gen.frequency(
    6 -> Gen.oneOf("WWiM m".toList).map(InsertChar(_)),
    3 -> Gen.const(DeleteBackward),
    1 -> Gen.const(Enter),
    2 -> Gen.oneOf(PageDown, PageUp),
    3 -> Gen.choose(1, 12).flatMap(n => Gen.oneOf(ScrollDown(n), ScrollUp(n)))
  )

  private val transitions: Gen[Transition] = Gen.frequency(
    8 -> edits.map(Transition.Edit(_)),
    3 -> (for
      line   <- Gen.choose(0, documentLineCount - 1)
      column <- Gen.choose(0, 40)
    yield Transition.MoveCursor(line, column)),
    1 -> (for
      columns <- Gen.choose(40, 90)
      rows    <- Gen.choose(10, 30)
    yield Transition.Resize(columns, rows))
  )

  private val frames: Gen[List[Frame]] = Gen.listOf(
    for
      transition <- transitions
      screen     <- Gen.choose(0, screens - 1)
      drawnInto  <- Gen.frequency(1 -> Gen.const(None), 9 -> Gen.choose(0, buffersPerScreen - 1).map(Some(_)))
    yield Frame(transition, screen, drawnInto)
  )

  /** The damage each frame's transition reports, from an editor that applies them in order. */
  private def damagesOf(frames: List[Frame]): List[Damage] =
    val (stateManager, _) = LaptopFrameBenchmarks.stateManagerHolding(document, grid = grid)
    val paneId            = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
    frames.map { frame =>
      val before = stateManager.getCurrentState.unsafeRunSync()
      frame.transition match
        case Transition.Edit(event)      => stateManager.applyEvent(event).unsafeRunSync()
        case Transition.MoveCursor(l, c) => stateManager.setCursorPosition(paneId, l, c).unsafeRunSync()
        case Transition.Resize(cols, rws) =>
          stateManager.applyEvent(ResizeEvent(ViewportSize(cols, rws))).unsafeRunSync()
      DamageProducer.forTransition(before, stateManager.getCurrentState.unsafeRunSync())
    }

  /** Whether `drained` repaints at least everything `expected` names. */
  private def covers(drained: Damage, expected: Damage): Boolean =
    Damage.isEverything(drained) || {
      val buffers = (0 to 3).map(BufferId(_))
      buffers.forall { buffer =>
        val owed = Damage.damagedLines(buffer, expected)
        Damage.damagesEveryRow(buffer, drained) ||
        !Damage.damagesEveryRow(buffer, expected) &&
        owed.subsetOf(Damage.damagedLines(buffer, drained)) &&
        Damage.damagedSpans(buffer, expected).forall(span => spanCovered(buffer, drained, span))
      } &&
      (!Damage.touchesChrome(expected) || Damage.touchesChrome(drained)) &&
      (!Damage.touchesTabBar(expected) || Damage.touchesTabBar(drained)) &&
      Damage.scrollbarPaneIds(expected).subsetOf(Damage.scrollbarPaneIds(drained)) &&
      Damage.paneChromeIds(expected).subsetOf(Damage.paneChromeIds(drained)) &&
      Damage.surfaceIds(expected).subsetOf(Damage.surfaceIds(drained))
    }

  private def spanCovered(buffer: BufferId, drained: Damage, span: Damage.BufferCells): Boolean =
    Damage.damagedLines(buffer, drained).contains(span.row) ||
      Damage.damagedSpans(buffer, drained).exists { held =>
        held.row == span.row && held.fromColumn <= span.fromColumn &&
        held.toColumn.forall(end => span.toColumn.exists(_ <= end))
      }

  /** Damage reported since each (screen, buffer) was last drawn into, newest first; absent until its first draw. */
  private type Pending = Map[(Int, Int), List[Damage]]

  /** The first frame at which a buffer is handed less damage than the frames since its last draw reported, if any. */
  private def firstShortfall(ledger: DamageLedger, schedule: List[Frame]): Option[String] =
    val damages = damagesOf(schedule)
    val outputs =
      Vector.fill(screens)(FrameOutput(ScreenIdentity(new Object), new AtomicReference[Option[List[PixelRect]]](None)))
    val keys =
      Vector.fill(screens)(Vector.fill(buffersPerScreen)(MockRenderSurface(10, 5, true).persistentContentKey.get))
    val verdict =
      schedule.zip(damages).zipWithIndex.foldLeft(Right(Map.empty[(Int, Int), List[Damage]]): Either[String, Pending]) {
        case (Right(pending), ((frame, damage), index)) =>
          ledger.accumulate(outputs(frame.screen), damage)
          val grown = pending.map((key, held) => key -> (if key._1 == frame.screen then damage :: held else held))
          frame.drawnInto.fold(Right(grown)) { buffer =>
            val key     = (frame.screen, buffer)
            val drained = ledger.drain(outputs(frame.screen), keys(frame.screen)(buffer))
            grown.get(key) match
              case Some(missed) =>
                val expected = missed.foldLeft(Damage.Nothing: Damage)(_ |+| _)
                Either.cond(
                  covers(drained, expected),
                  grown.updated(key, Nil),
                  s"frame $index (${frame.transition}) drew buffer $buffer of screen ${frame.screen} with $drained, " +
                    s"but the ${missed.size} frame(s) since it was last drawn into reported $expected"
                )
              case None => Right(grown.updated(key, Nil))
          }
        case (failed, _) => failed
      }
    verdict.left.toOption

  "A buffer drawn into after sitting out frames" should "be handed all the damage those frames reported" in
    forAll(frames)(schedule => firstShortfall(realLedger(), schedule) shouldBe None)

  "The coverage property" should "notice an accumulation that forgets a frame's damage" in
    intercept[TestFailedException] {
      forAll(frames, minSuccessful(40))(schedule => firstShortfall(forgetfulLedger(), schedule) shouldBe None)
    }
