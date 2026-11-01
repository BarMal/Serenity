package com.serenity.ui.benchmark

import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*

/** What each scenario does, frame by frame, as a pure function of the scenario: both backends are driven by the very
  * same states and damage, so the only difference between the two timings is the renderer.
  */
object BenchmarkScript:

  enum FrameKind:
    case Full
    case ColdFull
    case CaretOnly(visible: Boolean)

  final case class Step(state: AppState, damage: Damage, frame: FrameKind)

  def steps(scenario: Scenario, frames: Int, viewportLines: Int)(using Balance): Vector[Step] =
    val start = BenchmarkDocument.initial
    scenario match
      case Scenario.ColdFullFrame =>
        Vector.fill(frames)(Step(start, Damage.Everything, FrameKind.ColdFull))
      case Scenario.TypeCharacter   => typing(start, frames)
      case Scenario.ScrollOneLine   => scrolling(start, frames, viewportLines, lines = 1)
      case Scenario.ScrollFiveLines => scrolling(start, frames, viewportLines, lines = 5)
      case Scenario.PageDown        => scrolling(start, frames, viewportLines, lines = viewportLines)
      case Scenario.CaretBlink =>
        Vector.tabulate(frames)(index => Step(start, Damage.Nothing, FrameKind.CaretOnly(visible = index % 2 == 0)))

  private def bufferOf(state: AppState): Buffer = state.persisted.buffers(BenchmarkDocument.bufferId)

  private def withBuffer(state: AppState, buffer: Buffer): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers = state.persisted.buffers.updated(BenchmarkDocument.bufferId, buffer))
    )

  private def following(states: Vector[AppState], first: AppState)(using Balance): Vector[Step] =
    (first +: states)
      .sliding(2)
      .collect {
        case Vector(before, after) =>
          Step(after, DamageProducer.forTransition(before, after), FrameKind.Full)
      }
      .toVector

  private def typing(start: AppState, frames: Int)(using Balance): Vector[Step] =
    val states = (0 until frames)
      .scanLeft(start) { (state, index) =>
        val buffer = bufferOf(state)
        val edited = buffer.copy(
          document = buffer.document
            .withContent(
              buffer.document.content
                .insert(BenchmarkDocument.typedOffset + index, "x")
                .getOrElse(buffer.document.content)
            )
            .copy(isDirty = true),
          editing = EditingState(List(CursorPosition(BenchmarkDocument.typedLine, index + 1)))
        )
        withBuffer(state, edited)
      }
      .drop(1)
      .toVector
    following(states, start)

  private def scrolling(start: AppState, frames: Int, viewportLines: Int, lines: Int)(using Balance): Vector[Step] =
    val lastTop = bufferOf(start).document.content.lineCount - viewportLines
    val states = Vector.tabulate(frames) { index =>
      val buffer = bufferOf(start)
      withBuffer(start, buffer.copy(viewport = buffer.viewport.copy(topLine = (index * lines) % (lastTop + 1))))
    }
    following(states, start)
