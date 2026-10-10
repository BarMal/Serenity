package com.serenity.ui.layout

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Buffer, BufferId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChromeLayoutMemoSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(100, 30)

  private val memo = new ChromeLayoutMemo()

  private def layoutOf(state: AppState, size: ViewportSize): CalculatedLayout =
    memo.layoutFor(state, size, 0.0)(LayoutEngine.calculateLayoutWithUIUncached(state, size, 0.0))

  private def computationsDuring(work: => Unit): Int =
    val before = memo.computationCount
    work
    memo.computationCount - before

  private def withActiveBufferText(state: AppState, text: String): AppState =
    val id = state.activeBuffer.map(_.id).getOrElse(BufferId(0))
    state.copy(persisted =
      state.persisted.copy(buffers = state.persisted.buffers + (id -> Buffer.fromString(id, text)))
    )

  "Chrome layout" should "be computed once for the scene, accessibility and hit-test paths of one state" in {
    val state = AppState.initial(AppConfig.default.withLineNumbers(false))

    computationsDuring {
      List.fill(5)(layoutOf(state, viewport))
    } should be <= 1
  }

  it should "not be recomputed by a keystroke that only changes buffer text" in {
    val before = AppState.initial(AppConfig.default.withLineNumbers(false))
    val after  = withActiveBufferText(before, "typed")
    layoutOf(before, viewport)

    computationsDuring(layoutOf(after, viewport)) shouldBe 0
  }

  it should "be recomputed when the viewport changes" in {
    val state = AppState.initial(AppConfig.default.withLineNumbers(false))
    layoutOf(state, viewport)

    computationsDuring(layoutOf(state, ViewportSize(120, 40))) shouldBe 1
  }

  it should "be recomputed when the chrome config changes" in {
    val state = AppState.initial(AppConfig.default.withLineNumbers(false))
    layoutOf(state, viewport)
    val withLineNumbers =
      state.copy(persisted = state.persisted.copy(config = state.persisted.config.withLineNumbers(true)))

    computationsDuring(layoutOf(withLineNumbers, viewport)) shouldBe 1
  }

  it should "follow the gutter's width when the longest buffer gains a digit" in {
    val short = withActiveBufferText(AppState.initial(AppConfig.default.withLineNumbers(true)), "a\nb")
    val long  = withActiveBufferText(short, Vector.fill(1500)("x").mkString("\n"))

    val shortLayout = layoutOf(short, viewport)
    val longLayout  = layoutOf(long, viewport)

    longLayout.lineNumberRect.map(_.width) should not be shortLayout.lineNumberRect.map(_.width)
  }
