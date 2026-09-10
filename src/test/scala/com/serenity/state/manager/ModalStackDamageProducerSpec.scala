package com.serenity.state.manager

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `runtime.modalStack` (#814) coverage for [[DamageProducer]] -- kept separate from `DamageProducerSpec` so that
  * already-oversized file does not grow past its architecture-check baseline.
  */
class ModalStackDamageProducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def stateWithContent(text: String): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(document = AppState.initial.persisted.buffers(bufferId).document.copy(content = Rope(text)))
        )
      )
    )

  "DamageProducer.forTransition" should "report Everything when a modal dialog appears on the stack" in {
    val before = stateWithContent("alpha")
    val dialog = ModalDialog(SurfaceId("goto"), Modal.GotoLine(""), ModalPlacement.Centered)
    val after  = before.copy(runtime = before.runtime.copy(modalStack = List(dialog)))

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Damage.Surface scoped to a modal dialog when only its own content changes" in {
    val dialog = ModalDialog(SurfaceId("goto"), Modal.GotoLine(""), ModalPlacement.Centered)
    val bare   = stateWithContent("alpha")
    val before = bare.copy(runtime = bare.runtime.copy(modalStack = List(dialog)))
    val after  = before.copy(runtime = before.runtime.copy(modalStack = List(dialog.copy(modal = Modal.GotoLine("1")))))

    DamageProducer.forTransition(before, after) shouldBe Damage.Surface(SurfaceId("goto"))
  }

  it should "report Damage.Surface scoped to only the changed dialog when a second modal is stacked on top" in {
    val parent = ModalDialog(SurfaceId("parent"), Modal.GotoLine(""), ModalPlacement.Centered)
    val child  = ModalDialog(SurfaceId("child"), Modal.GotoLine(""), ModalPlacement.Centered)
    val bare   = stateWithContent("alpha")
    val before = bare.copy(runtime = bare.runtime.copy(modalStack = List(parent, child)))
    val after =
      before.copy(runtime = before.runtime.copy(modalStack = List(parent.copy(modal = Modal.GotoLine("1")), child)))

    DamageProducer.forTransition(before, after) shouldBe Damage.Surface(SurfaceId("parent"))
  }

  it should "report Everything when a modal dialog changes alongside another surface" in {
    val dialog = ModalDialog(SurfaceId("goto"), Modal.GotoLine(""), ModalPlacement.Centered)
    val pinned = UiSurface(
      SurfaceId("outline"),
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Docked
    )
    val bare   = stateWithContent("alpha")
    val before = bare.copy(runtime = bare.runtime.copy(modalStack = List(dialog), uiSurfaces = List(pinned)))
    val after = before.copy(runtime =
      before.runtime.copy(
        modalStack = List(dialog.copy(modal = Modal.GotoLine("1"))),
        uiSurfaces = List(pinned.copy(dismissOnMove = true))
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Everything when a modal dialog closes" in {
    val dialog = ModalDialog(SurfaceId("goto"), Modal.GotoLine(""), ModalPlacement.Centered)
    val bare   = stateWithContent("alpha")
    val before = bare.copy(runtime = bare.runtime.copy(modalStack = List(dialog)))
    val after  = before.copy(runtime = before.runtime.copy(modalStack = Nil))

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }
end ModalStackDamageProducerSpec
