package com.serenity.state.manager

import com.serenity.rope.{Balance, ChangeSet, Rope}
import com.serenity.state.models.*
import com.serenity.testkit.UniqueTextEdits
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Damage taken from the change a commit carries (#1838) is the damage found by comparing the two texts. */
class DamageProducerChangeSetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def withDocument(document: Document): AppState =
    val base = AppState.initial.persisted.buffers(bufferId)
    AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(buffers = AppState.initial.persisted.buffers.updated(bufferId, base.copy(document = document)))
    )

  "DamageProducer" should "report the same damage from a recorded change as from comparing the texts" in {
    val cases = UniqueTextEdits.cases(seed = 1838, count = 300)
    cases.size should be > 100
    cases.foreach { found =>
      val before   = Document(found.before)
      val recorded = before.edited(found.after, found.change)
      val compared = recorded.copy(changes = ChangeLog.empty)

      compared.changeFrom(before) shouldBe None
      withClue(s"${found.change}: ") {
        DamageProducer.forTransition(withDocument(before), withDocument(recorded)) shouldBe
          DamageProducer.forTransition(withDocument(before), withDocument(compared))
      }
    }
  }

  it should "report the same damage for a recorded change that changes nothing" in {
    val before   = Document(Rope("abc"))
    val recorded = before.edited(Rope("abc"), ChangeSet.identity(3))

    DamageProducer.forTransition(withDocument(before), withDocument(recorded)) shouldBe
      DamageProducer.forTransition(withDocument(before), withDocument(recorded.copy(changes = ChangeLog.empty)))
  }
