package com.serenity.testkit

import cats.kernel.laws.discipline.OrderTests
import com.serenity.state.models.{BufferId, PaneId}
import com.serenity.testkit.Generators.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

/** Law coverage for the typeclass instances the domain declares.
  *
  * `BufferId` and `PaneId` carry `Order` instances that reducers, layout and session ordering all rely on. An `Order`
  * has laws -- totality, antisymmetry, transitivity, and agreement between `compare`, `eqv` and the comparison
  * operators -- which example-based tests cannot cover, since they hold over all inputs rather than chosen ones.
  *
  * This suite also proves the discipline wiring works, so later law checks (`Monoid[Damage]` in #997,
  * `Monoid[ComponentResult]` in #991, the lens laws in #992) have a pattern to follow.
  */
class IdentifierLawSpec extends AnyFunSuite with FunSuiteDiscipline with Configuration with Matchers:

  checkAll("Order[BufferId]", OrderTests[BufferId].order)
  checkAll("Order[PaneId]", OrderTests[PaneId].order)
