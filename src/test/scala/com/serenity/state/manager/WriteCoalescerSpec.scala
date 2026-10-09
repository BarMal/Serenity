package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WriteCoalescerSpec extends AnyFlatSpec with Matchers:

  private val inBackground: IO[Unit] => IO[Unit] = _.start.void

  "A burst of requests" should "run the job once, on what is current when it starts" in {
    val program =
      for
        latest   <- Ref.of[IO, Int](0)
        ran      <- Ref.of[IO, List[Int]](Nil)
        coalesce <- IO(WriteCoalescer.unsafe)
        _ <- (1 to 5).toList.traverse_(n =>
          latest.set(n) >> coalesce.request(inBackground, latest.get.flatMap(n => ran.update(_ :+ n)))
        )
        _     <- IO.sleep(WriteCoalescer.Debounce * 2)
        calls <- ran.get
      yield calls

    runVirtual(program) shouldBe List(5)
  }

  "A request after the job has started" should "run the job again" in {
    val program =
      for
        latest   <- Ref.of[IO, Int](0)
        ran      <- Ref.of[IO, List[Int]](Nil)
        coalesce <- IO(WriteCoalescer.unsafe)
        job = latest.get.flatMap(n => ran.update(_ :+ n))
        _     <- latest.set(1) >> coalesce.request(inBackground, job)
        _     <- IO.sleep(WriteCoalescer.Debounce * 2)
        _     <- latest.set(2) >> coalesce.request(inBackground, job)
        _     <- IO.sleep(WriteCoalescer.Debounce * 2)
        calls <- ran.get
      yield calls

    runVirtual(program) shouldBe List(1, 2)
  }
