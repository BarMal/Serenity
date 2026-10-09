package com.serenity.state.manager

import java.net.URI

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.io.ReleasesPage
import com.serenity.state.models.{Notice, NoticeLevel}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReleasesPageEffectSpec extends AnyFlatSpec with Matchers:

  "ReleasesPageEffect.open" should "hand the releases URL to the browser and show no notice" in {
    val browsed = Ref.of[IO, List[URI]](Nil).unsafeRunSync()
    val notices = Ref.of[IO, List[Notice]](Nil).unsafeRunSync()

    ReleasesPageEffect.open(uri => browsed.update(_ :+ uri), notice => notices.update(_ :+ notice)).unsafeRunSync()

    browsed.get.unsafeRunSync() shouldBe List(ReleasesPage.uri)
    notices.get.unsafeRunSync() shouldBe empty
  }

  it should "show a warning that carries the URL when no browser can be opened" in {
    val notices = Ref.of[IO, List[Notice]](Nil).unsafeRunSync()

    ReleasesPageEffect
      .open(_ => IO.raiseError(new UnsupportedOperationException("headless")), notice => notices.update(_ :+ notice))
      .unsafeRunSync()

    val shown = notices.get.unsafeRunSync()
    shown.map(_.level) shouldBe List(NoticeLevel.Warning)
    shown.map(_.message).mkString should include(ReleasesPage.url)
  }
