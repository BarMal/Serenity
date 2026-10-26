package com.serenity.ui.skia

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import com.serenity.state.manager.RenderCaches
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaTextCachesSpec extends AnyFlatSpec with Matchers:

  "The Skia text caches of an owner" should "be created when Skia first asks for them and not before" in {
    val owner = RenderCaches.create()

    SkiaTextCaches.heldBy(owner.backendCaches) shouldBe false

    val _ = SkiaTextCaches.of(owner.backendCaches)

    SkiaTextCaches.heldBy(owner.backendCaches) shouldBe true
  }

  it should "be the same set for every surface the owner draws" in {
    val owner = RenderCaches.create()

    val first  = SkiaTextCaches.of(owner.backendCaches)
    val second = SkiaTextCaches.of(owner.backendCaches)

    second should be theSameInstanceAs first
    second.fonts should be theSameInstanceAs first.fonts
    second.shapedText should be theSameInstanceAs first.shapedText
    second.images should be theSameInstanceAs first.images
  }

  it should "not be shared with another owner" in {
    val one = SkiaTextCaches.of(RenderCaches.create().backendCaches)
    val two = SkiaTextCaches.of(RenderCaches.create().backendCaches)

    one should not be theSameInstanceAs(two)
    one.fonts should not be theSameInstanceAs(two.fonts)
    one.shapedText should not be theSameInstanceAs(two.shapedText)
    one.images should not be theSameInstanceAs(two.images)
  }

  "The Skia sources" should "keep no process-wide cache: every cache belongs to an owner" in {
    val root: Path = Paths.get("src", "main", "scala", "com", "serenity", "ui", "skia")
    val offenders = Using.resource(Files.walk(root)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
        .filter(path =>
          Files.readAllLines(path).asScala.exists(_.matches("""\s*(private\S*\s+)?(lazy\s+)?val\s+shared\b.*"""))
        )
        .map(path => root.relativize(path).toString)
        .toList
    }

    offenders shouldBe Nil
  }
