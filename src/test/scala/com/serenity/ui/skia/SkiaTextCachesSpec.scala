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

  "The warm-up stats of an owner" should "be absent until its Skia text caches exist" in {
    val owner = RenderCaches.create()

    SkiaTextCaches.warmupOf(owner.backendCaches) shouldBe None

    val _ = SkiaTextCaches.of(owner.backendCaches)

    SkiaTextCaches.warmupOf(owner.backendCaches) should not be None
  }

  it should "count the shaper, each face built once and each text shaped once" in {
    val owner  = RenderCaches.create()
    val caches = SkiaTextCaches.of(owner.backendCaches)
    caches.warmup.snapshot.shapers shouldBe 1
    caches.warmup.snapshot.shaperNanos should be > 0L
    caches.warmup.snapshot.faces shouldBe 0

    val awt  = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 14)
    val face = caches.fonts.faceFor(awt)
    caches.fonts.faceFor(awt)
    caches.warmup.snapshot.faces shouldBe 1
    caches.warmup.snapshot.faceNanos should be > 0L
    caches.warmup.snapshot.shapings shouldBe 0

    val _ = caches.shapedText.stretchOf("Hello", face.font)
    val _ = caches.shapedText.stretchOf("Hello", face.font)
    caches.warmup.snapshot.shapings shouldBe 1
    caches.warmup.snapshot.shapingNanos should be > 0L
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
