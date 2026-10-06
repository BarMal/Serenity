package com.serenity.io

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The local document provider must sample its clock before it stats: a clock read after the stat can land past the end
  * of a coarse tick that the stat did not, and the stamp would then vouch for content a same-size rewrite has since
  * replaced.
  */
class FileStampOrderingSpec extends AnyFlatSpec with Matchers:

  private def bytes(text: String): Array[Byte] = text.getBytes(StandardCharsets.UTF_8)

  private def externalRewrite(content: Array[Byte]): Array[Byte] = Array.fill(content.length)('x'.toByte)

  private def worldOver(text: String) =
    val path = Files.createTempFile("file-stamp-ordering", ".txt")
    Files.write(path, bytes(text))
    val world = CoarseTickWorld(path, externalRewrite)
    (
      StorageLocation.Local(path),
      world,
      LocalDocumentStorageProvider(clock = world.clock, attributes = world.attributes)
    )

  "open" should "not let its stamp vouch for bytes an in-tick external rewrite then replaced" in {
    val (location, world, provider) = worldOver("abc")
    val opened                      = provider.open(location).unsafeRunSync().toOption
    world.settle.unsafeRunSync()

    val saved = provider.save(location, bytes("new"), opened.flatMap(_.revision)).unsafeRunSync()

    saved shouldBe Left(DocumentStorageError.Conflict(location))
  }

  "save" should "not let the stamp of what it wrote vouch once an in-tick external rewrite replaced it" in {
    val (location, world, provider) = worldOver("abc")
    val written                     = provider.save(location, bytes("def"), None).unsafeRunSync().toOption
    world.settle.unsafeRunSync()

    val saved = provider.save(location, bytes("new"), written.flatMap(_.revision)).unsafeRunSync()

    saved shouldBe Left(DocumentStorageError.Conflict(location))
  }

  "a stamp taken inside the tick" should "vouch again once the tick is over (the specs above are not vacuous)" in {
    val (location, world, provider) = worldOver("abc")
    world.settle.unsafeRunSync()
    val opened = provider.open(location).unsafeRunSync().toOption.flatMap(_.revision)

    opened.flatMap(_.stamp).isDefined shouldBe true
  }
