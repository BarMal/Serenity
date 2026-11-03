package com.serenity.ui.layout

import java.util.concurrent.Executor

import scala.collection.mutable.ArrayBuffer

import com.serenity.ui.fonts.FontLoader
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Where a font's certificate comes from (#2447 B2): at once for a font set without ligatures, from a builder that runs
  * off the caller's thread for one set with them, and never again once a miscount has revoked it.
  */
class RowCertificatesSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val frc    = TextLayoutSnapshot.defaultFontRenderContext()
  private val config = FontLoader.FontConfig()

  final private class QueuedExecutor extends Executor:
    val tasks                         = ArrayBuffer.empty[Runnable]
    def execute(task: Runnable): Unit = tasks += task

    def runAll(): Unit =
      val queued = tasks.toList
      tasks.clear()
      queued.foreach(_.run())

  private def keyFor(ligatures: Boolean, sizePt: Float = 12.0f): NominalAdvances.Key =
    NominalAdvances.Key(FontLoader.previewCodeFont(config.copy(fontSize = sizePt, enableLigatures = ligatures)), frc)

  "A font set without ligatures" should "have its certificate at once, asking no builder" in {
    val builder      = QueuedExecutor()
    val certificates = RowCertificates(builder)

    certificates.of(keyFor(ligatures = false)) shouldBe defined
    builder.tasks shouldBe empty
  }

  "A font set with ligatures" should "have no certificate until the builder has run, then have one" in {
    val builder      = QueuedExecutor()
    val certificates = RowCertificates(builder)
    val key          = keyFor(ligatures = true)

    certificates.of(key) shouldBe None
    builder.runAll()

    certificates.of(key) shouldBe defined
  }

  it should "ask the builder once, however often the certificate is asked for before it is ready" in {
    val builder      = QueuedExecutor()
    val certificates = RowCertificates(builder)
    val key          = keyFor(ligatures = true)

    (1 to 5).foreach(_ => certificates.of(key))

    builder.tasks.size shouldBe 1
  }

  it should "not build on the thread that asked" in {
    val callers = ArrayBuffer.empty[Thread]
    val builder: Executor = task =>
      val thread = Thread(() =>
        callers += Thread.currentThread(); task.run()
      )
      thread.start()
      thread.join()
    val certificates = RowCertificates(builder)
    val key          = keyFor(ligatures = true)

    certificates.of(key)
    certificates.of(key) shouldBe defined

    callers.map(_ == Thread.currentThread()) shouldBe List(false)
  }

  it should "keep a refusal: a font that cannot be certified is built once and never certified" in {
    val builder      = QueuedExecutor()
    val certificates = RowCertificates(builder)
    val proportional = NominalAdvances.Key(FontLoader.previewTextFont(config), frc)

    certificates.of(proportional) shouldBe None
    builder.runAll()
    certificates.of(proportional) shouldBe None
    builder.tasks shouldBe empty
  }

  "A revoked certificate" should "stay gone for a font set with ligatures" in {
    val builder      = QueuedExecutor()
    val certificates = RowCertificates(builder)
    val key          = keyFor(ligatures = true)
    certificates.of(key)
    builder.runAll()

    certificates.revoke(key)

    certificates.of(key) shouldBe None
    builder.runAll()
    certificates.of(key) shouldBe None
    builder.tasks shouldBe empty
  }

  it should "stay gone for a font set without ligatures" in {
    val certificates = RowCertificates(QueuedExecutor())
    val key          = keyFor(ligatures = false)
    certificates.of(key) shouldBe defined

    certificates.revoke(key)

    certificates.of(key) shouldBe None
  }

  it should "leave another font's certificate alone" in {
    val certificates = RowCertificates(QueuedExecutor())
    val twelve       = keyFor(ligatures = false, sizePt = 12.0f)
    val fourteen     = keyFor(ligatures = false, sizePt = 14.0f)

    certificates.revoke(twelve)

    certificates.of(fourteen) shouldBe defined
  }

  it should "report whether it was revoked by this call, so a miscount is logged once" in {
    val certificates = RowCertificates(QueuedExecutor())
    val key          = keyFor(ligatures = false)

    certificates.revoke(key) shouldBe true
    certificates.revoke(key) shouldBe false
  }
