package com.serenity.io

import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AppleDirectoryModeSpec extends AnyFlatSpec with Matchers:

  private val propertyKey = AppleDirectoryMode.Key

  final private class FakeProperties(initial: Map[String, String] = Map.empty):
    private val store = new ConcurrentHashMap[String, String](initial.asJava)

    val properties: AppleDirectoryMode.Properties = AppleDirectoryMode.Properties(
      read = name => Option(store.get(name)),
      write = (name, value) =>
        value.fold { val _ = store.remove(name) } { v =>
          val _ = store.put(name, v)
        }
    )

    def current: Option[String] = Option(store.get(propertyKey))

  "AppleDirectoryMode" should "use the property AWT reads to switch a native dialog to directory picking" in {
    AppleDirectoryMode.Key shouldBe "apple.awt.fileDialogForDirectories"
  }

  it should "only apply on macOS" in {
    AppleDirectoryMode.appliesTo("Mac OS X") shouldBe true
    AppleDirectoryMode.appliesTo("Linux") shouldBe false
    AppleDirectoryMode.appliesTo("Windows 11") shouldBe false
  }

  it should "turn directory picking on only while the body runs" in {
    val fake = new FakeProperties()

    val seen = AppleDirectoryMode.around(forDirectories = true, fake.properties)(fake.current)

    seen shouldBe Some("true")
    fake.current shouldBe None
  }

  it should "restore the value that was set before, not just remove its own" in {
    val fake = new FakeProperties(Map(propertyKey -> "true"))

    val seen = AppleDirectoryMode.around(forDirectories = false, fake.properties)(fake.current)

    seen shouldBe Some("false")
    fake.current shouldBe Some("true")
  }

  it should "restore the previous value when the body throws" in {
    val fake = new FakeProperties(Map(propertyKey -> "false"))

    val failure = intercept[IllegalStateException]:
      AppleDirectoryMode.around(forDirectories = true, fake.properties)(throw IllegalStateException("dialog failed"))

    failure.getMessage shouldBe "dialog failed"
    fake.current shouldBe Some("false")
  }

  it should "leave the property absent after a failure when it was absent before" in {
    val fake = new FakeProperties()

    intercept[IllegalStateException]:
      AppleDirectoryMode.around(forDirectories = true, fake.properties)(throw IllegalStateException("dialog failed"))

    fake.current shouldBe None
  }

  it should "give a dialog shown inside a folder dialog its own setting and put the outer one back" in {
    val fake = new FakeProperties()

    val (outerBefore, innerSeen, outerAfter) =
      AppleDirectoryMode.around(forDirectories = true, fake.properties) {
        val outerBefore = fake.current
        val innerSeen   = AppleDirectoryMode.around(forDirectories = false, fake.properties)(fake.current)
        (outerBefore, innerSeen, fake.current)
      }

    outerBefore shouldBe Some("true")
    innerSeen shouldBe Some("false")
    outerAfter shouldBe Some("true")
    fake.current shouldBe None
  }

  it should "return what the body returns" in {
    AppleDirectoryMode.around(forDirectories = true, new FakeProperties().properties)(42) shouldBe 42
  }

  "The JVM properties" should "round-trip through the real system properties and restore them" in {
    val before = Option(System.getProperty(propertyKey))
    try
      AppleDirectoryMode.around(forDirectories = true)(System.getProperty(propertyKey)) shouldBe "true"
      Option(System.getProperty(propertyKey)) shouldBe before
    finally
      before.fold { val _ = System.clearProperty(propertyKey) } { v =>
        val _ = System.setProperty(propertyKey, v)
      }
  }
