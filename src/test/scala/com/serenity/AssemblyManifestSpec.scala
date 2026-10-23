package com.serenity

import scala.io.Source

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The attributes `sbt assembly` writes into Serenity.jar's manifest, as build.sbt hands them to the tests. JDK 24
  * warns on every native library load and will later block it unless native access is granted. An executable jar
  * grants it from its manifest, so `java -jar Serenity.jar` needs the attribute even where a package passes the option.
  */
class AssemblyManifestSpec extends AnyFlatSpec with Matchers:

  private def resourceText(path: String): String =
    Option(getClass.getResourceAsStream(path)).fold("") { stream =>
      val source = Source.fromInputStream(stream, "UTF-8")
      try source.mkString
      finally source.close()
    }

  private val attributes: Map[String, String] =
    resourceText("/assembly/manifest-attributes.txt").linesIterator
      .map(_.split(": ", 2).toList)
      .collect { case name :: value :: Nil => name -> value }
      .toMap

  "The assembled jar's manifest" should "grant native access to the classpath, where the native libraries load" in {
    attributes.get("Enable-Native-Access") shouldBe Some("ALL-UNNAMED")
  }
