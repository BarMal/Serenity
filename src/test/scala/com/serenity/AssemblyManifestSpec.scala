package com.serenity

import scala.io.Source

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The attributes `sbt assembly` writes into Serenity.jar's manifest, as build.sbt hands them to the tests. JDK 24 warns
  * on every native library load and will later block it unless native access is granted, and an executable jar grants
  * it from its manifest, so `java -jar Serenity.jar` needs the attribute even where a package passes the launch option.
  */
class AssemblyManifestSpec extends AnyFlatSpec with Matchers:

  private val attributes: Map[String, String] =
    Option(getClass.getResourceAsStream("/assembly/manifest-attributes.txt")).fold(Map.empty[String, String]) { stream =>
      val source = Source.fromInputStream(stream, "UTF-8")
      try
        source.getLines().flatMap(_.split(": ", 2) match
          case Array(name, value) => Some(name -> value)
          case _                  => None
        ).toMap
      finally source.close()
    }

  "The assembled jar's manifest" should "grant native access to the classpath, where Skiko, JNA and JLine load their libraries" in {
    attributes.get("Enable-Native-Access") shouldBe Some("ALL-UNNAMED")
  }
