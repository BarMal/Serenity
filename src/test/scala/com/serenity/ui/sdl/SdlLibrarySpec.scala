package com.serenity.ui.sdl

import java.nio.file.Path

import com.serenity.ui.sdl.SdlLibrary.Source
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SdlLibrarySpec extends AnyFlatSpec with Matchers:

  private val directory = Path.of("/opt/serenity/natives")
  private val bundled   = directory.resolve(SdlLibrary.FileName)
  private val system    = Source.System(SdlLibrary.FileName)

  "SdlLibrary.candidates" should "try the bundled library first when the property names a directory that holds it" in {
    val found = SdlLibrary.candidates(Map(SdlLibrary.PathProperty -> directory.toString), _ == bundled)

    found shouldBe List(Source.Bundled(bundled), system)
  }

  it should "accept the property naming the library file itself" in {
    val found = SdlLibrary.candidates(Map(SdlLibrary.PathProperty -> bundled.toString), _ == bundled)

    found shouldBe List(Source.Bundled(bundled), system)
  }

  it should "skip a property whose directory does not hold the library" in {
    SdlLibrary.candidates(Map(SdlLibrary.PathProperty -> directory.toString), _ => false) shouldBe List(system)
  }

  it should "offer only the system library when the property is absent or blank" in {
    SdlLibrary.candidates(Map.empty, _ => true) shouldBe List(system)
    SdlLibrary.candidates(Map(SdlLibrary.PathProperty -> "  "), _ => true) shouldBe List(system)
  }

  "SdlLibrary.load" should "return the first source that opens, and say which one it was" in {
    val opened = SdlLibrary.load(List(Source.Bundled(bundled), system), source => Right(source.toString))

    opened shouldBe Right((Source.Bundled(bundled), Source.Bundled(bundled).toString))
  }

  it should "fall through a source that fails to open" in {
    val opened = SdlLibrary.load(
      List(Source.Bundled(bundled), system),
      {
        case Source.Bundled(_) => Left("wrong ELF class")
        case other             => Right(other.toString)
      }
    )

    opened shouldBe Right((system, system.toString))
  }

  it should "report every failure when no source opens" in {
    val opened = SdlLibrary.load[String](
      List(Source.Bundled(bundled), system),
      {
        case Source.Bundled(file) => Left(s"cannot open $file")
        case Source.System(name)  => Left(s"$name: no such file")
      }
    )

    opened shouldBe Left(
      SdlError.LibraryUnavailable(List(s"cannot open $bundled", s"${SdlLibrary.FileName}: no such file"))
    )
  }
