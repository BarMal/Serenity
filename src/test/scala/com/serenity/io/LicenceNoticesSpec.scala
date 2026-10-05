package com.serenity.io

import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{FileSystems, Files}

import scala.io.Source
import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The licence and third-party notices ship inside the application (#2019): GPL-3.0-or-later for Serenity itself, and
  * the notices its dependencies and bundled fonts require to travel with every copy.
  */
class LicenceNoticesSpec extends AnyFlatSpec with Matchers:

  private def resourceLines(path: String): List[String] =
    Option(getClass.getResourceAsStream(path)).fold(List.empty[String]) { stream =>
      val source = Source.fromInputStream(stream, "UTF-8")
      try source.getLines().toList
      finally source.close()
    }

  "The bundled licence" should "be the GNU General Public License, version 3" in {
    val licence = LicenceNotices.licence.unsafeRunSync()

    licence should include("GNU GENERAL PUBLIC LICENSE")
    licence should include("Version 3, 29 June 2007")
  }

  "The bundled notices" should "list every runtime module the build resolves" in {
    val runtimeModules = resourceLines("/licences/runtime-modules.txt").filter(_.nonEmpty)
    val notices        = LicenceNotices.notices.unsafeRunSync()

    runtimeModules should not be empty
    runtimeModules.filterNot(module => notices.contains(s"| $module |")) shouldBe empty
  }

  they should "carry the licence text of the Monaspace fonts and Material Icons" in {
    val notices = LicenceNotices.notices.unsafeRunSync()

    notices should include("Monaspace Neon")
    notices should include("SIL OPEN FONT LICENSE Version 1.1")
    notices should include("Material Icons Round")
  }

  "The fonts directory" should "ship the Monaspace OFL beside the font files" in {
    val licence = resourceLines("/fonts/OFL.txt").mkString("\n")

    licence should include("Reserved Font Name \"Monaspace\"")
    licence should include("SIL OPEN FONT LICENSE Version 1.1")
  }

  "The licence document" should "combine the licence and the notices under one read-only file" in {
    val directory = Files.createTempDirectory("licence-notices-spec")
    val written   = LicenceNotices.writeReadOnly(directory).unsafeRunSync()
    val rewritten = LicenceNotices.writeReadOnly(directory).unsafeRunSync()

    written shouldBe rewritten
    assume(FileSystems.getDefault.supportedFileAttributeViews.contains("posix"))
    Files.getPosixFilePermissions(written).asScala should not contain PosixFilePermission.OWNER_WRITE
    val content = Files.readString(written)
    content should include("GNU GENERAL PUBLIC LICENSE")
    content should include("# Third-party notices")
  }
