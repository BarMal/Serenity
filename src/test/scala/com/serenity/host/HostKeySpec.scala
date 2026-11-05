package com.serenity.host

import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HostKeySpec extends AnyFlatSpec with Matchers:

  "HostKey" should "name a host directory with 16 lowercase hex characters" in {
    HostKey(Paths.get("/work/serenity")).id should fullyMatch regex "[0-9a-f]{16}"
  }

  it should "give the same id to the same root however the path is spelled" in {
    val canonical = HostKey(Paths.get("/work/serenity"))

    HostKey(Paths.get("/work/./serenity/")) shouldBe canonical
    HostKey(Paths.get("/work/other/../serenity")) shouldBe canonical
    HostKey(Paths.get("/work/serenity")).id shouldBe canonical.id
  }

  it should "give different roots different ids" in {
    HostKey(Paths.get("/work/a")).id should not be HostKey(Paths.get("/work/b")).id
  }

  it should "keep the root it was made from, made absolute and normalised" in {
    // On Windows "/work/serenity" has no drive, so making it absolute adds the current one: compare to that, not to
    // the literal path.
    HostKey(Paths.get("/work/./serenity")).root shouldBe Paths.get("/work/serenity").toAbsolutePath
  }

  it should "make a relative root absolute" in {
    HostKey(Paths.get("serenity")).root.isAbsolute shouldBe true
  }
