package com.serenity.ui.renderer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Java2DScratchBuffersSpec extends AnyFlatSpec with Matchers:

  "Java2DScratchBuffers.withImage" should "hand back the same image for the same size once it has been returned" in {
    val scratch = Java2DScratchBuffers()
    val first   = scratch.withImage(30, 20)(identity)
    val second  = scratch.withImage(30, 20)(identity)

    second should be theSameInstanceAs first
    (second.getWidth, second.getHeight) shouldBe (30, 20)
  }

  it should "never hand the same image to two borrowers at once" in {
    val scratch        = Java2DScratchBuffers()
    val (outer, inner) = scratch.withImage(30, 20)(outer => (outer, scratch.withImage(30, 20)(identity)))

    inner should not be theSameInstanceAs(outer)
  }

  it should "allocate an image of the requested size when only other sizes are pooled" in {
    val scratch = Java2DScratchBuffers()
    val small   = scratch.withImage(10, 10)(identity)
    val large   = scratch.withImage(40, 30)(identity)

    large should not be theSameInstanceAs(small)
    (large.getWidth, large.getHeight) shouldBe (40, 30)
  }

  it should "keep pooling newly seen sizes once the pool is full" in {
    val scratch = Java2DScratchBuffers()
    (1 to 20).foreach(size => scratch.withImage(size, size)(identity))
    val latest = scratch.withImage(20, 20)(identity)

    scratch.withImage(20, 20)(identity) should be theSameInstanceAs latest
  }
