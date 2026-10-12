package com.serenity.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Section-name matching, with cases from the EditorConfig core test suite (its glob tests). */
class EditorConfigGlobSpec extends AnyFlatSpec with Matchers:

  private def matching(glob: String, path: String): Boolean = EditorConfigGlob.compile(glob).matches(path)

  "A star" should "match any run of characters within one segment, at any depth" in {
    matching("*", "a.c") shouldBe true
    matching("*.py", "src/deep/a.py") shouldBe true
    matching("*.py", "a.pyc") shouldBe false
    matching("a*e.c", "ae.c") shouldBe true
    matching("a*e.c", "Tex.c") shouldBe false
  }

  it should "not cross a separator in a name that has one" in {
    matching("a/*.c", "a/b.c") shouldBe true
    matching("a/*.c", "a/b/c.c") shouldBe false
  }

  "A double star" should "cross separators" in {
    matching("a**z.c", "a/z.c") shouldBe true
    matching("a**z.c", "amnz.c") shouldBe true
    matching("a**z.c", "am/nz.c") shouldBe true
    matching("a**z.c", "a/mnz.c") shouldBe true
    matching("**.c", "a/b/c.c") shouldBe true
    matching("a/**/z.c", "a/b/c/z.c") shouldBe true
  }

  "A question mark" should "match one character other than a separator" in {
    matching("???.c", "abc.c") shouldBe true
    matching("???.c", "ab.c") shouldBe false
    matching("a?b.c", "a/b.c") shouldBe false
  }

  "A character class" should "match one of its characters or ranges" in {
    matching("[ab].c", "a.c") shouldBe true
    matching("[ab].c", "c.c") shouldBe false
    matching("[a-c].c", "b.c") shouldBe true
    matching("[a-c].c", "d.c") shouldBe false
  }

  it should "match anything else but a separator when negated" in {
    matching("[!ab].c", "c.c") shouldBe true
    matching("[!ab].c", "a.c") shouldBe false
    matching("a[!b]c", "a/c") shouldBe false
  }

  it should "be a plain bracket when it never closes or holds a separator" in {
    matching("a[b.c", "a[b.c") shouldBe true
    matching("ab[e/]cd.i", "ab[e/]cd.i") shouldBe true
    matching("ab[e/]cd.i", "abe/cd.i") shouldBe false
  }

  "Braces" should "match any of their comma separated alternatives" in {
    matching("*.{py,js,html}", "a.js") shouldBe true
    matching("*.{py,js,html}", "a.css") shouldBe false
    matching("{a,b}.c", "b.c") shouldBe true
    matching("{,a}.c", ".c") shouldBe true
  }

  it should "nest" in {
    matching("{a,{b,c}d}.x", "cd.x") shouldBe true
    matching("{a,{b,c}d}.x", "c.x") shouldBe false
  }

  it should "stand for themselves when they hold a single choice" in {
    matching("{single}.b", "{single}.b") shouldBe true
    matching("{single}.b", "single.b") shouldBe false
  }

  it should "stand for themselves when never closed" in {
    matching("{a,b.c", "{a,b.c") shouldBe true
  }

  it should "match an integer within an inclusive range" in {
    matching("{3..120}", "3") shouldBe true
    matching("{3..120}", "120") shouldBe true
    matching("{3..120}", "121") shouldBe false
    matching("{3..120}", "2") shouldBe false
    matching("{-3..3}", "-2") shouldBe true
    matching("{-3..3}", "-4") shouldBe false
    matching("file{1..3}.txt", "file2.txt") shouldBe true
    matching("file{1..3}.txt", "file4.txt") shouldBe false
  }

  it should "check each of several ranges" in {
    matching("{1..2}x{5..6}", "2x6") shouldBe true
    matching("{1..2}x{5..6}", "2x7") shouldBe false
    matching("{a,{1..2}}x{5..6}", "1x7") shouldBe false
    matching("{a,{1..2}}x{5..6}", "1x5") shouldBe true
  }

  "A backslash" should "make the next character ordinary" in {
    matching("a\\*b", "a*b") shouldBe true
    matching("a\\*b", "axb") shouldBe false
    matching("{a\\,b,c}", "a,b") shouldBe true
    matching("\\[a].c", "[a].c") shouldBe true
  }

  "A name with a separator" should "be anchored to the file's directory" in {
    matching("src/*.c", "src/a.c") shouldBe true
    matching("src/*.c", "x/src/a.c") shouldBe false
    matching("/a.c", "a.c") shouldBe true
    matching("/a.c", "x/a.c") shouldBe false
  }

  "A name with no separator" should "match at the directory level and below" in {
    matching("a.c", "a.c") shouldBe true
    matching("a.c", "x/y/a.c") shouldBe true
    matching("a.c", "xa.c") shouldBe false
  }

  "A name that is not a valid expression" should "match nothing" in {
    matching("[z-a]", "a") shouldBe false
  }

end EditorConfigGlobSpec
