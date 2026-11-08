package com.serenity.lsp

import com.serenity.lsp.client.LspGlob
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/** The glob syntax of LSP 3.17 `workspace/didChangeWatchedFiles` registrations (#2531 W1). Cases follow the spec's own
  * description: `*` and `?` stay within a segment, `**` spans any number of segments including none, `{}` groups
  * alternatives, `[]` is a character range and `[!...]` its negation.
  */
class LspGlobSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

  private val cases = Table(
    ("pattern", "path", "matches"),
    ("*.ts", "a.ts", true),
    ("*.ts", "src/a.ts", false),
    ("**/*.ts", "a.ts", true),
    ("**/*.ts", "src/deep/a.ts", true),
    ("**/*.{ts,js}", "src/a.js", true),
    ("**/*.{ts,js}", "src/a.jsx", false),
    ("src/**/test", "src/test", true),
    ("src/**/test", "src/a/b/test", true),
    ("src/**/test", "lib/test", false),
    ("src/**", "src/a/b.c", true),
    ("**", "anything/at/all", true),
    ("a?c", "abc", true),
    ("a?c", "a/c", false),
    ("example.[0-9]", "example.0", true),
    ("example.[0-9]", "example.a", false),
    ("example.[!0-9]", "example.a", true),
    ("example.[!0-9]", "example.0", false),
    ("**/build.sbt", "build.sbt", true),
    ("**/build.sbt", "sub/build.sbt", true),
    ("**/build.sbt", "sub/build.sbt.bak", false),
    ("{src,test}/**/*.scala", "test/x/A.scala", true),
    ("{src,test}/**/*.scala", "docs/A.scala", false),
    ("**/*.{c,h{,pp}}", "a/b.hpp", true),
    ("**/*.{c,h{,pp}}", "a/b.h", true),
    ("a.b", "aXb", false),
    ("a+b(c)", "a+b(c)", true)
  )

  "LspGlob" should "match the path forms the LSP 3.17 glob syntax describes" in
    forAll(cases)((pattern, path, expected) =>
      withClue(s"$pattern against $path: ")(LspGlob.compile(pattern).matches(path) shouldBe expected)
    )

  it should "treat an unclosed group or range as literal text rather than fail" in {
    LspGlob.compile("a{b").matches("a{b") shouldBe true
    LspGlob.compile("a[b").matches("a[b") shouldBe true
    LspGlob.compile("a[b").matches("ab") shouldBe false
  }

  it should "keep the pattern it was compiled from" in {
    LspGlob.compile("**/*.ts").pattern shouldBe "**/*.ts"
  }
