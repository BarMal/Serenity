package com.serenity.io

import java.nio.file.Path

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GitIgnoreSpec extends AnyFlatSpec with Matchers:

  private def ignores(patterns: String, path: String, isDirectory: Boolean = false): Boolean =
    IgnoreRules.empty.withLayer("", GitIgnore.parse(patterns)).ignores(path, isDirectory)

  "GitIgnore" should "skip blank lines and comments, and take an escaped # or ! literally" in {
    ignores("\n# comment\n\n", "comment") shouldBe false
    ignores("\\#hash", "#hash") shouldBe true
    ignores("\\!bang", "!bang") shouldBe true
  }

  it should "match a pattern without a slash at any depth, by name" in {
    ignores("*.log", "a.log") shouldBe true
    ignores("*.log", "x/y/a.log") shouldBe true
    ignores("*.log", "a.logx") shouldBe false
    ignores("secret", "x/secret") shouldBe true
  }

  it should "anchor a pattern with a leading or inner slash to the ignore file's directory" in {
    ignores("/foo", "foo") shouldBe true
    ignores("/foo", "x/foo") shouldBe false
    ignores("a/b", "a/b") shouldBe true
    ignores("a/b", "x/a/b") shouldBe false
  }

  it should "match only directories for a pattern with a trailing slash" in {
    ignores("build/", "build", isDirectory = true) shouldBe true
    ignores("build/", "x/build", isDirectory = true) shouldBe true
    ignores("build/", "build") shouldBe false
  }

  it should "never let * or ? cross a slash" in {
    ignores("doc/*.txt", "doc/a.txt") shouldBe true
    ignores("doc/*.txt", "doc/x/a.txt") shouldBe false
    ignores("a?c", "abc") shouldBe true
    ignores("a?c", "a/c") shouldBe false
  }

  it should "match bracket expressions, ranges and their negation" in {
    ignores("file[ab].txt", "filea.txt") shouldBe true
    ignores("file[ab].txt", "filec.txt") shouldBe false
    ignores("file[a-c].txt", "filec.txt") shouldBe true
    ignores("file[!a].txt", "filea.txt") shouldBe false
    ignores("file[!a].txt", "fileb.txt") shouldBe true
    ignores("odd[", "odd[") shouldBe true
  }

  it should "let ** cross any number of directories" in {
    ignores("**/foo", "foo") shouldBe true
    ignores("**/foo", "a/b/foo") shouldBe true
    ignores("abc/**", "abc/x/y") shouldBe true
    ignores("abc/**", "abc", isDirectory = true) shouldBe false
    ignores("a/**/b", "a/b") shouldBe true
    ignores("a/**/b", "a/x/y/b") shouldBe true
    ignores("a/**/b", "x/a/b") shouldBe false
  }

  it should "let the last matching rule win, so a later negation re-includes and an earlier one does not" in {
    ignores("*.log\n!keep.log", "keep.log") shouldBe false
    ignores("*.log\n!keep.log", "other.log") shouldBe true
    ignores("!keep.log\n*.log", "keep.log") shouldBe true
  }

  it should "ignore unescaped trailing spaces and carriage returns, but keep an escaped one" in {
    ignores("foo   ", "foo") shouldBe true
    ignores("foo\r\n", "foo") shouldBe true
    ignores("foo\\ ", "foo ") shouldBe true
    ignores("foo\\ ", "foo") shouldBe false
  }

  "IgnoreRules" should "scope a layer to its directory and let deeper layers override shallower ones" in {
    val rules = IgnoreRules.empty
      .withLayer("", GitIgnore.parse("*.log\n"))
      .withLayer("pkg", GitIgnore.parse("local.txt\n!keep.log\n"))

    rules.ignores("pkg/local.txt", isDirectory = false) shouldBe true
    rules.ignores("pkg/inner/local.txt", isDirectory = false) shouldBe true
    rules.ignores("local.txt", isDirectory = false) shouldBe false
    rules.ignores("other/local.txt", isDirectory = false) shouldBe false
    rules.ignores("pkg/keep.log", isDirectory = false) shouldBe false
    rules.ignores("keep.log", isDirectory = false) shouldBe true
    rules.ignores("pkgx/local.txt", isDirectory = false) shouldBe false
  }

  "A git config" should "name the global excludes file, with ~/ expanded against the home directory" in {
    val home = Path.of("/home/me")

    GitIgnore.excludesFileFrom("[core]\n\texcludesfile = ~/.gitignore_global\n", home) shouldBe
      Some(home.resolve(".gitignore_global"))
    GitIgnore.excludesFileFrom("[core]\n\texcludesFile = /etc/ignore\n", home) shouldBe Some(Path.of("/etc/ignore"))
    GitIgnore.excludesFileFrom("[user]\n\tname = me\n", home) shouldBe None
  }
