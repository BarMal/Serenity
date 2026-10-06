package com.serenity.spellcheck

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.serenity.config.SpellCheckConfig

/** A small `en_GB` Hunspell dictionary written to a temp directory. It reproduces the structure of the real LibreOffice
  * dictionary that matters to prose: `ICONV ’ '`, prefix/suffix flags that combine, possessive `'s` suffixes, entries
  * containing apostrophes and hyphens, and a stem with no entry for its inflected forms.
  */
object EnGbFixtureDictionary:

  private val affix: String =
    """SET UTF-8
      |TRY esianrtolcdugmphbyfvkwz'
      |ICONV 1
      |ICONV ’ '
      |NOSUGGEST !
      |
      |PFX A Y 1
      |PFX A 0 re .
      |
      |PFX U Y 1
      |PFX U 0 un .
      |
      |PFX X Y 1
      |PFX X 0 dis .
      |
      |SFX S Y 1
      |SFX S 0 s .
      |
      |SFX M Y 1
      |SFX M 0 's .
      |
      |SFX D Y 2
      |SFX D 0 d e
      |SFX D 0 ed [^e]
      |
      |SFX G Y 2
      |SFX G e ing e
      |SFX G 0 ing [^e]
      |
      |SFX I Y 1
      |SFX I 0 ing .
      |
      |SFX L Y 2
      |SFX L 0 led l
      |SFX L 0 ling l
      |
      |SFX B Y 1
      |SFX B e able e
      |
      |SFX F Y 1
      |SFX F 0 ful .
      |
      |SFX N Y 1
      |SFX N 0 ment .
      |
      |SFX Y Y 1
      |SFX Y 0 ish .
      |""".stripMargin

  private val words: List[String] = List(
    "organisation/MS",
    "organise/ADGS",
    "realise/DGS",
    "apologise/DG",
    "recognise/UB",
    "analyse/DG",
    "manoeuvre/DG",
    "colour/DFMSX",
    "behaviour/S",
    "centre/S",
    "programme/S",
    "neighbourhood/S",
    "catalogue/DG",
    "judge/DN",
    "favourite/S",
    "travel/L",
    "fulfil/L",
    "jewellery",
    "grey/Y",
    "think/AI",
    "understood",
    "misunderstood",
    "author/MS",
    "children/M",
    "writer/MS",
    "metre/S",
    "reader/S",
    "don't",
    "can't",
    "won't",
    "i'm",
    "you're",
    "they've",
    "it's",
    "o'clock",
    "co-operate",
    "well",
    "known",
    "long",
    "term",
    "date",
    "twenty",
    "one",
    "mother",
    "law",
    "self",
    "aware",
    "state",
    "the",
    "art",
    "half",
    "heart/D",
    "blind",
    "examine/D",
    "love/G",
    "although",
    "honestly",
    "whatever",
    "century",
    "house",
    "novel",
    "cost",
    "about",
    "nearly",
    "chapter/S",
    "narrator",
    "press",
    "reply",
    "claim",
    "plan",
    "draft/S",
    "detail/S",
    "today",
    "email",
    "now",
    "read",
    "guide",
    "first",
    "see",
    "for",
    "and",
    "was",
    "she",
    "you",
    "they",
    "can",
    "not",
    "know",
    "stay",
    "matter",
    "then",
    "leaving",
    "gone",
    "right",
    "late",
    "home",
    "took",
    "hour/S",
    "said",
    "replied",
    "go",
    "room",
    "depth",
    "aims",
    "wrote",
    "world",
    "word/S",
    "hello",
    "lot",
    "worked",
    "work/B",
    "write/G",
    "receive/DG",
    "friend/S",
    "their",
    "there",
    "note/S",
    "book/S",
    "six",
    "rain/D",
    "change",
    "fade",
    "this",
    "left",
    "her",
    "some",
    "after",
    "thing",
    "here",
    "a",
    "i"
  )

  def write(extraAffix: String = ""): Path =
    val directory = Files.createTempDirectory("serenity-en-gb-fixture")
    Files.writeString(directory.resolve("en_GB.aff"), affix + extraAffix, StandardCharsets.UTF_8)
    Files.writeString(
      directory.resolve("en_GB.dic"),
      s"${words.size}\n${words.mkString("\n")}\n",
      StandardCharsets.UTF_8
    )
    directory

  def config(directory: Path): SpellCheckConfig =
    SpellCheckConfig(enabled = true, languages = List("en-GB"), dictionaryPaths = List(directory.toString))

  def load(extraAffix: String = ""): (SpellCheckConfig, DictionaryContext) =
    val configured = config(write(extraAffix))
    configured -> DictionaryLoader.loadSnapshot(configured, DictionaryCache()).context
