package com.serenity.spellcheck

/** Correctly spelled British-English prose that the checker must leave unflagged (#1808), grouped by the tokenising or
  * lookup rule each group exercises. Every entry is a complete line of text, checked on its own.
  */
object EnGbProseCorpus:

  val affixes: List[String] = List(
    "organisation",
    "organisations",
    "reorganised",
    "realised",
    "colourful",
    "discoloured",
    "unrecognisable",
    "centres",
    "travelled",
    "travelling",
    "favourite",
    "behaviours",
    "apologised",
    "programmes",
    "neighbourhoods",
    "catalogued",
    "analysing",
    "judgement",
    "manoeuvring",
    "jewellery",
    "fulfilled",
    "greyish",
    "rethinking",
    "misunderstood"
  )

  val possessives: List[String] = List(
    "the author's notes",
    "the author’s notes",
    "the children's books",
    "the children’s books",
    "the writers' room",
    "the writers’ room",
    "the colour's depth",
    "the organisation’s aims"
  )

  val contractions: List[String] = List(
    "we don't know",
    "we don’t know",
    "she can't stay",
    "she can’t stay",
    "it won't matter",
    "it won’t matter",
    "then I'm leaving",
    "then I’m leaving",
    "you're right",
    "you’re right",
    "they've gone",
    "they’ve gone",
    "it's late",
    "it’s late",
    "at six o'clock",
    "at six o’clock"
  )

  val hyphenated: List[String] = List(
    "a well-known author",
    "a long-term plan",
    "an up-to-date draft",
    "my mother-in-law",
    "twenty-one chapters",
    "a self-aware narrator",
    "a state-of-the-art press",
    "they co-operate",
    "a half-hearted reply",
    "a colour-blind reader",
    "a re-examined claim",
    "a book-loving reader",
    "a well‐known author"
  )

  val sentenceStarts: List[String] = List(
    "Although it rained.",
    "Organisations change.",
    "Colours fade.",
    "Don't go.",
    "Don’t go.",
    "Realising this, she left.",
    "It was late. Travelling home took hours.",
    "“Honestly,” she said.",
    "‘Whatever,’ he replied."
  )

  val numbers: List[String] = List(
    "in the 1990s",
    "the 21st century",
    "her 3rd novel",
    "it cost £20",
    "at 10:30",
    "about 3.5 metres",
    "nearly 50% of readers",
    "a 19th-century house"
  )

  val urls: List[String] = List(
    "see https://example.com/writing/drafts for details",
    "see www.example.co.uk today",
    "email editor@example.co.uk now",
    "read [the guide](https://example.org/guides/style) first"
  )

  val all: List[String] = affixes ++ possessives ++ contractions ++ hyphenated ++ sentenceStarts ++ numbers ++ urls
