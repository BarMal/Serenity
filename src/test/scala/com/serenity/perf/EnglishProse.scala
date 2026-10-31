package com.serenity.perf

import scala.util.Random

/** Deterministic English prose for benchmarks that write into a document with spell check on: every word is one the
  * dictionary accepts, bar the occasional misspelling injected on purpose, so squiggles appear at about the rate a
  * writer leaves them, not on nearly every word as lorem ipsum does.
  */
private[perf] object EnglishProse:

  final case class Document(text: String, misspelt: Int)

  private val seed = 20240611L

  private val sentences = Vector(
    "The morning light came slowly across the fields, and the village was quiet.",
    "She had always believed that a letter written by hand meant more than any message sent in a hurry.",
    "Nobody in the house remembered who had planted the apple tree, but everyone knew where to find the best fruit.",
    "We walked along the river until the path turned away from the water and climbed towards the hills.",
    "It was a different kind of silence from the one he had known in the city.",
    "Before the rain arrived, the children brought the washing in and set the table for supper.",
    "Through the window she could see her neighbour carrying a ladder across the garden.",
    "The old library smelled of dust and polish, and the floorboards creaked with every step.",
    "People often say that the first draft is the hardest, though the second is usually worse.",
    "He kept the map folded in his pocket, because he preferred asking for directions to being told he was lost.",
    "There were, as there always are, a great many reasons to leave and only one good reason to stay.",
    "The train was late again, so the passengers stood together on the platform and complained about the weather.",
    "A good friend will tell you when your argument is weak, and a better one will help you improve it.",
    "Because the road was closed, we took the long way home and stopped for tea in a small town.",
    "Every autumn the market filled with pumpkins, chestnuts and the bright scarves that her mother knitted.",
    "The committee met on Thursday evening, and nothing was decided except the date of the next meeting.",
    "Somewhere beyond the trees a bell was ringing, though no one could say which church it belonged to.",
    "I would rather read a short book twice than a long one once, and I have never regretted it.",
    "The engineer explained the plan slowly, drawing each step on the back of an envelope.",
    "On Sunday afternoons the whole street seemed to hold its breath, waiting for the week to begin again.",
    "He learned to cook by watching his grandmother, who never measured anything and was never wrong.",
    "The garden had grown wild over the summer, and the paths were hidden under long grass and clover.",
    "None of the answers were simple, but the questions were worth asking all the same.",
    "She wrote down every idea she had, however small, and kept the notebooks on a shelf by the door.",
    "The ferry crossed the bay twice a day, carrying post, bread and the occasional visitor.",
    "When the lamps were lit the square looked smaller, and the voices from the cafe sounded closer.",
    "We argued about the best route for an hour and then followed the one that the signpost recommended.",
    "Their conversation moved easily from the price of coal to the habits of swans.",
    "It takes a long time to understand a place, and longer still to be understood by it.",
    "The teacher handed back the essays with a single comment on each, written in careful green ink.",
    "Behind the shop there was a yard full of crates, where the cat slept through most of the afternoon.",
    "A small boat drifted past the harbour wall, and the fishermen watched it go without a word.",
    "He promised to write every week, and for a while he kept his promise.",
    "The soup was too hot to eat, so they talked until it cooled and then forgot to finish it.",
    "Over the years the house had been painted many colours, and each one still showed faintly through the next.",
    "She thought about the question all evening, and in the morning the answer seemed obvious.",
    "There is a particular pleasure in finding a quiet seat on a crowded train.",
    "The mountains were further away than they looked, and the afternoon was already fading when we arrived.",
    "Everyone agreed that the concert was wonderful, although no two people could agree on the best song."
  )

  /** The misspelling of a word, for the words in the sentences that a typing slip could plausibly mangle. */
  private val slips = Map(
    "morning"    -> "mornign",
    "believed"   -> "beleived",
    "different"  -> "diferent",
    "through"    -> "thorugh",
    "people"     -> "poeple",
    "because"    -> "becuase",
    "together"   -> "togther",
    "friend"     -> "freind",
    "which"      -> "wich",
    "before"     -> "befor",
    "arrived"    -> "arrvied",
    "village"    -> "vilage",
    "answer"     -> "anwser",
    "afternoon"  -> "afternon",
    "question"   -> "quesiton",
    "evening"    -> "evning",
    "weather"    -> "wether",
    "remembered" -> "rememberd",
    "listened"   -> "listend",
    "wonderful"  -> "wonderfull"
  )

  def document(paragraphs: Int, misspellingEvery: Int): Document =
    val random = Random(seed)
    val written = Vector.fill(paragraphs)(
      Vector.fill(4 + random.nextInt(5))(sentences(random.nextInt(sentences.size))).mkString(" ").split(" ").toVector
    )
    val (text, misspelt) = written.foldLeft((Vector.empty[String], Slipping.start(misspellingEvery))) {
      case ((paragraphsDone, slipping), words) =>
        val (rewritten, next) =
          words.foldLeft((Vector.empty[String], slipping))((acc, word) => acc._2.next(acc._1, word))
        (paragraphsDone :+ rewritten.mkString(" "), next)
    } match
      case (paragraphTexts, slipping) => (paragraphTexts.mkString("\n\n"), slipping.count)
    Document(text, misspelt)

  /** Counts words and, every `every` words, owes a misspelling that is paid by the next word that has one. */
  final private case class Slipping(every: Int, seen: Int, owed: Boolean, count: Int):

    def next(done: Vector[String], word: String): (Vector[String], Slipping) =
      val seenNow = seen + 1
      val owing   = owed || (every > 0 && seenNow % every == 0)
      val letters = word.takeWhile(_.isLetter)
      slips.get(letters) match
        case Some(slip) if owing =>
          (done :+ (slip + word.drop(letters.length)), copy(seen = seenNow, owed = false, count = count + 1))
        case _ => (done :+ word, copy(seen = seenNow, owed = owing))

  private object Slipping:
    def start(every: Int): Slipping = Slipping(every, seen = 0, owed = false, count = 0)
