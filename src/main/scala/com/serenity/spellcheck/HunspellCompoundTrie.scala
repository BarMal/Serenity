package com.serenity.spellcheck

/** One node of a `CompoundTrie`: `children` keyed by the next character, and `wordFlags` set only when a
  * compound-flagged dictionary word ends exactly at this node (the union of flags across every word with that exact
  * spelling, matching how `DictionaryLoader.mergeCompoundWordFlags` already merges duplicate entries).
  */
final case class CompoundTrieNode(children: Map[Char, CompoundTrieNode], wordFlags: Option[Set[String]])

object CompoundTrieNode:
  val empty: CompoundTrieNode = CompoundTrieNode(Map.empty, None)

/** A trie over the compound-flagged vocabulary, built once per dictionary load (issue #1198) and stored alongside
  * `DictionaryContext.compoundWordFlags` for the same lifetime. `HunspellFreeCompoundMatcher`'s DP walks it
  * character-by-character from each candidate position to find every valid compound member starting there in
  * `O(matched length)` with shared-prefix traversal, replacing a per-lookup hash-bucket-and-`startsWith` scan (the
  * approach `CompoundCandidateIndex` uses for COMPOUNDRULE -- which, per #1445, is now itself built once per dictionary
  * load rather than rebuilt per call, as #1415 had already flagged it should be).
  */
final case class CompoundTrie(root: CompoundTrieNode)

object CompoundTrie:

  val empty: CompoundTrie = CompoundTrie(CompoundTrieNode.empty)

  def build(words: Map[String, Set[String]]): CompoundTrie =
    CompoundTrie(words.foldLeft(CompoundTrieNode.empty) { case (root, (word, flags)) => insert(root, word, flags) })

  private def insert(node: CompoundTrieNode, remaining: String, flags: Set[String]): CompoundTrieNode =
    remaining.headOption match
      case None =>
        node.copy(wordFlags = Some(node.wordFlags.getOrElse(Set.empty) ++ flags))
      case Some(char) =>
        val child = node.children.getOrElse(char, CompoundTrieNode.empty)
        node.copy(children = node.children.updated(char, insert(child, remaining.tail, flags)))

  /** Every compound-flagged word `text` contains starting at `start`, as `(endExclusive, flags)` pairs -- one
    * character-by-character walk down `trie` shared across every word beginning there, rather than testing each
    * dictionary word independently.
    */
  def wordsAt(trie: CompoundTrie, text: String, start: Int): List[(Int, Set[String])] =
    def loop(node: CompoundTrieNode, pos: Int, acc: List[(Int, Set[String])]): List[(Int, Set[String])] =
      val withHere = node.wordFlags.fold(acc)(flags => (pos, flags) :: acc)
      if pos >= text.length then withHere
      else
        node.children.get(text.charAt(pos)) match
          case Some(child) => loop(child, pos + 1, withHere)
          case None        => withHere
    loop(trie.root, start, Nil)
