package com.serenity.manuscript

/** The one rule for page numbers and running heads, shared by the DOCX writer and the paginator so that the two outputs
  * cannot drift. It follows Shunn's novel format: the title page and dedication carry neither a number nor a head,
  * numbering starts at [[FirstBodyPage]] on the first page of body text, and every body page, a chapter's first
  * included, carries the running head.
  */
object ManuscriptPageNumbering:

  val FirstBodyPage: Int = 1

  /** The running-head placeholder for the page number; a DOCX writer turns it into a `PAGE` field. */
  val PageToken: String = "<$p>"

  /** The printed number of the body page at zero-based position `bodyIndex`. */
  def printedNumber(bodyIndex: Int): Int = FirstBodyPage + bodyIndex

  /** The head text with surname and keyword filled in and the page token left in place. A slash-separated segment left
    * empty (a manuscript with no surname) is dropped along with its separator.
    */
  def runningHead(template: String, meta: ManuscriptMeta): String =
    template
      .split(" / ", -1)
      .toList
      .map(_.replace("<$surname>", meta.author.surname).replace("<$keyword>", meta.shortTitle).trim)
      .filter(_.nonEmpty)
      .mkString(" / ")

  def runningHead(template: String, meta: ManuscriptMeta, number: Int): String =
    runningHead(template, meta).replace(PageToken, number.toString)
