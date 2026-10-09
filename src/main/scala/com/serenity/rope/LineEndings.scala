package com.serenity.rope

import scala.annotation.tailrec

/** The one line-ending policy for text entering the editor. */
object LineEndings:

  /** CRLF and bare CR become LF, so downstream code only ever sees '\n'. */
  def normalized(in: String): String =
    if !in.exists(c => c == '\r' || c == '\n') then in
    else
      @tailrec
      def loop(index: Int, acc: StringBuilder): String =
        if index >= in.length then acc.toString
        else
          val char = in.charAt(index)
          if char == '\r' then
            acc.append('\n')
            val skipsFollowingLf = index + 1 < in.length && in.charAt(index + 1) == '\n'
            loop(if skipsFollowingLf then index + 2 else index + 1, acc)
          else loop(index + 1, acc.append(char))

      loop(0, new StringBuilder(in.length))
