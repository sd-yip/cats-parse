/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.parse

import fs2.Chunk.{ArraySlice, CharBuffer}
import fs2.{Chunk, INothing, Pull, Stream}

import java.lang.Character.{toLowerCase, toUpperCase}
import java.nio.CharBuffer.wrap
import scala.annotation.tailrec
import scala.reflect.ClassTag

private[parse]
object Chunks {
  def unconsN[F[_], O](
    stream: Stream[F, O], n: Int, allowFewer: Boolean
  ): Pull[F, INothing, (Chunk[O], Stream[F, O])] =
    stream.pull.unconsN(n, allowFewer).map {
      case None => (Chunk.empty, Stream.empty)
      case Some(t) => t
    }

  def unconsIndexedN[F[_], O : ClassTag](
    stream: Stream[F, O], n: Int, allowFewer: Boolean
  ): Pull[F, INothing, (Chunk[O], Stream[F, O])] =
    stream.pull.unconsN(n, allowFewer).map {
      case None => (Chunk.empty, Stream.empty)
      case Some((head: Chunk.Queue[O], tail)) => (head.compact, tail)
      case Some(t) => t
    }

  def wrapString(string: String): Chunk[Char] =
    string match {
      case "" => Chunk.empty
      case _ => Chunk.charBuffer(wrap(string))
    }

  def makeString(chunk: Chunk[Char]): String =
    chunk match {
      case _ if chunk.getClass == classOf[CharBuffer] =>
        chunk.asInstanceOf[CharBuffer].buf.toString

      case _ if chunk.getClass == classOf[ArraySlice[_]] =>
        val a = chunk.asInstanceOf[ArraySlice[Char]]
        new String(a.values, a.offset, a.length)

      case c: Chunk.Singleton[Char] =>
        Character.toString(c.value)

      case _ =>
        new String(chunk.toArray)
    }

  def equalElements(s1: Chunk[Char], s2: String): Boolean = {
    @tailrec
    def go(n: Int)(offset: Int): Boolean =
      n == 0 || s1(offset) == s2(offset) && go(n - 1)(1 + offset)

    val n = s2.length
    s1.size == n && go(n)(0)
  }

  def equalElementsIgnoreCase(s1: Chunk[Char], s2: String): Boolean = {
    @tailrec
    def go(n: Int)(offset: Int): Boolean =
      n match {
        case 0 => true
        case _ =>
          val c1 = s1(offset)
          val c2 = s2(offset)

          if (c1 == c2) {
            go(n - 1)(1 + offset)
          } else {
            val u1 = toUpperCase(c1)
            val u2 = toUpperCase(c2)

            (u1 == u2 || toLowerCase(u1) == toLowerCase(u2)) && go(n - 1)(1 + offset)
          }
      }

    val n = s2.length
    s1.size == n && go(n)(0)
  }
}
