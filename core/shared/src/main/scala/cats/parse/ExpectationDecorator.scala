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

import cats.data.NonEmptyChain
import cats.parse.Parser.Expectation
import cats.syntax.all._
import cats.{Comonad, Id}
import fs2.Compiler.Target
import fs2.{Chunk, Pure, Stream}

private[parse]
sealed abstract class ExpectationDecorator[F[_]] {
  val comonad: Comonad[F]

  def wrap(source: Chunk[Char], expectation: Expectation): F[NonEmptyChain[Expectation]]
  def empty(expectation: Expectation): F[NonEmptyChain[Expectation]]
  def apply(source: Stream[Pure, Char], expectations: NonEmptyChain[Expectation]): F[NonEmptyChain[Expectation]]

  def parseBacktracked[G[_] : Target, A, B](parser: Parser0[A])(
    state: StreamState[G], mapper: ResultMapper[A, B]
  ): G[(StreamState[G], Either[F[NonEmptyChain[Expectation]], B])]
}

private[parse]
object ExpectationDecorator {
  def unsafeCastLeft[A, B](p: (A, Either[B, Any])): (A, Left[B, Nothing]) =
    p.asInstanceOf[(A, Left[B, Nothing])]

  def unsafeCastRight[A, B](p: (A, Either[Any, B])): (A, Right[Nothing, B]) =
    p.asInstanceOf[(A, Right[Nothing, B])]


  case object Identity extends ExpectationDecorator[Id] {
    override val comonad: Comonad[Id] = implicitly

    override def wrap(source: Chunk[Char], expectation: Expectation): NonEmptyChain[Expectation] =
      NonEmptyChain.one(expectation)
    override def empty(expectation: Expectation): NonEmptyChain[Expectation] =
      NonEmptyChain.one(expectation)
    override def apply(
      source: Stream[Pure, Char], expectations: NonEmptyChain[Expectation]
    ): NonEmptyChain[Expectation] =
      expectations

    override def parseBacktracked[G[_] : Target, A, B](parser: Parser0[A])(
      state: StreamState[G], mapper: ResultMapper[A, B]
    ): G[(StreamState[G], Either[NonEmptyChain[Expectation], B])] =
      parser.parseStream(state, Backtrack, mapper).map {
        case (s, Left((consumed, expectations))) =>
          (StreamState(state.offset, consumed ++ s.chars), Left(expectations))
        case t =>
          unsafeCastRight(t)
      }
  }

  case object Backtrack extends ExpectationDecorator[(Stream[Pure, Char], *)] {
    override val comonad: Comonad[(Stream[Pure, Char], *)] = implicitly

    override def wrap(source: Chunk[Char], expectation: Expectation): (Stream[Pure, Char], NonEmptyChain[Expectation]) =
      (Stream.chunk(source), NonEmptyChain.one(expectation))
    override def empty(expectation: Expectation): (Stream[Pure, Char], NonEmptyChain[Expectation]) =
      (Stream.empty, NonEmptyChain.one(expectation))
    override def apply(
      source: Stream[Pure, Char], expectations: NonEmptyChain[Expectation]
    ): (Stream[Pure, Char], NonEmptyChain[Expectation]) =
      (source, expectations)

    override def parseBacktracked[G[_] : Target, A, B](parser: Parser0[A])(
      state: StreamState[G], mapper: ResultMapper[A, B]
    ): G[(StreamState[G], Either[(Stream[Pure, Char], NonEmptyChain[Expectation]), B])] =
      parser.parseStream(state, Backtrack, mapper).map {
        case (s, Left((consumed, expectations))) =>
          (StreamState(state.offset, consumed ++ s.chars), Left((Stream.empty, expectations)))
        case t =>
          t
      }
  }
}
