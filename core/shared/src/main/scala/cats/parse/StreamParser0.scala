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
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.parse.Parser.Expectation
import fs2.Compiler.Target
import fs2.Stream

import scala.annotation.unchecked.uncheckedVariance

private[parse]
object StreamParser0 {
  type Output[F[_], G[_], B] = F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], B])]
}

private[parse]
trait StreamParser0[+A] {
  // Effectively `F[+_]`
  final def parse[F[_] : Sync](
    stream: Stream[F, Char]
  ): F[Either[Parser.Error, (Stream[F, Char], A @uncheckedVariance)]] =
    parseStream(stream)

  // Effectively `F[+_]`
  final def parse[F[_] : Concurrent](
    stream: Stream[F, Char]
  ): F[Either[Parser.Error, (Stream[F, Char], A @uncheckedVariance)]] =
    parseStream(stream)


  // Effectively `F[+_]`
  private
  def parseStream[F[_] : Target](
    stream: Stream[F, Char]
  ): F[Either[Parser.Error, (Stream[F, Char], A @uncheckedVariance)]] =
    parseStream(StreamState(0, stream), ExpectationDecorator.Identity, ResultMapper.Identity[A]).map {
      case (StreamState(offset, chars), result) =>
        result.bimap(
          expectations => Parser.Error(offset, Parser.Expectation.unify(expectations.extract.toNonEmptyList)),
          (chars, _)
        )
    }

  private[parse] def parseStream[F[_] : Target, G[_], B](
    state: StreamState[F], decorator: ExpectationDecorator[G], mapper: ResultMapper[A, B]
  ): F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], B])]
}
