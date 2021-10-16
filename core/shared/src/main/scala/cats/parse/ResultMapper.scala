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

import cats.Applicative
import cats.data.NonEmptyChain
import cats.parse.Chunks.{makeString, wrapString}
import cats.parse.ExpectationDecorator.{unsafeCastLeft, unsafeCastRight}
import cats.parse.Parser.Expectation
import cats.parse.StreamParser0.Output
import cats.syntax.all._
import fs2.Chunk
import fs2.Compiler.Target

import scala.annotation.unchecked.uncheckedVariance

private[parse]
sealed abstract class ResultMapper[-A, +B] {
  def output(source: Chunk[Char], a: A): B
  def outputAssembled(source: String, a: A): B
  def assemble(source: Chunk[Char])(implicit ev: String <:< A): B

  def void(implicit ev: Unit <:< A): ResultMapper[Any, B]
  def sourceString(implicit ev: String <:< A): ResultMapper[Any, B]

  def parseProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
    state: StreamState[F], decorator: ExpectationDecorator[G]
  )(
    implicit ev: (P, Q) <:< A
  ): StreamParser0.Output[F, G, B @uncheckedVariance]

  def parseSoftProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
    state: StreamState[F], decorator: ExpectationDecorator[G]
  )(
    implicit ev: (P, Q) <:< A
  ): StreamParser0.Output[F, G, B @uncheckedVariance]
}

private[parse]
object ResultMapper {
  sealed abstract class Voidable[-A, +B] extends ResultMapper[A, B] {
    def outputUnit(implicit ev: Unit <:< A): B
  }

  case object Void extends Voidable[Any, Unit] {
    override def output(source: Chunk[Char], a: Any): Unit = ()
    override def outputAssembled(source: String, a: Any): Unit = ()
    override def outputUnit(implicit ev: Unit <:< Any): Unit = ()
    override def assemble(source: Chunk[Char])(implicit ev: String <:< Any): Unit = ()

    override def void(implicit ev: Unit <:< Any): ResultMapper[Any, Unit] = this
    override def sourceString(implicit ev: String <:< Any): ResultMapper[Any, Unit] = this

    override def parseProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< Any
    ): F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], Unit])] =
      decorator match {
        case ExpectationDecorator.Identity =>
          first.parseStream(state, decorator, Void).flatMap {
            case (s, Right(())) =>
              second.parseStream(s, decorator, Void)
            case result =>
              Applicative[F].pure(
                result
              )
          }
        case ExpectationDecorator.Backtrack =>
          first.parseStream(state, decorator, SourceProduct(Void)).flatMap {
            case (s, Right((c, ()))) =>
              second.parseStream(s, ExpectationDecorator.Backtrack, Void).map {
                case (ss, Left((cc, e))) =>
                  (ss, Left(decorator(cc.cons(c), e)))
                case result =>
                  unsafeCastRight(result)
              }
            case result =>
              Applicative[F].pure(
                unsafeCastLeft(result)
              )
          }
      }

    override def parseSoftProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< Any
    ): F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], Unit])] =
      first.parseStream(state, decorator, SourceProduct(Void)).flatMap {
        case (s, Right((c, ()))) =>
          second.parseStream(s, decorator, Void).map {
            case (StreamState(offset, chars), e @ Left(_)) if offset == s.offset =>
              (StreamState(offset - c.size, chars.cons(c)), e)
            // TODO: Handle Backtrack
            case result =>
             result
          }
        case result =>
          Applicative[F].pure(
            unsafeCastLeft(result)
          )
      }
  }


  sealed abstract class AbstractIdentity[A] extends Voidable[A, A] {
    override def output(source: Chunk[Char], a: A): A = a
    override def outputAssembled(source: String, a: A): A = a
    override def outputUnit(implicit ev: Unit <:< A): A = ()
    override def assemble(source: Chunk[Char])(implicit ev: String <:< A): A = makeString(source)

    override def void(implicit ev: Unit <:< A): ResultMapper[Any, A] = Void.asInstanceOf[ResultMapper[Any, A]]
    override def sourceString(implicit ev: String <:< A): ResultMapper[Any, A] =
      SourceString.asInstanceOf[ResultMapper[Any, A]]

    override def parseProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< A
    ): F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], A])] =
      first.parseStream(state, decorator, Identity[P]).flatMap {
        case (s, Right(p)) =>
          second.parseStream(s, decorator, Identity[Q]).map(
            _.map(_.map((p, _)))
          )
        case result =>
          Applicative[F].pure(
            unsafeCastLeft(result)
          )
      }

    override def parseSoftProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< A
    ): F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], A])] = {
      ???
    }
  }

  case object Identity extends AbstractIdentity[Any] {
    def apply[A]: ResultMapper[A, A] = this.asInstanceOf[ResultMapper[A, A]]
  }


  case object SourceString extends ResultMapper[Any, String] {
    override def output(source: Chunk[Char], a: Any): String = makeString(source)
    override def outputAssembled(source: String, a: Any): String = source
    override def assemble(source: Chunk[Char])(implicit ev: String <:< Any): String = makeString(source)

    override def void(implicit ev: Unit <:< Any): ResultMapper[Any, String] = this
    override def sourceString(implicit ev: String <:< Any): ResultMapper[Any, String] = this

    override def parseProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< Any
    ): F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], String])] =
      first.parseStream(state, decorator, Source).flatMap {
        case (s, Right(p)) =>
          second.parseStream(s, decorator, Source).map(
            _.map(_.map(q => makeString(p ++ q)))
          )
        case result =>
          Applicative[F].pure(
            unsafeCastLeft(result)
          )
      }

    override def parseSoftProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< Any
    ): F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], String])] = ???
  }

  case object Source extends ResultMapper[Any, Chunk[Char]] {
    override def output(source: Chunk[Char], a: Any): Chunk[Char] = source
    override def outputAssembled(source: String, a: Any): Chunk[Char] = wrapString(source)
    override def assemble(source: Chunk[Char])(implicit ev: String <:< Any): Chunk[Char] = source

    override def void(implicit ev: Unit <:< Any): ResultMapper[Any, Chunk[Char]] = this
    override def sourceString(implicit ev: String <:< Any): ResultMapper[Any, Chunk[Char]] = this

    override def parseProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< Any
    ): F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], Chunk[Char]])] =
      first.parseStream(state, decorator, Source).flatMap {
        case (s, Right(p)) =>
          second.parseStream(s, decorator, Source).map(
            _.map(_.map(p ++ _))
          )
        case result =>
          Applicative[F].pure(
            result
          )
      }

    override def parseSoftProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< Any
    ): F[(StreamState[F], Either[G[NonEmptyChain[Expectation]], Chunk[Char]])] = ???
  }


  case class SourceProduct[-A, +B](delegate: ResultMapper[A, B]) extends ResultMapper[A, (Chunk[Char], B)] {
    override def output(source: Chunk[Char], a: A): (Chunk[Char], B) = (source, delegate.output(source, a))
    override def outputAssembled(source: String, a: A): (Chunk[Char], B) =
      {
        val chunk = wrapString(source)
        (chunk, delegate.output(chunk, a))
      }
    override def assemble(source: Chunk[Char])(implicit ev: String <:< A): (Chunk[Char], B) =
      (source, delegate.assemble(source))

    override def void(implicit ev: Unit <:< A): ResultMapper[Any, (Chunk[Char], B)] = SourceProduct(delegate.void)
    override def sourceString(implicit ev: String <:< A): ResultMapper[Any, (Chunk[Char], B)] =
      SourceProduct(delegate.sourceString)

    override def parseProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< A
    ): Output[F, G, (Chunk[Char], B @uncheckedVariance)] = ???

    override def parseSoftProduct[F[_] : Target, G[_], P, Q](first: Parser0[P], second: Parser0[Q])(
      state: StreamState[F], decorator: ExpectationDecorator[G]
    )(
      implicit ev: (P, Q) <:< A
    ): Output[F, G, (Chunk[Char], B @uncheckedVariance)] = ???
  }
}
