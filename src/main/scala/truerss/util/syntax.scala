package truerss.util

import scala.util.Either

object syntax {

  type \/[L, R] = Either[L, R]

  final class OptionExt[T](val x: T) extends AnyVal {
    def some: Option[T] = Some(x)
  }
  trait WithOptionExt {
    implicit def WithOptionExt[T](x: T): OptionExt[T] = new OptionExt(x)
  }

  final class EitherExt[T](val x: T) extends AnyVal {
    def right[R]: Either[R, T] = Right(x)
    def left[R]: Either[T, R] = Left(x)
  }
  trait WithEitherExt {
    implicit def WithEitherExt[T](x: T): EitherExt[T] = new EitherExt(x)
  }

  object ext extends WithOptionExt with WithEitherExt

}
