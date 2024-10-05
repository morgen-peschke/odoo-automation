package peschke.odoo.algebras

import cats.MonadError
import cats.data.NonEmptyList
import cats.parse.Parser.Expectation
import cats.parse.Parser.Expectation.FailWith
import cats.parse.Rfc5234
import cats.parse.{Parser => P}
import cats.syntax.all._
import peschke.odoo.models.TextFilter

import scala.annotation.tailrec

trait TextFilterParser[F[_]] {

  /** Parse a TextFilter
    *
    * Note: does not attempt to parse regular expressions
    */
  def parse(expression: String): F[TextFilter]
}
object TextFilterParser {
  def default[F[_], E](mapError: P.Error => E)(implicit F: MonadError[F, E]): TextFilterParser[F] =
    new TextFilterParser[F] {
      private val quoteP: P[Unit] = P.char('"')

      private val bareTextP: P[String] =
        P.charsWhile(TextFilter.validBareTextChar).withContext("[A-Za-z0-9._-]")

      private val escapedTextP: P[String] =
        quoteP *> P.until(quoteP) <* quoteP

      private val textP: P[String] = escapedTextP.orElse(bareTextP)

      private val falseP: P[TextFilter] =
        P.ignoreCase("false").as(TextFilter.False)

      private val trueP: P[TextFilter] =
        P.ignoreCase("true").as(TextFilter.True)

      private val exactP: P[TextFilter] =
        (P.ignoreCase("is:") *> textP).map(TextFilter.Exact)

      private val startsWithP: P[TextFilter] =
        P.ignoreCase("starts:") *> textP.map(TextFilter.StartsWith)

      private val containsP: P[TextFilter] =
        P.ignoreCase("contains:") *> textP.map(TextFilter.Contains)

      private val endsWithP: P[TextFilter] =
        P.ignoreCase("ends:") *> textP.map(TextFilter.EndsWith)

      private val tokenParser: P[NonEmptyList[Token]] = {
        val terminalToken =
          (P.index.with1 ~ P.oneOf(
            List(
              trueP,
              falseP,
              exactP,
              startsWithP,
              containsP,
              endsWithP
            )
          ) ~ P.index.?).map { case ((s, f), e) =>
            Token.Terminal(s, f, e)
          }

        val placeholderTokens =
          (P.index.with1 ~ P.oneOf(
            List(
              P.ignoreCase("not:").orElse(P.char('!')).as(Token.Not.apply _),
              P.oneOf(
                List(
                  P.ignoreCase("and").orElse(P.char('&')).as(Token.And.apply _),
                  P.ignoreCase("or").orElse(P.char('|')).as(Token.Or.apply _)
                )
              ).surroundedBy(Rfc5234.wsp),
              P.char('(').as(Token.OpenParen.apply _),
              P.char(')').as(Token.CloseParen.apply _)
            )
          ) ~ P.index.?).map { case ((s, f), e) => f(s, e) }

        terminalToken.orElse(placeholderTokens).rep
      }

      private def tokenize(expression: String): F[NonEmptyList[Token]] =
        tokenParser.parseAll(expression).leftMap(mapError).liftTo[F]

      private def infixToRPN(tokens: NonEmptyList[Token], expression: String): F[NonEmptyList[ReversePolishToken]] = {
        def error(index: Int, expectation: Expectation): E =
          mapError(P.Error(expression, index, NonEmptyList.one[Expectation](expectation)))

        def raise[A](index: Int, expectation: Expectation): F[A] = error(index, expectation).raiseError[F, A]

        @tailrec
        def loop
          (pending: List[Token], outputStack: List[ReversePolishToken], operatorStack: List[OperatorToken])
          : F[List[ReversePolishToken]] = {
          pending match {
            case Nil           =>
              @tailrec
              def clearOperatorStack
                (remaining: List[OperatorToken], accum: List[ReversePolishToken])
                : F[List[ReversePolishToken]] =
                remaining match {
                  case Nil          => accum.reverse.pure[F]
                  case head :: tail =>
                    head match {
                      case and: Token.And         => clearOperatorStack(tail, and :: accum)
                      case or: Token.Or           => clearOperatorStack(tail, or :: accum)
                      case not: Token.Not         => clearOperatorStack(tail, not :: accum)
                      case paren: Token.OpenParen =>
                        raise(paren.start, FailWith(paren.start, "Unclosed paren"))
                    }
                }

              clearOperatorStack(operatorStack, outputStack)
            case token :: rest =>
              token match {
                case terminal: Token.Terminal   =>
                  loop(rest, terminal :: outputStack, operatorStack)
                case operator: Token.And        => loop(rest, outputStack, operator :: operatorStack)
                case operator: Token.Or         => loop(rest, outputStack, operator :: operatorStack)
                case operator: Token.Not        => loop(rest, outputStack, operator :: operatorStack)
                case operator: Token.OpenParen  => loop(rest, outputStack, operator :: operatorStack)
                case Token.CloseParen(start, _) =>
                  @tailrec
                  def findParen
                    (newOutputStack: List[ReversePolishToken], newOperatorStack: List[OperatorToken])
                    : (List[ReversePolishToken], List[OperatorToken]) =
                    newOperatorStack match {
                      case Nil                        => (newOutputStack, newOperatorStack)
                      case head :: remainingOperators =>
                        head match {
                          case and: Token.And      =>
                            findParen(and :: newOutputStack, remainingOperators)
                          case or: Token.Or        =>
                            findParen(or :: newOutputStack, remainingOperators)
                          case not: Token.Not      =>
                            findParen(not :: newOutputStack, remainingOperators)
                          case op: Token.OpenParen =>
                            (newOutputStack, op :: remainingOperators)
                        }
                    }

                  val (newOutputStack, newOperatorStack) = findParen(outputStack, operatorStack)
                  if (newOperatorStack.isEmpty) raise(start, FailWith(start, "Unmatched closing paren"))
                  else if (newOutputStack.isEmpty) raise(start, FailWith(start, "Empty parens, is a filter missing?"))
                  else loop(rest, newOutputStack, newOperatorStack.drop(1))
              }
          }
        }

        loop(tokens.toList, Nil, Nil).flatMap {
          NonEmptyList.fromList(_).liftTo[F](error(0, FailWith(0, "Empty expression")))
        }
      }

      private def rpnToFilter(tokens: NonEmptyList[ReversePolishToken], expression: String): F[TextFilter] = {
        def error(index: Int, expectation: Expectation): E =
          mapError(P.Error(expression, index, NonEmptyList.one[Expectation](expectation)))

        def raise[A](index: Int, expectation: Expectation): F[A] = error(index, expectation).raiseError[F, A]

        @tailrec
        def loop(pending: List[ReversePolishToken], buffer: List[Token.Terminal]): F[TextFilter] =
          pending match {
            case Nil                          =>
              buffer match {
                case head :: Nil => head.filter.pure[F]
                case Nil         => raise(0, FailWith(0, "Empty expression"))
                case filters     =>
                  raise(
                    filters.map(_.start).min,
                    FailWith(
                      filters.map(f => f.end.getOrElse(f.start)).max,
                      "Multiple adjacent filters"
                    )
                  )
              }
            case rpnToken :: remainingPending =>
              rpnToken match {
                case terminal: Token.Terminal =>
                  loop(remainingPending, terminal :: buffer)
                case not: Token.Not           =>
                  buffer match {
                    case terminal :: remainingFilters =>
                      loop(remainingPending, not(terminal) :: remainingFilters)
                    case _                            => raise(not.start, FailWith(not.start, "Missing argument for NOT"))
                  }
                case and: Token.And           =>
                  buffer match {
                    case rhs :: lhs :: remainingFilters =>
                      loop(remainingPending, and(lhs, rhs) :: remainingFilters)
                    case _                              => raise(and.start, FailWith(and.start, "Missing argument(s) for AND"))
                  }
                case or: Token.Or             =>
                  buffer match {
                    case rhs :: lhs :: remainingFilters =>
                      loop(remainingPending, or(lhs, rhs) :: remainingFilters)
                    case _                              => raise(or.start, FailWith(or.start, "Missing argument(s) for OR"))
                  }
              }
          }

        loop(tokens.toList, Nil)
      }

      override def parse(expression: String): F[TextFilter] =
        tokenize(expression)
          .flatMap(infixToRPN(_, expression))
          .flatMap(rpnToFilter(_, expression))
    }

  private sealed trait Token {
    def start: Int
    def end: Option[Int]

    def minStart(other: Int): Int =
      start.min(other)

    def maxEnd(other: Option[Int]): Option[Int] = {
      val compared = for {
        te <- end
        oe <- other
      } yield te.max(oe)

      compared.orElse(end).orElse(other)
    }

  }
  private sealed trait ReversePolishToken { _: Token => }
  private sealed trait OperatorToken      { _: Token => }

  private object Token {
    final case class Terminal(start: Int, filter: TextFilter, end: Option[Int]) extends Token with ReversePolishToken

    final case class And(start: Int, end: Option[Int]) extends Token with ReversePolishToken with OperatorToken {
      def apply(lhs: Terminal, rhs: Terminal): Terminal =
        Terminal(
          lhs.minStart(rhs.minStart(start)),
          TextFilter.and(lhs.filter, rhs.filter),
          lhs.maxEnd(rhs.maxEnd(end))
        )
    }

    final case class Or(start: Int, end: Option[Int]) extends Token with ReversePolishToken with OperatorToken {
      def apply(lhs: Terminal, rhs: Terminal): Terminal =
        Terminal(
          lhs.minStart(rhs.minStart(start)),
          TextFilter.or(lhs.filter, rhs.filter),
          lhs.maxEnd(rhs.maxEnd(end))
        )
    }

    final case class Not(start: Int, end: Option[Int]) extends Token with ReversePolishToken with OperatorToken {
      def apply(other: Terminal): Terminal =
        Terminal(
          other.minStart(start),
          TextFilter.not(other.filter),
          other.maxEnd(end)
        )
    }

    final case class OpenParen(start: Int, end: Option[Int]) extends Token with OperatorToken

    final case class CloseParen(start: Int, end: Option[Int]) extends Token
  }
}
