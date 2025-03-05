package peschke.odoo.models

import cats.Show
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all._

import scala.util.matching.Regex

final case class LabelFilter(textFilter: TextFilter)
final case class PickingNameFilter(textFilter: TextFilter)
final case class SourceLocationFilter(textFilter: TextFilter)
final case class DestinationLocationFilter(textFilter: TextFilter)
final case class ProductFilter(textFilter: TextFilter)

sealed trait TagFilter extends Product with Serializable
object TagFilter {
  final case class Exists(filter: TextFilter) extends TagFilter
  final case class ForAll(filter: TextFilter) extends TagFilter

  implicit val show: Show[TagFilter] = Show.show {
    case Exists(filter) => show"exists($filter)"
    case ForAll(filter) => show"forAll($filter)"
  }
}

final case class TemplateFilters
  (labelFilter: LabelFilter,
   tagFilter: TagFilter,
   sourceLocationFilter: SourceLocationFilter,
   destinationLocationFilter: DestinationLocationFilter,
   pickingNameFilter: PickingNameFilter,
   productFilter: ProductFilter
  )

sealed trait TextFilter extends Product with Serializable {
  def isTrivial: Boolean = this match {
    case TextFilter.False         => true
    case TextFilter.True          => true
    case TextFilter.Not(filter)   => filter.isTrivial
    case TextFilter.And(lhs, rhs) => lhs.isTrivial && rhs.isTrivial
    case TextFilter.Or(lhs, rhs)  => lhs.isTrivial && rhs.isTrivial
    case _                        => false
  }

  def check(text: String): Validated[TextFilter, TextFilter] =
    this match {
      case TextFilter.False                   => this.invalid
      case TextFilter.True                    => this.valid
      case TextFilter.Exact(value)            => if (value === text) this.valid else this.invalid
      case TextFilter.StartsWith(prefix)      => if (text.startsWith(prefix)) this.valid else this.invalid
      case TextFilter.Contains(substring)     => if (text.contains(substring)) this.valid else this.invalid
      case TextFilter.EndsWith(suffix)        => if (text.endsWith(suffix)) this.valid else this.invalid
      case TextFilter.Matches(regex)          => if (regex.matches(text)) this.valid else this.invalid
      case TextFilter.Not(filter)             => filter.check(text).swap.bimap(TextFilter.not, TextFilter.not)
      case TextFilter.And(lhs, rhs)           =>
        (lhs.check(text), rhs.check(text)) match {
          case (Invalid(subLhs), Invalid(subRhs)) => TextFilter.and(subLhs, subRhs).invalid
          case (Valid(subLhs), Valid(subRhs))     => TextFilter.and(subLhs, subRhs).valid
          case (Valid(subLhs), Invalid(_))        => subLhs.invalid
          case (Invalid(_), Valid(subRhs))        => subRhs.invalid
        }
      case TextFilter.Or(lhs, rhs)            =>
        (lhs.check(text), rhs.check(text)) match {
          case (Invalid(subLhs), Invalid(subRhs)) => TextFilter.or(subLhs, subRhs).invalid
          case (Valid(subLhs), Valid(subRhs))     => TextFilter.or(subLhs, subRhs).valid
          case (Valid(subLhs), Invalid(_))        => subLhs.valid
          case (Invalid(_), Valid(subRhs))        => subRhs.valid
        }
      case TextFilter.CaseInsensitive(filter) => filter.toLowerCase.check(text.toLowerCase)
    }

  def toLowerCase: TextFilter = this match {
    case TextFilter.False | TextFilter.True | TextFilter.Matches(_) => this
    case TextFilter.Exact(value)                                    => TextFilter.Exact(value.toLowerCase)
    case TextFilter.StartsWith(prefix)                              => TextFilter.StartsWith(prefix.toLowerCase)
    case TextFilter.Contains(substring)                             => TextFilter.Contains(substring.toLowerCase)
    case TextFilter.EndsWith(suffix)                                => TextFilter.EndsWith(suffix.toLowerCase)
    case TextFilter.Not(filter)                                     => TextFilter.Not(filter.toLowerCase)
    case TextFilter.And(lhs, rhs)                                   => TextFilter.And(lhs.toLowerCase, rhs.toLowerCase)
    case TextFilter.Or(lhs, rhs)                                    => TextFilter.Or(lhs.toLowerCase, rhs.toLowerCase)
    case TextFilter.CaseInsensitive(filter)                         => filter.toLowerCase
  }

  def matches(text: String): Option[TextFilter] = check(text).toOption
  def fails(text: String): Option[TextFilter] = check(text).swap.toOption
}
object TextFilter {
  case object False                                      extends TextFilter
  case object True                                       extends TextFilter
  final case class Exact(value: String)                  extends TextFilter
  final case class StartsWith(prefix: String)            extends TextFilter
  final case class Contains(substring: String)           extends TextFilter
  final case class EndsWith(suffix: String)              extends TextFilter
  final case class Matches(regex: Regex)                 extends TextFilter
  final case class Not(filter: TextFilter)               extends TextFilter
  final case class And(lhs: TextFilter, rhs: TextFilter) extends TextFilter
  final case class Or(lhs: TextFilter, rhs: TextFilter)  extends TextFilter
  final case class CaseInsensitive(filter: TextFilter)   extends TextFilter

  val falsy: TextFilter = False
  val truthy: TextFilter = True
  def exact(value: String): TextFilter = Exact(value)
  def starts(prefix: String): TextFilter = StartsWith(prefix)
  def contains(substring: String): TextFilter = Contains(substring)
  def ends(prefix: String): TextFilter = EndsWith(prefix)
  def matches(regex: Regex): TextFilter = Matches(regex)
  def not(filter: TextFilter): TextFilter = Not(filter)
  def and(lhs: TextFilter, rhs: TextFilter): TextFilter = And(lhs, rhs)
  def or(lhs: TextFilter, rhs: TextFilter): TextFilter = Or(lhs, rhs)
  def ci(filter: TextFilter): TextFilter = CaseInsensitive(filter)

  def validBareTextChar(c: Char): Boolean =
    c.isLetterOrDigit ||
      c === '.' ||
      c === '_' ||
      c === '-'

  def isValidBareText(s: String): Boolean = s.forall(validBareTextChar)

  implicit val show: Show[TextFilter] = Show.show {
    case False                   => "False"
    case True                    => "True"
    case Exact(value)            =>
      if (isValidBareText(value)) s"is:$value" else s"""is:"$value""""
    case StartsWith(prefix)      =>
      if (isValidBareText(prefix)) s"starts:$prefix" else s"""starts:"$prefix""""
    case Contains(substring)     =>
      if (isValidBareText(substring)) s"contains:$substring" else s"""contains:"$substring""""
    case EndsWith(prefix)        =>
      if (isValidBareText(prefix)) s"ends:$prefix" else s"""ends:"$prefix""""
    case Matches(regex)          => s"""matches:"$regex""""
    case Not(filter)             => s"not:(${show.show(filter)})"
    case And(lhs, rhs)           => s"(${show.show(lhs)}) and (${show.show(rhs)})"
    case Or(lhs, rhs)            => s"(${show.show(lhs)}) or (${show.show(rhs)})"
    case CaseInsensitive(filter) => s"ci:(${show.show(filter)})"
  }
}
