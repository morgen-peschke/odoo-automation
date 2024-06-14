package peschke.odoo.models

import scala.util.matching.Regex

sealed trait LabelFilter extends Product with Serializable
object LabelFilter {
  final case class Exact(label: String) extends LabelFilter
  final case class StartsWith(prefix: String) extends LabelFilter
  final case class Contains(substring: String) extends LabelFilter
  final case class Matches(regex: Regex) extends LabelFilter
}
