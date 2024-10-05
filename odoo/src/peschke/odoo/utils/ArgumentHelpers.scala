package peschke.odoo.utils

import cats.data.Chain
import cats.syntax.all._
import com.monovore.decline.Argument

import scala.annotation.tailrec

object ArgumentHelpers {
  def enumArgument[E <: enumeratum.EnumEntry](implicit E: enumeratum.Enum[E]): Argument[E] = {
    val names = E.values.map(_.entryName)
    val uniquePrefixes = calculateUnambiguousPrefixes[E]
    val renderedPrefixes = uniquePrefixes._1F.mkString(", ")
    Argument.from(names.mkString("|")) { raw =>
      E.withNameInsensitiveOption(raw)
        .orElse {
          val lowerRaw = raw.toLowerCase
          uniquePrefixes.find { case (prefix, _) =>
            lowerRaw.startsWith(prefix)
          }._2F
        }
        .toValidNel(names.mkString("Expected one of: ", ", ", s" (or a unique prefix: $renderedPrefixes)"))
    }
  }

  private[utils] def calculateUnambiguousPrefixes[E <: enumeratum.EnumEntry]
    (implicit E: enumeratum.Enum[E])
    : Vector[(String, E)] = {
    @tailrec
    def loop(current: Chain[(Chain[Char], (Chain[Char], E))], accum: Chain[(Chain[Char], E)]): Vector[(String, E)] = {
      val (canGrowNoLonger, grown) =
        current.map { case (prefix, (suffix, value)) =>
          suffix
            .uncons
            .map { case c -> newSuffix =>
              (prefix.append(c), (newSuffix, value))
            }
            .toRight(prefix -> value)
        }.separate

      val ready = accum.concat(canGrowNoLonger)
      val prefixes = ready.concat(grown)._1F
      val uniques = prefixes.distinct.sorted
      if (prefixes.length =!= uniques.length && grown.nonEmpty) loop(grown, ready)
      else
        ready
          .concat(grown.map { case (prefix, (_, value)) => prefix -> value })
          .map { case prefix -> value => prefix.mkString_("") -> value }
          .toVector
    }

    loop(
      Chain.fromSeq(E.values).map(e => (Chain.empty[Char], (Chain.fromSeq(e.entryName.toLowerCase), e))),
      Chain.empty
    )
  }
}
