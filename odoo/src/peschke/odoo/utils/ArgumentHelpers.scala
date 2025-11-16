package peschke.odoo.utils

import cats.Show
import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all._
import com.monovore.decline.Argument

import scala.annotation.tailrec

object ArgumentHelpers {

  def const[A: Show](value: A): Argument[A] = const(value.show, value)

  def const[A](name: String, value: A): Argument[A] =
    Argument.from(s"'$name'") { raw =>
      if (raw.equalsIgnoreCase(name)) value.validNel
      else s"Expected '$name'".invalidNel
    }

  val boolean: Argument[Boolean] = Argument.from("t|f")(_.toLowerCase.trim match {
    case "0" | "f" | "off" | "false" => false.valid
    case "1" | "t" | "on" | "true" => true.valid
    case _ => "Expected one of 'true' (or 't', '1', or 'on') or 'false' (or 'f', '0', or 'off'".invalidNel
  })

  def oneOf[A](arg0: Argument[A], argN: Argument[A]*): Argument[A] =
    oneOf[A](NonEmptyChain.fromChainPrepend(arg0, Chain.fromSeq(argN)))

  def oneOf[A](arguments: NonEmptyChain[Argument[A]]): Argument[A] = {
    val metaVar = arguments.map(_.defaultMetavar).mkString_("|")
    Argument.from(metaVar) { raw =>
      arguments.map(_.read(raw)).reduceLeft(_.findValid(_))
    }
  }

  def pairArgument[A, B](metavarAOverride: Option[String], delimiter: String, metavarBOverride: Option[String])(implicit argA: Argument[A], argB: Argument[B]): Argument[(A, B)] = {
    val mva = {
      val m = metavarAOverride.getOrElse(argA.defaultMetavar)
      if (m.startsWith("'") && m.endsWith("'")) m
      else if (m.startsWith("\"") && m.endsWith("\"")) m
      else s"<$m>"
    }
    val mvb = {
      val m = metavarBOverride.getOrElse(argB.defaultMetavar)
      if (m.startsWith("'") && m.endsWith("'")) m
      else if (m.startsWith("\"") && m.endsWith("\"")) m
      else s"<$m>"
    }
    val metavar = s"$mva$delimiter$mvb"
    Argument.from(metavar) { raw =>
      val i = raw.indexOf(delimiter)
      if (raw.isEmpty) s"Expected $metavar, but was empty".invalidNel[(A, B)]
      else if (i == -1) s"In '$raw', missing expected '$delimiter' in $metavar".invalidNel[(A, B)]
      else {
        val rawA = raw.take(i)
        val rawB = raw.drop(i + delimiter.length)
        val validatedA =
          argA
            .read(rawA)
            .leftMap(e => e.mkString_(s"In '$raw', invalid $mva before '$delimiter':\n", "\n", "\n"))
            .toValidatedNel
        val validatedB =
          argB
            .read(rawB)
            .leftMap(e => e.mkString_(s"In '$raw', invalid $mvb after '$delimiter':\n", "\n", "\n"))
            .toValidatedNel

        (validatedA, validatedB).tupled
      }
    }
  }

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
