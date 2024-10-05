package peschke.odoo.algebras

import cats.Show
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.syntax.all._
import munit.Location
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink
import peschke.odoo.models.TextFilter

class TextFilterParserTest extends ScalaCheckSuite {

  private def assertEq[A: Show, B: Show]
    (obtained:     A, expected:  B, clue: => Any = "values are not the same") // scalafix:ok DisableSyntax.defaultArgs
    (implicit loc: Location, ev: B <:< A)
    : Unit =
    assertEquals[A, B](obtained, expected, clues(clue, obtained.show, expected.show))(loc, ev)

  private val parser = TextFilterParser.default[Either[String, *], String] { e =>
    val header =
      Chain(
        e.input.map(str => s"Input: $str"),
        s"Index: ${e.failedAtOffset}".some,
        e.input.map { str =>
          val prefix = if (e.failedAtOffset === 0) "" else "..."
          val slice = str.slice(e.failedAtOffset, e.failedAtOffset + 20)
          val suffix = if (slice.length === 20) "..." else ""
          s"Focus: $prefix$suffix$slice"
        }
      ).flatMap(Chain.fromOption)

    val expectations =
      NonEmptyChain.fromNonEmptyList(e.expected.map(_.show)).toChain

    header
      .append("")
      .append("Expected:")
      .concat(expectations)
      .mkString_("\n")
  }

  private val validBareStrings: Gen[String] =
    Gen.chooseNum(1, 20).flatMap { n =>
      Gen.stringOfN(n, Gen.oneOf(Gen.alphaNumChar, Gen.oneOf[Char]("_.-")))
    }

  private val validQuotableStrings: Gen[String] =
    Gen.chooseNum(1, 20).flatMap { n =>
      Gen.stringOfN(n, Gen.asciiPrintableChar.filterNot(_ === '"'))
    }

  private def genWithMixedCase(value: String): Gen[String] =
    Gen.frequency(
      10 -> Gen.oneOf(value :: value.toUpperCase :: value.toLowerCase :: Nil),
      1 ->
        Gen
          .sequence[List[Char], Char](
            value
              .toSeq.map(c =>
                arbitrary[Boolean].map {
                  case true  => c.toLower
                  case false => c.toUpper
                }
              )
          )
          .map(_.mkString)
    )

  private val terminalFilters: Gen[(String, TextFilter)] = Gen.oneOf(
    Gen.oneOf(
      "true" -> TextFilter.truthy,
      "false" -> TextFilter.falsy
    ),
    validBareStrings.flatMap { input =>
      Gen.oneOf(
        List(
          s"is:$input" -> TextFilter.exact(input),
          s"starts:$input" -> TextFilter.starts(input),
          s"contains:$input" -> TextFilter.contains(input),
          s"ends:$input" -> TextFilter.ends(input)
        )
      )
    },
    validQuotableStrings.flatMap { input =>
      Gen.oneOf(
        List(
          s"""is:"$input"""" -> TextFilter.exact(input),
          s"""starts:"$input"""" -> TextFilter.starts(input),
          s"""contains:"$input"""" -> TextFilter.contains(input),
          s"""ends:"$input"""" -> TextFilter.ends(input)
        )
      )
    }
  )

  // Terminal Filters

  test("TextFilter.default should parse 'false'") {
    forAllNoShrink(genWithMixedCase("false")) { input =>
      assertEq(parser.parse(input), TextFilter.falsy.asRight[String])
    }
  }

  test("TextFilter.default should parse 'true'") {
    forAllNoShrink(genWithMixedCase("true")) { input =>
      assertEq(parser.parse(input), TextFilter.truthy.asRight[String])
    }
  }

  test("TextFilter.default should parse 'is:unquoted'") {
    forAllNoShrink(validBareStrings) { input =>
      assertEq(parser.parse(s"is:$input"), TextFilter.exact(input).asRight[String])
    }
  }

  test("TextFilter.default should parse 'is:quoted'") {
    forAllNoShrink(validQuotableStrings) { input =>
      assertEq(parser.parse(s"""is:"$input""""), TextFilter.exact(input).asRight[String])
    }
    assertEq(
      parser.parse("""is:"<\">""""),
      """Input: is:"<\">"
        |Index: 7
        |Focus: ...>"
        |
        |Expected:
        |must end the string""".stripMargin.asLeft[TextFilter],
      clue("Escapes are not supported")
    )
  }

  test("TextFilter.default should parse 'starts:unquoted'") {
    forAllNoShrink(validBareStrings) { input =>
      assertEq(parser.parse(s"starts:$input"), TextFilter.starts(input).asRight[String])
    }
  }

  test("TextFilter.default should parse 'starts:quoted'") {
    forAllNoShrink(validQuotableStrings) { input =>
      assertEq(parser.parse(s"""starts:"$input""""), TextFilter.starts(input).asRight[String])
    }
  }

  test("TextFilter.default should parse 'contains:unquoted'") {
    forAllNoShrink(validBareStrings) { input =>
      assertEq(parser.parse(s"contains:$input"), TextFilter.contains(input).asRight[String])
    }
  }

  test("TextFilter.default should parse 'contains:quoted'") {
    forAllNoShrink(validQuotableStrings) { input =>
      assertEq(parser.parse(s"""contains:"$input""""), TextFilter.contains(input).asRight[String])
    }
  }

  test("TextFilter.default should parse 'ends:unquoted'") {
    forAllNoShrink(validBareStrings) { input =>
      assertEq(parser.parse(s"ends:$input"), TextFilter.ends(input).asRight[String])
    }
  }

  test("TextFilter.default should parse 'ends:quoted'") {
    forAllNoShrink(validQuotableStrings) { input =>
      assertEq(parser.parse(s"""ends:"$input""""), TextFilter.ends(input).asRight[String])
    }
  }

  // Unary Op Filters

  test("TestFilter.default should parse 'not:filter'") {
    forAllNoShrink(terminalFilters) { case (input, filter) =>
      assertEq(parser.parse(s"not:$input"), TextFilter.not(filter).asRight[String])
    }
  }

  test("TestFilter.default should parse '!filter'") {
    forAllNoShrink(terminalFilters) { case (input, filter) =>
      assertEq(parser.parse(s"!$input"), TextFilter.not(filter).asRight[String])
    }
  }

  test("TestFilter.default should permit parens around Not filter") {
    forAllNoShrink(terminalFilters) { case (input, filter) =>
      assertEq(parser.parse(s"not:$input"), TextFilter.not(filter).asRight[String], clue("Unchanged input"))
      assertEq(parser.parse(s"not:($input)"), TextFilter.not(filter).asRight[String])
    }
  }

  // Binary Op Filters

  test("TestFilter.default should parse 'lhs and rhs' and 'lhs & rhs'") {
    forAllNoShrink(
      terminalFilters,
      Gen.frequency(
        1 -> Gen.const("&"),
        5 -> genWithMixedCase("and")
      ),
      terminalFilters
    ) { case ((lhsInput, lhs), op, (rhsInput, rhs)) =>
      assertEq(parser.parse(s"$lhsInput $op $rhsInput"), TextFilter.and(lhs, rhs).asRight[String])
    }
  }

  test("TestFilter.default should parse 'a & b & c'") {
    forAllNoShrink(terminalFilters, terminalFilters, terminalFilters) { case ((aInput, a), (bInput, b), (cInput, c)) =>
      assertEq(
        parser.parse(s"$aInput & $bInput & $cInput"),
        TextFilter.and(a, TextFilter.and(b, c)).asRight[String]
      )
    }
  }

  test("TestFilter.default should permit parens around And filter") {
    forAllNoShrink(terminalFilters, terminalFilters, terminalFilters) { case ((aInput, a), (bInput, b), (cInput, c)) =>
      assertEq(
        parser.parse(s"$aInput & $bInput & $cInput"),
        TextFilter.and(a, TextFilter.and(b, c)).asRight[String],
        clue("Unchanged input")
      )
      assertEq(parser.parse(s"($aInput) & $bInput"), TextFilter.and(a, b).asRight[String])
      assertEq(parser.parse(s"$aInput & ($bInput)"), TextFilter.and(a, b).asRight[String])
      assertEq(parser.parse(s"($aInput) & ($bInput)"), TextFilter.and(a, b).asRight[String])

      assertEq(
        parser.parse(s"($aInput) & $bInput & $cInput"),
        TextFilter.and(a, TextFilter.and(b, c)).asRight[String]
      )
      assertEq(
        parser.parse(s"$aInput & ($bInput) & $cInput"),
        TextFilter.and(a, TextFilter.and(b, c)).asRight[String]
      )
      assertEq(
        parser.parse(s"$aInput & $bInput & ($cInput)"),
        TextFilter.and(a, TextFilter.and(b, c)).asRight[String]
      )
      assertEq(
        parser.parse(s"($aInput) & ($bInput) & ($cInput)"),
        TextFilter.and(a, TextFilter.and(b, c)).asRight[String]
      )

      assertEq(
        parser.parse(s"($aInput & $bInput) & $cInput"),
        TextFilter.and(TextFilter.and(a, b), c).asRight[String]
      )
      assertEq(
        parser.parse(s"$aInput & ($bInput & $cInput)"),
        TextFilter.and(a, TextFilter.and(b, c)).asRight[String]
      )
    }
  }

  test("TestFilter.default should parse 'lhs or rhs' and 'lhs | rhs'") {
    forAllNoShrink(
      terminalFilters,
      Gen.frequency(
        1 -> Gen.const("|"),
        5 -> genWithMixedCase("or")
      ),
      terminalFilters
    ) { case ((lhsInput, lhs), op, (rhsInput, rhs)) =>
      assertEq(parser.parse(s"$lhsInput $op $rhsInput"), TextFilter.or(lhs, rhs).asRight[String])
    }
  }

  test("TestFilter.default should parse 'a | b | c'") {
    forAllNoShrink(terminalFilters, terminalFilters, terminalFilters) { case ((aInput, a), (bInput, b), (cInput, c)) =>
      assertEq(
        parser.parse(s"$aInput | $bInput | $cInput"),
        TextFilter.or(a, TextFilter.or(b, c)).asRight[String]
      )
    }
  }

  test("TestFilter.default should parse 'a & b | c' and 'a | b & c'") {
    forAllNoShrink(terminalFilters, terminalFilters, terminalFilters) { case ((aInput, a), (bInput, b), (cInput, c)) =>
      assertEq(
        parser.parse(s"$aInput & $bInput | $cInput"),
        TextFilter.and(a, TextFilter.or(b, c)).asRight[String]
      )
      assertEq(
        parser.parse(s"$aInput | $bInput & $cInput"),
        TextFilter.or(a, TextFilter.and(b, c)).asRight[String]
      )
    }
  }
}
