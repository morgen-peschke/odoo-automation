package peschke.odoo.algebras

import cats.syntax.all._
import io.circe.Decoder
import peschke.odoo.utils.Circe._

class TemplateDecoderTest extends munit.FunSuite {
  test("Loading from parent") {
    val json = io
      .circe.parser.parse {
        """{ "value": "parent",
          |  "values": [
          |    {},
          |    { "value": "child" }
          |  ]
          |}""".stripMargin
      }.valueOr(fail("Invalid Json", _))

    final case class Value(value: String)
    final case class Test(values: List[Value])

    implicit val valueDecoder: Decoder[Value] = accumulatingDecoder { c =>
      c.downField("value").asAcc[String].orElse {
          c.up.up.downField("value").asAcc[String]
        }.map(Value)
    }
    implicit val testDecoder: Decoder[Test] = accumulatingDecoder { c =>
      c.downField("values").asAcc[List[Value]].map(Test)
    }

    assertEquals(
      json.hcursor.asAcc[Test],
      Test(Value("parent") :: Value("child") :: Nil).valid
    )
  }
}
