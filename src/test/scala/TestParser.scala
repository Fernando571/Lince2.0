import lince.syntax.Lince.Expr.Num
import lince.syntax.Lince.Program.Assign

class TestParser extends munit.FunSuite {
  import lince.syntax.*
  import Lince.*
  
  test("Parsing an assignment") {
    val obtained =
      lince.syntax.Parser.parseProgram("x:=2;")
    val expected =
      Assign("x",Num(2))
    assertEquals(obtained, expected)
  }
}