package cbc
import org.scalatest.FunSuite

class ParseSuite extends FunSuite {
  def parse(code: String): cbc.Trees.Tree = {
    val source = new cbc.util.BatchSourceFile("", code)
    val parser = new cbc.parser.Parsers.SourceFileParser(source)
    parser.parse()
  }

  test("parse `object O`") {
    assert(parse("object O").nonEmpty)
  }
}
