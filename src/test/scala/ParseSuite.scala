package cbc
import org.scalatest.FunSuite

class ParseSuite extends FunSuite {
  def parse(code: String): cbc.Trees.Tree = {
    val source = new cbc.util.BatchSourceFile("", code)
    val parser = new cbc.parser.Parsers.SourceFileParser(source)
    parser.parse()
  }

  def parseExpr(code: String): cbc.Trees.Tree = {
    val source = new cbc.util.BatchSourceFile("", code)
    val parser = new cbc.parser.Parsers.SourceFileParser(source)
    parser.parseRule(_ => parser.expr())
  }

  test("parse `42`") {
    val SafeTree.Term.Int(42) = parseExpr("42")
  }

  test("parse `42L`") {
    val SafeTree.Term.Long(42L) = parseExpr("42L")
  }

  test("parse `42.42f`") {
    val SafeTree.Term.Float(42.42f) = parseExpr("42.42f")
  }

  test("parse `42.42`") {
    val SafeTree.Term.Double(42.42) = parseExpr("42.42")
  }

  test("parse `'a'`") {
    val SafeTree.Term.Char('a') = parseExpr("'a'")
  }

  test("parse `\"abc\"`") {
    val SafeTree.Term.String("abc") = parseExpr("\"abc\"")
  }

  test("parse `'a`") {
    val SafeTree.Term.Symbol('a) = parseExpr("'a")
  }

  test("parse `true`") {
    val SafeTree.Term.Bool(true) = parseExpr("true")
  }

  test("parse `null`") {
    val SafeTree.Term.Null() = parseExpr("null")
  }

}
