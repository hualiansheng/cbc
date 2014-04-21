package cbc
import org.scalatest.FunSuite
import SafeTree._
import Names._

class ParseSuite extends FunSuite {
  def parse(code: String): cbc.Trees.Tree = {
    val source = new cbc.util.BatchSourceFile("", code)
    val parser = new cbc.parser.Parsers.SourceFileParser(source)
    parser.parse()
  }

  def parseExpr(code: String): cbc.Trees.Tree = {
    val source = new cbc.util.BatchSourceFile("", code)
    val parser = new cbc.parser.Parsers.SourceFileParser(source)
    parser.parseRule(_.expr())
  }

  def parsePat(code: String): cbc.Trees.Tree = {
    val source = new cbc.util.BatchSourceFile("", code)
    val parser = new cbc.parser.Parsers.SourceFileParser(source)
    parser.parseRule(_.patterns())
  }

  test("parse `42`") {
    val Term.Int(42) = parseExpr("42")
  }

  test("parse `42L`") {
    val Term.Long(42L) = parseExpr("42L")
  }

  test("parse `42.42f`") {
    val Term.Float(42.42f) = parseExpr("42.42f")
  }

  test("parse `42.42`") {
    val Term.Double(42.42) = parseExpr("42.42")
  }

  test("parse `'a'`") {
    val Term.Char('a') = parseExpr("'a'")
  }

  test("parse `\"abc\"`") {
    val Term.String("abc") = parseExpr("\"abc\"")
  }

  test("parse `'a`") {
    val Term.Symbol('a) = parseExpr("'a")
  }

  test("parse `true`") {
    val Term.Bool(true) = parseExpr("true")
  }

  test("parse `false`") {
    val Term.Bool(false) = parseExpr("false")
  }

  test("parse `null`") {
    val Term.Null() = parseExpr("null")
  }

  test("parse `if`") {
    val Term.If(Term.Int(42), Term.Bool(true), Term.Bool(false)) = parseExpr("if (42) true else false")
  }

  test("parse `tuple`") {
    val Term.Tuple(List(Term.Int(1), Term.Int(2))) = parseExpr("(1, 2)")
  }

  test("parse `while`") {
    val Term.While(Term.Bool(true), Term.Int(1)) = parseExpr("while (true) 1")
  }

  test("parse `add`") {
    val Term.Apply(Term.Select(Term.Int(1), TermName("$plus")), List(Term.Int(2))) = parseExpr("1+2")
  }

  test("parse `add & mul`") {
    val Term.Apply(Term.Select(Term.Apply(Term.Select(Term.Int(1), TermName("$plus")), List(Term.Apply(Term.Select(Term.Int(2), TermName("$times")), List(Term.Int(3))))), TermName("$plus")), List(Term.Int(4))) = parseExpr("1+2*3+4")
  }

  test("parse `complex arithmetic`") {
    val Term.Apply(Term.Select(Term.Int(2), TermName("$minus")), List(Term.Apply(Term.Select(Term.Apply(Term.Select(Term.Int(1), TermName("$plus")), List(Term.Apply(Term.Select(Term.Int(5), TermName("$times")), List(Term.Int(3))))), TermName("$div")), List(Term.Int(4))))) = parseExpr("2-(1+5*3)/4")
  }

  test("parse `pattern lit`") {
    val Pat.Lit(Term.Int(3)) = parsePat("3")
  }

  test("parse `pattern stableId`") {
    val Pat.Ident(Term.SuperSelect(TypeName(""), TypeName("Foo"), TermName("a"))) = parsePat("super[Foo].a")
  }

  test("parse `pattern typed`") {
    val Pat.Typed(Term.Ident(TermName("a")), Type.Empty) = parsePat("a:Empty")
  }

  test("parse `pattern binding`") {
    val Pat.Bind(Term.Ident(TermName("t")), Pat.Lit(Term.Double(22.22))) = parsePat("t @ 22.22")
  }

  test("parse `pattern extractor`") {
    val Pat.Extractor(Term.Ident(TermName("f")), List(Pat.Lit(Term.Int(1)), Pat.Lit(Term.Int(2)))) = parsePat("f(1,2)")
  }

}
