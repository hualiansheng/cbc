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

  /*test("parse `nested if`") {
    val Term.If(Term.If(Term.Int(42), Term.Bool(true), Term.Bool(false)), Term.If(Term.Int(42), Term.Bool(true), Term.Bool(false)), Term.If(Term.Int(42), Term.Bool(true), Term.Bool(false))) = parseExpr("if (if (42) true else false) {if (42) true else false} else {if (42) true else false}")
  }*/

  test("parse `while`") {
    val Term.While(Term.Bool(true), Term.Int(1)) = parseExpr("while (true) 1")
  }

  test("parse `add`") {
    val Term.Apply(Term.Select(Term.Int(1), TermName("$plus")), List(Term.Int(2))) = parseExpr("1+2")
  }

  test("parse `complex arithmetic`") {
    val Term.Apply(Term.Select(Term.Int(2), TermName("$minus")), List(Term.Apply(Term.Select(Term.Apply(Term.Select(Term.Int(1), TermName("$plus")), List(Term.Apply(Term.Select(Term.Int(5), TermName("$times")), List(Term.Int(3))))), TermName("$div")), List(Term.Int(4))))) = parseExpr("2-(1+5*3)/4")
  }

}
