/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package cbc

import scala.annotation.elidable
import scala.collection.{ mutable, immutable }
import cbc.{TreeGen => InternalTreeGen}
import cbc.util._

abstract class SymbolTable extends Names
                              with StdNames
                              with FreshNames
                              with Trees
                              with Positions
                              with Constants
                              with CompilationUnits {
  val reporter: cbc.reporters.Reporter = new cbc.reporters.ConsoleReporter
  val gen = new { val global: this.type = this } with TreeGen
  val treeInfo = new { val global: this.type = this } with TreeInfo

  // Members declared in cbc.FreshNames
  def currentFreshNameCreator: cbc.util.FreshNameCreator = new cbc.util.FreshNameCreator

  // Members declared in cbc.Positions
  def inform(msg: String): Unit = ()

  def abort(msg: String): Nothing = throw new Exception("abort: $msg")

  /** Called from parser, which signals hereby that a method definition has been parsed. */
  def signalParseProgress(pos: Position) {}
}

