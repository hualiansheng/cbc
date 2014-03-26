/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package cbc

import cbc.util.{ SourceFile, NoSourceFile, FreshNameCreator }
import cbc.reporters.Reporter
import scala.collection.mutable
import scala.collection.mutable.{ LinkedHashSet, ListBuffer }

trait CompilationUnits { global: SymbolTable =>

  /** An object representing a missing compilation unit.
   */
  object NoCompilationUnit extends CompilationUnit(NoSourceFile) {
    override lazy val isJava = false
    override def exists = false
    override def toString() = "NoCompilationUnit"
  }

  /** One unit of compilation that has been submitted to the compiler.
    * It typically corresponds to a single file of source code.  It includes
    * error-reporting hooks.  */
  class CompilationUnit(val source: SourceFile) { self =>

    /** the fresh name creator */
    implicit val fresh: FreshNameCreator     = new FreshNameCreator
    def freshTermName(prefix: String = "x$") = global.freshTermName(prefix)
    def freshTypeName(prefix: String)        = global.freshTypeName(prefix)

    /** the content of the compilation unit in tree form */
    var body: Tree = EmptyTree

    /** The position of the first xml literal encountered while parsing this compilation unit.
     * NoPosition if there were none. Write-once.
     */
    private[this] var _firstXmlPos: Position = NoPosition

    /** Record that we encountered XML. Should only be called once. */
    def encounteredXml(pos: Position) = _firstXmlPos = pos

    /** Does this unit contain XML? */
    def hasXml = _firstXmlPos ne NoPosition

    /** Position of first XML literal in this unit. */
    def firstXmlPos = _firstXmlPos

    def exists = source != NoSourceFile && source != null

    // namer calls typer.computeType(rhs) on DefDef / ValDef when tpt is empty. the result
    // is cached here and re-used in typedDefDef / typedValDef
    // Also used to cache imports type-checked by namer.
    val transformed = new mutable.AnyRefMap[Tree, Tree]

    /** things to check at end of compilation unit */
    val toCheck = new ListBuffer[() => Unit]

    def position(pos: Int) = source.position(pos)

    /** The position of a targeted type check
     *  If this is different from NoPosition, the type checking
     *  will stop once a tree that contains this position range
     *  is fully attributed.
     */
    def targetPos: Position = NoPosition

    def reporter = global.reporter

    def echo(pos: Position, msg: String) =
      reporter.echo(pos, msg)

    def error(pos: Position, msg: String) =
      reporter.error(pos, msg)

    def warning(pos: Position, msg: String) =
      reporter.warning(pos, msg)

    def deprecationWarning(pos: Position, msg: String) =
      reporter.warning(pos, s"depraction warning: $msg")

    def incompleteInputError(pos: Position, msg:String) =
      reporter.incompleteInputError(pos, msg)

    def comment(pos: Position, msg: String) =
      reporter.comment(pos, msg)

    /** Is this about a .java source file? */
    lazy val isJava = source.file.name.endsWith(".java")

    override def toString() = source.toString()
  }
}
