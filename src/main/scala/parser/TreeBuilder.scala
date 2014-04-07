/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package cbc.parser

import scala.collection.mutable.ListBuffer
import cbc.{TreeGen, TreeInfo, nme, tpnme}
import cbc.Flags._
import cbc.Trees._
import cbc.Constants._
import cbc.Names._
import cbc.FreshNames.{freshTermName, freshTypeName}
import cbc.Positions._
import cbc.util.{Position, SourceFile, FreshNameCreator}
import cbc.SafeTree

/** Methods for building trees, used in the parser.  All the trees
 *  returned by this class must be untyped.
 */
abstract class TreeBuilder {
  def source: SourceFile
  implicit def fresh: FreshNameCreator

  def o2p(offset: Int): Position                    = Position.offset(source, offset)
  def r2p(start: Int, mid: Int, end: Int): Position = rangePos(source, start, mid, end)

  def rootScalaDot(name: Name) = TreeGen.rootScalaDot(name)
  def scalaDot(name: Name)     = TreeGen.scalaDot(name)
  def scalaAnyRefConstr        = scalaDot(tpnme.AnyRef)
  def scalaUnitConstr          = scalaDot(tpnme.Unit)

  def convertToTypeName(t: Tree) = TreeGen.convertToTypeName(t)

  def byNameApplication(tpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(tpnme.BYNAME_PARAM_CLASS_NAME), List(tpe))
  def repeatedApplication(tpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(tpnme.REPEATED_PARAM_CLASS_NAME), List(tpe))

  def makeImportSelector(name: Name, nameOffset: Int): ImportSelector =
    ImportSelector(name, nameOffset, name, nameOffset)

  def makeTupleTerm(elems: List[Tree]) = TreeGen.mkTuple(elems)

  def makeTupleType(elems: List[Tree]) = TreeGen.mkTupleType(elems)

  def stripParens(t: Tree) = t match {
    case Parens(ts) => atPos(t.pos) { makeTupleTerm(ts) }
    case _ => t
  }

  def stripParens(t: cbc.SafeTree.Term) = t match {
    //case cbc.SafeTree.Parent(ts) => cbc.SafeTree.Term.Tuple(ts)
    case _ => t
  }

  def makeAnnotated(t: Tree, annot: Tree): Tree =
    atPos(annot.pos union t.pos)(Annotated(annot, t))

  def makeSelfDef(name: TermName, tpt: Tree): ValDef =
    ValDef(Modifiers(PRIVATE), name, tpt, EmptyTree)

  /** Create tree representing (unencoded) binary operation expression or pattern. */
/*  
  def makeBinop(isExpr: Boolean, left: Tree, op: TermName, right: Tree, opPos: Position, targs: List[Tree] = Nil): Tree = {
    require(isExpr || targs.isEmpty || targs.exists(_.isErroneous), s"Incompatible args to makeBinop: !isExpr but targs=$targs")

    def mkSelection(t: Tree) = {
      def sel = atPos(opPos union t.pos)(Select(stripParens(t), op.encode))
      if (targs.isEmpty) sel else atPos(left.pos)(TypeApply(sel, targs))
    }
    def mkNamed(args: List[Tree]) = if (isExpr) args map TreeInfo.assignmentToMaybeNamedArg else args
    val arguments = right match {
      case Parens(args) => mkNamed(args)
      case _            => List(right)
    }
    if (isExpr) {
      if (TreeInfo.isLeftAssoc(op)) {
        Apply(mkSelection(left), arguments)
      } else {
        val x = freshTermName()
        Block(
          List(ValDef(Modifiers(SYNTHETIC | ARTIFACT), x, TypeTree(), stripParens(left))),
          Apply(mkSelection(right), List(Ident(x))))
      }
    } else {
      Apply(Ident(op.encode), stripParens(left) :: arguments)
    }
  }
*/  
  def makeBinop(isExpr: Boolean, left: SafeTree.Term, op: TermName, right: SafeTree.Term, targs: List[SafeTree.Type] = Nil): SafeTree.Term = {
    require(isExpr || targs.isEmpty || targs.exists(_.isErroneous), s"Incompatible args to makeBinop: !isExpr but targs=$targs")

    def mkSelection(t: SafeTree.Term) = {
      def sel = SafeTree.Term.Select(stripParens(t), op.encode)
      if (targs.isEmpty) sel else SafeTree.Term.TypeApply(sel, targs)
    }
    //def mkNamed(args: List[Tree]) = if (isExpr) args map TreeInfo.assignmentToMaybeNamedArg else args
    val arguments = right match {
      //case Parens(args) => mkNamed(args)
      case _            => List(right)
    }
    if (isExpr) {
      if (TreeInfo.isLeftAssoc(op)) {
        SafeTree.Term.Apply(mkSelection(left), arguments)
      } else {
        /*val x = freshTermName()
        SafeTree.Term.Block(
          List(ValDef(Modifiers(SYNTHETIC | ARTIFACT), x, TypeTree(), stripParens(left))),
          SafeTree.Term.Apply(mkSelection(right), List(SafeTree.Term.Ident(x))))*/
        SafeTree.Term.Empty()
      }
    } else {
      SafeTree.Term.Apply(SafeTree.Term.Ident(op.encode), stripParens(left) :: arguments)
    }
  }


  /** Tree for `od op`, start is start0 if od.pos is borked. */
  def makePostfixSelect(start0: Int, end: Int, od: Tree, op: Name): Tree = {
    val start = if (od.pos.isDefined) od.pos.start else start0
    atPos(r2p(start, end, end + op.length)) { new PostfixSelect(od, op.encode) }
  }

  /** Create tree representing a while loop */
  def makeWhile(startPos: Int, cond: Tree, body: Tree): Tree = {
    val lname = freshTermName(nme.WHILE_PREFIX)
    def default = wrappingPos(List(cond, body)) match {
      case p if p.isDefined => p.end
      case _                => startPos
    }
    val continu = atPos(o2p(body.pos pointOrElse default)) { Apply(Ident(lname), Nil) }
    val rhs = If(cond, Block(List(body), continu), Literal(Constant(())))
    LabelDef(lname, Nil, rhs)
  }

  /** Create tree representing a do-while loop */
  def makeDoWhile(lname: TermName, body: Tree, cond: Tree): Tree = {
    val continu = Apply(Ident(lname), Nil)
    val rhs = Block(List(body), If(cond, continu, Literal(Constant(()))))
    LabelDef(lname, Nil, rhs)
  }

  /** Create block of statements `stats`  */
  def makeBlock(stats: List[Tree]): Tree = TreeGen.mkBlock(stats)

  def makeParam(pname: TermName, tpe: Tree) =
    ValDef(Modifiers(PARAM), pname, tpe, EmptyTree)

  def makeSyntheticTypeParam(pname: TypeName, bounds: Tree) =
    TypeDef(Modifiers(DEFERRED | SYNTHETIC), pname, Nil, bounds)

  /** Create tree for a pattern alternative */
  def makeAlternative(ts: List[Tree]): Tree = {
    def alternatives(t: Tree): List[Tree] = t match {
      case Alternative(ts)  => ts
      case _                => List(t)
    }
    Alternative(ts flatMap alternatives)
  }

  /** Create tree for case definition <case pat if guard => rhs> */
  def makeCaseDef(pat: Tree, guard: Tree, rhs: Tree): CaseDef =
    CaseDef(TreeGen.patvarTransformer.transform(pat), guard, rhs)

  /** Creates tree representing:
   *    { case x: Throwable =>
   *        val catchFn = catchExpr
   *        if (catchFn isDefinedAt x) catchFn(x) else throw x
   *    }
   */
  def makeCatchFromExpr(catchExpr: Tree): CaseDef = {
    val binder   = freshTermName()
    val pat      = Bind(binder, Typed(Ident(nme.WILDCARD), Ident(tpnme.Throwable)))
    val catchDef = ValDef(Modifiers(ARTIFACT), freshTermName("catchExpr"), TypeTree(), catchExpr)
    val catchFn  = Ident(catchDef.name)
    val body     = atPos(catchExpr.pos.makeTransparent)(Block(
      List(catchDef),
      If(
        Apply(Select(catchFn, nme.isDefinedAt), List(Ident(binder))),
        Apply(Select(catchFn, nme.apply), List(Ident(binder))),
        Throw(Ident(binder))
      )
    ))
    makeCaseDef(pat, EmptyTree, body)
  }

  /** Create a tree representing the function type (argtpes) => restpe */
  def makeFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree = TreeGen.mkFunctionTypeTree(argtpes, restpe)

  /** Append implicit parameter section if `contextBounds` nonempty */
  def addEvidenceParams(owner: Name, vparamss: List[List[ValDef]], contextBounds: List[Tree]): List[List[ValDef]] = {
    if (contextBounds.isEmpty) vparamss
    else {
      val mods = Modifiers(if (owner.isTypeName) PARAMACCESSOR | LOCAL | PRIVATE else PARAM)
      def makeEvidenceParam(tpt: Tree) = ValDef(mods | IMPLICIT | SYNTHETIC, freshTermName(nme.EVIDENCE_PARAM_PREFIX), tpt, EmptyTree)
      val evidenceParams = contextBounds map makeEvidenceParam

      val vparamssLast = if(vparamss.nonEmpty) vparamss.last else Nil
      if(vparamssLast.nonEmpty && vparamssLast.head.mods.hasFlag(IMPLICIT))
        vparamss.init ::: List(evidenceParams ::: vparamssLast)
      else
        vparamss ::: List(evidenceParams)
    }
  }

  def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree) = TreeGen.mkPatDef(mods, pat, rhs)
}
