package cbc

import scala.language.existentials
import scala.language.implicitConversions
import shapeless._
import Conversions._

sealed abstract class SafeTree
object SafeTree {
  sealed trait Ref extends SafeTree

  sealed trait Term extends SafeTree with BlockStmt with TemplateStmt
  object Term {
    final case object Empty extends Term
    sealed trait Ref extends Term
    sealed trait PostfixExpr extends Term
    sealed trait SimpleExpr extends PostfixExpr
    sealed trait SimpleExpr1 extends SimpleExpr
    sealed trait WildIdent extends SimpleExpr1

    sealed trait Lit extends Term.SimpleExpr1 with Pat.SimplePattern
    sealed trait Bool extends Lit
    final case class True() extends Bool
    final case class False() extends Bool
    final case class Int(value: scala.Int) extends Lit
    final case class Long(value: scala.Long) extends Lit
    final case class Float(value: scala.Float) extends Lit
    final case class Double(value: scala.Double) extends Lit
    final case class Char(value: scala.Char) extends Lit
    final case class String(value: Predef.String) extends Lit
    final case class Symbol(value: scala.Symbol) extends Lit
    final case class Null() extends Lit
    final case class Unit() extends Lit

    trait StableId[T]
    object StableId {
      implicit object IdentStableId extends StableId[Term.Ident]
      implicit def selectStableId[P <: Term : Path]: StableId[Select[P]] = new StableId[Select[P]] {}
      implicit object SuperSelectStableId extends StableId[SuperSelect]
    }

    trait Path[T]
    object Path {
      implicit object IdentPath extends Path[Term.Ident]
      implicit def selectPath[P <: Term : Path]: Path[Select[P]] = new Path[Select[P]] {}
      implicit object SuperSelectPath extends Path[SuperSelect]
      implicit object ThisPath extends Path[This]
    }

    final case class Ident(name: ValidId) extends Ref with WildIdent with Pat.SimplePattern
    //final case class VarIdent(name: VarId) extends Ref with WildIdent
    final case class Select[T <: Term](qual: T, name: Term.Ident) extends Ref with SimpleExpr1
    final case class SuperSelect(qual: Option[Type.Ident], supertyp: Option[Type.Ident], selector: Term.Ident) extends Ref with SimpleExpr1
    final case class This(qual: Option[Type.Ident]) extends Ref with SimpleExpr1
    final case class Placeholder() extends WildIdent
    //TODO length of elements >= 2
    final case class Tuple(elements: List[Term]) extends SimpleExpr1
    final case class Apply[T : ArgExpr](fun: Term, args: T) extends SimpleExpr1
    final case class ApplyInfix(lhs: Term, op: Term.Ident, rhs: Term) extends PostfixExpr
    //TODO unary op only
    final case class ApplyPrefix(op: Term.Ident, arg: Term) extends PostfixExpr
    final case class ApplyPostfix(arg: Term, op: Term.Ident) extends PostfixExpr
    final case class TypeApply(fun: Term, args: List[Type]) extends SimpleExpr1
    final case class Eta(meth: Term) extends SimpleExpr

    //TODO result not correct
    final case class Block(stats: List[BlockStmt], result: Option[Term]) extends SimpleExpr
    sealed trait LegalNew[T]
    object LegalNew {
      implicit object ClassTemplateNew extends LegalNew[ClassTemplate]
      implicit object TemplateBodyNew extends LegalNew[List[TemplateStmt]]
    }
    final case class New[T : LegalNew](templ: T) extends SimpleExpr
    final case class CaseClauses(first: Case, rest: List[Case]) extends SimpleExpr

    final case class If(cond: Term, thenp: Term, elsep: Option[Term]) extends Term
    final case class Match(scrut: Term, cases: CaseClauses) extends Term
    final case class Assign(lhs: Select[_ <: Term], rhs: Term) extends Term
    final case class Update[T : ArgExpr](expr: Term, args: T) extends Term
    final case class Return(expr: Term) extends Term
    final case class Throw(expr: Term) extends Term
    final case class Typed(expr: Term, typ: Type.InfixType) extends Term
    //TODO length of annots >= 1
    final case class Annotated(expr: Term, annots: List[Annotation]) extends Term
    final case class ExprRepeatedArgs(expr: Term) extends Term
    final case class Try(expr: Block, catchp: Option[CaseClauses], finallyp: Option[Term]) extends Term
    final case class Binding(term: WildIdent, typ: Option[Type])
    //TODO length of param >= 1
    final case class Function(param: List[Binding], body: Term) extends Term
    final case class ImplicitFunction(param: Term.Ident, body: Term) extends Term
    //final case class PartialFunction(cases: List[Case]) extends Term
    final case class While(expr: Term, body: Term) extends Term
    final case class DoWhile(body: Term, expr: Term) extends Term
    final case class For(headEnum: Enumerator.Generator, tailEnums: List[Enumerator], body: Term) extends Term
    final case class ForYield(headEnum: Enumerator.Generator, tailEnums: List[Enumerator], body: Term) extends Term
    //final case class Interpolated(interpolator: TermName, parts: List[Term.String], args: List[Term])
  }

  sealed trait Type extends SafeTree with Param.ParamType
  object Type {
    sealed trait Ref extends Type with SafeTree.Ref
    //temperary empty type
    final case object Empty extends Type
    sealed trait SimpleType extends Type
    sealed trait WildIdent extends SimpleType

    final case class Ident(name: ValidId) extends WildIdent
    final case class Select[P: Term.Path](qual: P, name: Type.Ident) extends SimpleType
    final case class SuperSelect(qual: Option[Type.Ident], supertyp: Option[Type.Ident], selector: Type.Ident) extends SimpleType
    final case class Singleton[P : Term.Path](ref: P)  extends SimpleType
    final case class Project(qual: Type, name: Type.Ident) extends SimpleType
    //TODO non-empty targs, what's the typ?
    final case class Apply(typ: SimpleType, targs: List[Type]) extends SimpleType
    //TODO length >= 2
    final case class Tuple(elements: List[Type]) extends SimpleType

    sealed trait InfixType extends Type
    final case class ApplyInfix(lhs: InfixType, op: Type.Ident, rhs: InfixType) extends InfixType
    implicit def annotToCompound(t: Annotated): Compound = Compound(List(t), None)
    final case class Compound(parents: List[Annotated], rfns: Option[RefineStat]) extends InfixType
    //TODO non-empty dcls
    final case class Existential(typ: InfixType, dcls: List[Decl.ExistentialDcl]) extends Type
    final case class Function(params: List[Param.ParamType], ret: Type) extends Type
    implicit def simpleToAnnot(t: SimpleType): Annotated = Annotated(t, Nil)
    //TODO length of annots >= 1
    final case class Annotated(typ: SimpleType, annots: List[Annotation]) extends Type

    final case class Placeholder() extends WildIdent
  }

  sealed trait Pat extends SafeTree
  object Pat {
    final case object Empty extends Pat
    final case class Wildcard() extends Pat
    final case class SeqWildcard()
    sealed trait Pattern1 extends Pat
    sealed trait Pattern2 extends Pattern1
    sealed trait Pattern3 extends Pattern2
    sealed trait SimplePattern extends Pattern3
    final case class Alternative(left: Pat, right: Pat) extends Pat
    //TODO length of elems >= 2
    final case class Tuple(elements: List[Pat]) extends SimplePattern
    final case class Typed(lhs: Term.WildIdent, tpt: Type) extends Pattern1
    //name should be varid
    final case class Bind(name: Term.Ident, body: Pat) extends Pattern2
    final case class StableId[T : Term.StableId](ref: T) extends SimplePattern
    final case class Apply[T : Term.StableId](ref: T, elements: List[Pat]) extends SimplePattern
    final case class ApplySeq[T : Term.StableId](ref: T, elements: List[Pat], repeated: Option[Term.Ident]) extends SimplePattern
    final case class ApplyInfix(lhs: Pat, op: Term.Ident, rhs: Pat) extends Pattern3
  }

  implicit def refineToCompound(t: RefineStat): Type.Compound = Type.Compound(Nil, Some(t))
  
  sealed trait RefineStat extends SafeTree

  sealed trait Decl extends SafeTree with TemplateStmt
  object Decl {
    sealed trait ExistentialDcl extends Decl
    sealed trait PureDcl extends Decl with RefineStat
    //TODO length of ids >= 1
    final case class Val(annots: List[Annotation], mods: List[Mod], ids: List[Term.Ident], typ: Type) extends Decl
    final case class PureVal(ids: List[Term.Ident], typ: Type) extends ExistentialDcl with PureDcl

    final case class Var(annots: List[Annotation], mods: List[Mod], ids: List[Term.Ident], typ: Type) extends Decl
    final case class PureVar(ids: List[Term.Ident], typ: Type) extends PureDcl

    final case class Def(annots: List[Annotation], mods: List[Mod], name: Term.Ident, tparams: List[Param.TypeParam], paramss: Param.Clauses, typ: Option[Type]) extends Decl
    final case class PureDef(name: Term.Ident, tparams: List[Param.TypeParam], paramss: Param.Clauses, typ: Option[Type]) extends PureDcl

    final case class Typ(annots: List[Annotation], mods: List[Mod], name: Type.Ident, tparams: List[Param.Variant], lower: Option[Type], upper: Option[Type]) extends Decl
    final case class PureType(name: Type.Ident, tparams: List[Param.Variant], lower: Option[Type], upper: Option[Type]) extends ExistentialDcl with PureDcl
  }

  sealed trait Defn extends SafeTree with TemplateStmt
  object Defn {
    sealed trait TopLevel extends Defn
    sealed trait Nested extends Defn
    sealed trait PatVar extends Defn
    sealed trait Def extends Defn with Nested

    //TODO length of pats >= 1
    final case class Val(annots: List[Annotation], mods: List[Mod], pats: List[Pat.Pattern2], typ: Option[Type], rhs: Term) extends PatVar

    final case class Var(annots: List[Annotation], mods: List[Mod], pats: List[Pat.Pattern2], typ: Option[Type], rhs: Term) extends PatVar

    final case class Fun(annots: List[Annotation], mods: List[Mod], name: Term.Ident, tparams: List[Param.TypeParam], paramss: Param.Clauses, typ: Option[Type], body: Term) extends Def

    final case class This(annots: List[Annotation], mods: List[Mod], params: List[Param.Item], paramss: Param.Clauses, ce: ConstrExpr) extends Def

    final case class Typ(annots: List[Annotation], mods: List[Mod], name: Type.Ident, tparams: List[Param.Variant], body: Type) extends Defn with RefineStat

    //final case class Macro(annots: List[Annotation], mods: List[Mod], name: Term.Ident, tparams: List[Param.TypeParam], paramss: Param.Clauses, typ: Type, body: Term) extends Nested

    final case class Class(annots: List[Annotation], mods: List[Mod], name: Type.Ident, tparams: List[Param.Variant], ctor: PrimaryConstr, templ: ClassTemplate) extends TopLevel with Nested with BlockStmt

    final case class CaseClass(annots: List[Annotation], mods: List[Mod], name: Type.Ident, tparams: List[Param.Variant], ctor: PrimaryConstr, templ: ClassTemplate) extends TopLevel with Nested with BlockStmt
    
    final case class Trait(annots: List[Annotation], mods: List[Mod], name: Type.Ident, tparams: List[Param.Variant], templ: TraitTemplate) extends TopLevel with Nested with BlockStmt

    final case class Object(annots: List[Annotation], mods: List[Mod], name: Term.Ident, templ: ClassTemplate) extends TopLevel with Nested with BlockStmt

    final case class CaseObject(annots: List[Annotation], mods: List[Mod], name: Term.Ident, templ: ClassTemplate) extends TopLevel with Nested with BlockStmt
    
    final case class Package(ref: Term.Ref, body: List[Defn.TopLevel]) extends TopLevel with Nested
    
    final case class PackageObject(obj: Defn.Object) extends TopLevel with Nested
  }

  final case class Import(clauses: List[Import.ImportExpr[_]]) extends BlockStmt with TemplateStmt with Defn.TopLevel
  object Import { 
    final case class ImportExpr[T: Term.StableId](ref: T, sels: List[Selector], last: Option[Term.Placeholder]) extends SafeTree

    sealed trait Selector extends SafeTree
    object Selector {
      final case class Normal(name: Term.Ident) extends Selector
      final case class Rename(from: Term.Ident, to: Term.Ident) extends Selector
      final case class Unimport(name: Term.Ident) extends Selector
    }
  }

  sealed trait ArgExpr[T]
  object ArgExpr {
    implicit object Exprs extends ArgExpr[List[Term]]
    implicit object ExprsRepeat extends ArgExpr[Param.ArgExprRepeat]
    implicit object CaseClauses extends ArgExpr[Term.CaseClauses]
    implicit object Block extends ArgExpr[Term.Block]
  }
  sealed trait Param extends SafeTree
  object Param {
    final case class ArgExprRepeat(args: List[Term], rearg: Term)

    sealed trait ParamType extends SafeTree
    final case class CallByNameType(typ: Type) extends ParamType
    final case class RepeatedType(typ: Type) extends ParamType

    final case class Item(annots: List[Annotation], id: Term.Ident, typ: Option[ParamType], rhs: Option[Term])
    final case class Clauses(normal: List[List[Item]], im: Option[List[Item]])

    sealed trait VarianceOp
    final case object Invariance extends VarianceOp
    final case object Covariance extends VarianceOp
    final case object Contravariance extends VarianceOp

    final case class Variant(annots: List[Annotation], variance: VarianceOp, tparam: TypeParam)
    final case class TypeParam(name: Type.WildIdent, typs: List[Variant], lower: Option[Type], upper: Option[Type], view: List[Type], context: List[Type])

    final case class ClassClauses(normal: List[List[ClassParam]], im: Option[List[ClassParam]])
    //TODO val | var
    sealed trait ValOrVar
    final case object Val extends ValOrVar
    final case object Var extends ValOrVar
    final case class ClassParamMods(mods: List[Mod], pre: Option[ValOrVar])
    final case class ClassParam(annots: List[Annotation], mods: Option[ClassParamMods], name: Term.Ident, typ: ParamType, rhs: Option[Term])
  }
  //args should be list of ArgExpr
  final case class Annotation(typ: Type.SimpleType, args: List[List[Term]]) extends SafeTree
  final case class ConstrAnnotation[T : ArgExpr](typ: Type.SimpleType, args: T) extends SafeTree
  final case class PrimaryConstr(annot: List[ConstrAnnotation[_]], mod: Option[Mod.AccessModifier], cparams: Param.ClassClauses) extends SafeTree

  sealed trait Mod extends SafeTree
  object Mod {
    sealed trait LocalModifier extends Mod
    sealed trait AccessModifier extends Mod
    final case object Override extends Mod

    final case object Abstract extends LocalModifier
    final case object Final extends LocalModifier
    final case object Sealed extends LocalModifier
    final case object Implicit extends LocalModifier
    final case object Lazy extends LocalModifier

    sealed trait AccessQualifier[T]
    object AccessQualifier {
      implicit object IdentQual extends AccessQualifier[Term.Ident]
      implicit object ThisQual extends AccessQualifier[Term.This]
    }
    final case class Private[T : AccessQualifier](qual: Option[T]) extends AccessModifier
    final case class Protected[T : AccessQualifier](qual: Option[T]) extends AccessModifier
  }
 
  final case class Case(pat: Pat, cond: Option[Term], body: Term.Block) extends SafeTree

  final case class ConstrExpr(args: List[List[Term]], stmt: Option[List[BlockStmt]]) extends SafeTree
  // for definitions, the modifiers can only be implicit or lazy, for templates, only local modifiers
  sealed trait BlockStmt extends SafeTree
  sealed trait TemplateStmt extends SafeTree
  final case class Constr(typ: Type.Annotated, args: List[List[Term]]) extends SafeTree
  final case class EarlyDef(annots: List[Annotation], mods: List[Mod], dfs: Defn.PatVar) extends SafeTree
  final case class ClassTemplate(early: List[EarlyDef], ctor: Constr, parents: List[Type.Annotated], self: Option[SelfType[_]], body: List[TemplateStmt]) extends SafeTree
  final case class TraitTemplate(early: List[EarlyDef], parents: List[Type.Annotated], self: Option[SelfType[_]], body: List[TemplateStmt]) extends SafeTree
  sealed trait LegalSelf[T]
  object LegalSelf {
    implicit object IdentSelf extends LegalSelf[Term.Ident]
    implicit object ThisSelf extends LegalSelf[Term.This]
  }
  final case class SelfType[T : LegalSelf](term: T, typ: Option[Type]) extends SafeTree

  sealed trait Enumerator extends SafeTree
  object Enumerator {
    final case class Generator(pat: Pat.Pattern1, rhs: Term, cond: Option[Term]) extends Enumerator
    final case class ValueDefinition(pat: Pat.Pattern1, rhs: Term) extends Enumerator
    final case class Guard(cond: Term) extends Enumerator
  }

  final case class CompilationUnit(pkgs: List[Term.Ref], top: Defn.TopLevel) extends SafeTree

}
