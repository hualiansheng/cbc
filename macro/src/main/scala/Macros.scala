package cbc
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.whitebox.Context
import scala.util.matching.Regex
import shapeless._
  
sealed trait ValidId
final case class ConstantId(name: String) extends ValidId
final case class VarId(name: String) extends ValidId
final case class OpId(name: String) extends ValidId
final case class BackQuotedId(name: String) extends ValidId

object Conversions {
  implicit def stringToValidId(name: String): ValidId = macro stringToValidId_impl
  def stringToValidId_impl(c: Context)(name: c.Expr[String]) = {
    import c.universe._
    val q"${s: String}" = name.tree
    val backQuotedIdPat = "`(.*)`".r
    val opIdPat = "([!#%&*+-/:<=>?@\\\\^|~]+)".r
    val varIdPat = "([a-z][\\w\\$]*(?:_[!#%&*+-/:<=>?@\\\\^|~]+)?)".r
    val constantIdPat = "([A-Z_\\$][\\w\\$]*(?:_[!#%&*+-/:<=>?@\\\\^|~]+)?)".r
    s match {
      case "abstract"|"case"|"catch"|"class"|"def"|"do"|"else"|"extends"|"false"|"final"|
           "finally"|"for"|"forSome"|"if"|"implicit"|"import"|"lazy"|"match"|"new"|"null"|
           "object"|"override"|"package"|"private"|"protected"|"return"|"sealed"|"super"|
           "this"|"throw"|"trait"|"try"|"true"|"type"|"val"|"var"|"while"|"with"|"yield"|
           "_"|":"|"="|"=>"|"<-"|"<:"|"<%"|">:"|"#"|"@"
           => c.abort(c.enclosingPosition, "reserved identifier")
      case backQuotedIdPat(s) => q"BackQuotedId($s)"
      case opIdPat(s) => q"OpId($s)"
      case varIdPat(s) => q"VarId($s)"
      case constantIdPat(s) => q"ConstantId($s)"
      case _ => c.abort(c.enclosingPosition, "not valid identifier")
    }
  }  
  
  implicit def listToSized[T](l: List[T]): Sized[List[T], _ <: Nat] = macro listToSized_impl
  def listToSized_impl(c: Context)(l: c.Expr[Any]) = {
    import c.universe._
    val q"List(..$args)" = l.tree
    q"_root_.shapeless.Sized(..$args)"
  }

}
