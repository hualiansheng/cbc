/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package cbc

import cbc.util.FreshNameCreator
import Names._, StdNames._

object FreshNames {
  // SI-6879 Keeps track of counters that are supposed to be globally unique
  //         as opposed to traditional freshers that are unique to compilation units.
  implicit val globalFreshNameCreator = new FreshNameCreator

  // create fresh term/type name using implicit fresh name creator
  def freshTermName(prefix: String = nme.FRESH_TERM_NAME_PREFIX)(implicit creator: FreshNameCreator): TermName = newTermName(creator.newName(prefix))
  def freshTypeName(prefix: String)(implicit creator: FreshNameCreator): TypeName = newTypeName(creator.newName(prefix))

  // Extractor that matches names which were generated by some
  // FreshNameCreator with known prefix. Extracts user-specified
  // prefix that was used as a parameter to newName by stripping
  // global creator prefix and unique number in the end of the name.
  class FreshNameExtractor(creatorPrefix: String = "") {
    // quote prefix so that it can be used with replaceFirst
    // which expects regExp rather than simple string
    val quotedCreatorPrefix = java.util.regex.Pattern.quote(creatorPrefix)

    def unapply(name: Name): Option[String] = {
      val sname = name.toString
      // name should start with creatorPrefix and end with number
      if (!sname.startsWith(creatorPrefix) || !sname.matches("^.*\\d*$")) None
      else Some(NameTransformer.decode(sname.replaceFirst(quotedCreatorPrefix, "").replaceAll("\\d*$", "")))
    }
  }
}
