package yorrick.macros.to_string


import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.whitebox.Context


class DataToString extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DataToString.impl
}


object DataToString {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val classDef: ClassDef = {
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents { $self => ..$stats }" :: Nil => {
          val valDefs: List[ValDef] = paramss.flatten

          val fieldNames = valDefs.map(_.name.decodedName.toString)
          val emptyString = ""

          val dataToStringTree: Tree = fieldNames
            .map(fn => Ident(TermName(fn)))
            .map(identifier => q"$identifier")
            .foldLeft(q"$emptyString")((t1, t2) => q"$t1.toString + $t2.toString")

          println(dataToStringTree)

          // use http://docs.scala-lang.org/overviews/quasiquotes/intro.html to be more concise
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents{
            def dataToString: String = {
             $dataToStringTree
            }
          }"""
        }
        case _ => c.abort(c.enclosingPosition, "Annotation @DataToString can be used only with case classes")
      }
    }

    val result: Expr[Any] = c.Expr[Any](classDef)

    result
  }
}
