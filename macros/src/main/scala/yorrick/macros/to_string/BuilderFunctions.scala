package yorrick.macros.to_string


import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.whitebox.Context


class BuilderFunctions extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BuilderFunctions.impl
}


object BuilderFunctions {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val classDef: ClassDef = {
      annottees.map(_.tree).toList match {
        // this matches only case classes
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents { $self => ..$stats }" :: Nil => {
          val valDefs: List[ValDef] = paramss.flatten

          val builderFunctions: Seq[Tree] = valDefs.map {
            case ValDef(_, name, tpt, _) => (name, tpt)
          }.map { case (name, tpt) =>
            q"def $name[T](t: T)(implicit to: T => $tpt): $tpname = this.copy($name = to(t))"
          }

//          val fieldNames = valDefs.map(_.name.decodedName.toString)
//          val emptyString = ""
//
//          val dataToStringTree: Tree = fieldNames
//            .map(fn => Ident(TermName(fn)))
//            .map(identifier => q"$identifier")
//            .foldLeft(q"$emptyString")((t1, t2) => q"$t1.toString + $t2.toString")

          // use http://docs.scala-lang.org/overviews/quasiquotes/intro.html to be more concise
          // TODO use basic API here!!
          val basicClassDef: ClassDef = q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents{
          }"""

//          println(basicClassDef)

          // use basic API
          basicClassDef match {
            case ClassDef(mods, name, something, template) =>
              val q = template match {
                case Template(superMaybe, emptyValDef, defs) =>
                  Template(superMaybe, emptyValDef, defs ++ stats ++ builderFunctions)
                case y =>
                  y
              }
              ClassDef(mods, name, something, q)
          }
        }
        case _ => c.abort(c.enclosingPosition, "Annotation @DataToString can be used only with case classes")
      }
    }

    val result: Expr[Any] = c.Expr[Any](classDef)

    result
  }
}
