package yorrick.macros.to_string


import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context


class BuilderFunctions extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BuilderFunctions.impl
}


object BuilderFunctions {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val classDef: ClassDef = {
      annottees.map(_.tree).toList match {
        // use http://docs.scala-lang.org/overviews/quasiquotes/intro.html to be more concise
        // this matches only case classes
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents { $self => ..$stats }" :: Nil => {
          val valDefs: List[ValDef] = paramss.flatten

          val builderFunctions: Seq[Tree] = valDefs.map {
            case ValDef(_, name, tpt, _) => (name, tpt)
          }.map { case (name, tpt) =>
            q"def $name[T](t: T)(implicit to: T => $tpt): $tpname = this.copy($name = to(t))"
          }

          // TODO use basic API here, once we find out how to define $ctorMods(...$paramss) using ClassDef
          val basicClassDef: ClassDef =
            q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents{
              $self => ..$stats
            }"""

//          val classImplementation = Template(parents, self, stats ++ builderFunctions)
//          val basicClassDef: ClassDef = ClassDef(mods, tpname, tparams, classImplementation)

          def appendMethods(cd: ClassDef, methods: Seq[Tree]): ClassDef = cd match {
            case ClassDef(mods, name, something, template) =>
              val q = template match {
                case Template(superMaybe, emptyValDef, defs) =>
                  Template(superMaybe, emptyValDef, defs ++ builderFunctions)
                case y =>
                  y
              }

              ClassDef(mods, name, something, q)
          }

          // use basic API
          appendMethods(basicClassDef, builderFunctions)
        }
        case _ => c.abort(c.enclosingPosition, "Annotation @DataToString can be used only with case classes")
      }
    }

    c.Expr[Any](classDef)
  }
}
