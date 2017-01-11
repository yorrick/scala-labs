package yorrick.macros.talkinganimal

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
//import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.whitebox.Context


class TalkingAnimalSpell extends StaticAnnotation {
//  def macroTransform(annottees: Any*) = macro TalkingAnimalSpell.impl
  def macroTransform(annottees: Any*): Any = macro TalkingAnimalSpell.impl
//  def macroTransform(annottees: Any*): Context#Expr[Any] = macro TalkingAnimalSpell.impl
//  def macroTransform(annottees: Any*): Expr[Context] = macro TalkingAnimalSpell.impl
//  def macroTransform(annottees: Any*): Context#Tree = macro TalkingAnimalSpell.impl
}

object TalkingAnimalSpell {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

//    println("inside macROROORORORORO")

    val classDef: ClassDef = {
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends Animal with ..$parents { $self => ..$stats }" :: Nil => {
          val animalType = tpname.toString()

          // use http://docs.scala-lang.org/overviews/quasiquotes/intro.html to be more concise
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends Animal with ..$parents{
            def sayHello: String = {
              "Hello I'm " + $animalType + " and my name is " + name
            }
          }"""
        }
        case _ => c.abort(c.enclosingPosition, "Annotation @TalkingAnimal can be used only with case classes which extends Animal trait")
      }
    }

    val result: Expr[Any] = c.Expr[Any](classDef)

    result
  }
}