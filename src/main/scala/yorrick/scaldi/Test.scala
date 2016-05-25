package yorrick.scaldi

import java.util.Properties

import scaldi.{Injectable, PropertiesInjector}


object Test extends Injectable {
  def main(args: Array[String]) {
    val props = new  Properties()
    props.setProperty("greeting.official", "Welcome")

    implicit val injector = PropertiesInjector(props) :: new UserModule
    println(inject[MessageService].getGreetMessage("toto"))
  }
}
