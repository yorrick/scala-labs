package yorrick.macros

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import yorrick.macros.talkinganimal.TalkingAnimalSpell


trait Animal{
  val name: String
}

@TalkingAnimalSpell
case class Dog(name: String) extends Animal


class TalkingAnimalTest extends FlatSpec with Matchers with OptionValues with TryValues {
  "TalkingAnimalSpell annotation" should "make animals talk" in {
    Dog("Jack").sayHello shouldBe "Hello I'm Dog and my name is Jack"
  }

}
