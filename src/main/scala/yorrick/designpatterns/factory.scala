package yorrick.designpatterns



trait Animal

private class Cat extends Animal
private class Dog extends Animal


object Animal {
  def apply(kind: String): Animal = kind match {
    case "cat" => new Cat
    case "dog" => new Dog
    case _ => throw new Exception("Unknown animal")
  }
}