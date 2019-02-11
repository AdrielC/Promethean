import org.scalatest.FlatSpec

class EquatableTest extends FlatSpec {
  case class Entity(id: Int, lastModified: Int) extends Equatable[Entity] {
    override protected def identifyingFields = Seq(field("id"))
  }

  class Parent(id: Long) extends Equatable[Parent] {
    override protected val identifyingFields = Seq()
  }

  case class Child(id: Int) extends Parent(id)

  case class Sibling(id: Int) extends Parent(id)

  "An Equatable" should "be equal to an identical instance" in {
    assert(Entity(1, 1) == Entity(1, 1))
  }

  it should "not be equal to an instance with different values in non-identifiable fields" in {
    assert(Entity(1, 1) != Entity(1, 2))
  }

  it should "be equatable to an instance with different values in non-identifiable fields" in {
    assert(Entity(1, 1) === Entity(1, 2))
  }

  it should "be equatable to an identical instance" in {
    assert(Entity(1, 1) === Entity(1, 1))
  }

  it should "not be equatable to an instance with different values in identifiable fields" in {
    assert(Entity(1, 1) !== Entity(2, 1))
  }

  "A child class" should "not be equatable to a sibling" in {
    assert(Child(1) !== Sibling(1))
  }
}