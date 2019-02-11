import java.lang.reflect.Field

import org.scalatest.FlatSpec

class MergingSetTest extends FlatSpec {
  case class Element(id: Int, count: Int = 1) extends Mergeable[Element] with Equatable[Element] {
    override def merge(other: Element): Element = Element(id, count + other.count)

    override protected def identifyingFields: Seq[Field] = Seq(field("id"))
  }

  "An empty MergingSet" should "have no elements" in {
    assert(new MergingSet[Element]().size === 0)
  }

  it should "accept new elements" in {
    val set = new MergingSet[Element]()
    val set2 = set + Element(1)

    assert(set.size === 1)
    assert(set == set2)
  }

  "A non-empty MergingSet" should "add non-equatable new elements" in {
    val set = new MergingSet[Element]() + Element(1) + Element(2)

    assert(set.size === 2)
  }

  it should "merge equatable elements with the existing one" in {
    val set = new MergingSet[Element]() + Element(1) + Element(1)

    assert(set.size === 1)
    assert(set.iterator.next === Element(1, 2))
  }
}