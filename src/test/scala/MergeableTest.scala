import org.scalatest.FlatSpec

class MergeableTest extends FlatSpec {
  case class MergeableClass(first: String, second: Int, third: String) extends Mergeable[MergeableClass] {
    override def merge(other: MergeableClass): MergeableClass = MergeableClass(first, second, s"$third:${other.third}")
  }

  "A Mergeable[T]" should "merge with another Mergeable[T]" in {
    val c1 = MergeableClass("foo", 2, "third1")
    val c2 = MergeableClass("foo", 2, "third2")
    val expected = MergeableClass("foo", 2, "third1:third2")

    assert(c1.merge(c2) === expected)
  }
}
