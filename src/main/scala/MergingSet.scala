import scala.collection.mutable

/**
  * MergingSet allows for the creation of sets that test existence based on equatability rather than equality.
  * For instance, say you have the following class:
  *
  * {{{
  *   case class ProductClick(id: ProductId, name: String, callChainId: CallChainId)
  * }}}
  *
  * The creation of a set of clicked products would generally require a new class because, although `id` and `name`
  * will be unique to a particular product, the `callChainId` will not be. So, a normal set would result in duplicate
  * entries because of the uniqueness of `callChainId`.
  *
  * With [[MergingSet]], you can update the class definition to support equating by id and the merging of the
  * callChainIds into a [[Seq[CallChainId]].
  *
  * {{{
  *   case class ProductClick(id: ProductId, name: String, callChainIds: Seq[CallChainId]) {
  *     override def merge(other: ProductClick): ProductClick = ProductClick(id, name, callChainIds ++ other.callChainIds)
  *
  *     override protected def identifyingFields: Seq[Field] = Seq(field("id"))
  *   }
  * }}}
  *
  * Now, the following call will result in set of size one while maintaining all of the data:
  *
  * {{{
  *   val set = new MergingSet[ProductClick]() + ProductClick(1, "", "12345") + ProductClick(1, "", "45678")
  * }}}
  *
  * @tparam A The type of the elements to be stored in the set
  */
class MergingSet[A <: Mergeable[A] with Equatable[A]] extends Set[A] {
  private val set = new mutable.HashSet[A]()

  override def contains(elem: A): Boolean = exists { elem === _ }

  override def +(elem: A): Set[A] = {
    val e = set.find { elem === _ }

    if (e.isDefined) {
      set -= e.get
      set += e.get.merge(elem)
    }
    else set += elem

    this
  }

  override def -(elem: A): Set[A] = { set -= elem; this }

  override def iterator: Iterator[A] = set.iterator
}
