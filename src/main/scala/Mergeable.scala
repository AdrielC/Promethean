
/**
  * A trait that allows for the merger of two instances of the same type, [[T]].
  *
  * @tparam T The type to be merged
  */
trait Mergeable[T] {
  /**
    * Returns a [[T]] representing the merger of `this` and `other`.
    *
    * @param other The [[T]] to merge with
    * @return The merged [[T]]
    */
  def merge(other: T): T
}
