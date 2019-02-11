import java.lang.reflect.Field

/**
  * A trait that defines a method of equating to instances of [[T]] by comparing a subset of the fields of [[T]].
  * For example, take the following type:
  *
  * {{{
  *   case class Entity(id: Long, lastModified: Long)
  * }}}
  *
  * If you would like to be able to consider two Entities identical as long as the IDs matched and ignore the timestamp
  * of the last modification, you could make `Entity` equatable as such:
  *
  * {{{
  *   case class Entity(id: Long, lastModified: Long) extends Equatable[Entity] {
  *     override protected def identifyingFields = Seq(field("id"))
  *   }
  * }}}
  *
  * Now, `Entity` has two levels of strictness when determining equality to another `Entity`:
  *
  * {{{
  *   Entity(1, 0) == Entity(1, 0) // true
  *   Entity(1, 0) == Entity(1, 1) // false
  *   Entity(1, 0) === Entity(1, 0) // true
  *   Entity(1, 0) === Entity(1, 1) // true
  *   Entity(1, 0) !== Entity(1, 1) // false
  * }}}
  *
  * @tparam T The type to be equated
  */
trait Equatable[T] {
  /**
    * Checks that all fields specified via [[identifyingFields]] are equal between `this` and `other`, as determined
    * by `this.field.equals(other.field)`.
    *
    * @param other The [[T]] to compare `this` to
    * @return true if all [[identifyingFields]] are equal
    */
  def isEquivalentTo(other: T): Boolean =
    this.getClass == other.getClass &&
      identifyingFields.forall(field => fieldsAreEqual(field, other))

  val === : T => Boolean = isEquivalentTo
  val !== : T => Boolean = isEquivalentTo(_) == false

  /**
    * Names the fields that identify a [[T]]. Essentially, two [[T]]s will be grouped by the given fields.
    *
    * @return The fields that identify a [[T]]
    */
  protected def identifyingFields: Seq[Field]

  /**
    * Returns a reference to the [[java.lang.reflect.Field]] given by `name`. Also makes the given field
    * accessible, in order to enable reflective use by [[isEquivalentTo()]].
    *
    * @param name The name of the field
    * @return The field named by `name`
    */
  protected def field(name: String): Field = {
    val f = this.getClass.getDeclaredField(name)
    f.setAccessible(true)
    f
  }

  private def fieldsAreEqual(field: Field, other: T) = field.get(this).equals(field.get(other))
}

