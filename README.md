# Promethean
**Promethean** is a type class based generic programming library using Scala's reflection API
The primary uses for this library are data consolidation between conceptually similar case classes and for data validation.

## Using Promethean
Promethean is an abstract class that can be extended by any case class to enrich it with generic programming methods. 
Any case class that extends Promethean must define the `identifyingFields` method, which is the method used by Promethean
to determine equatability between Promethean classes.

### Consolidating Data with absorb
```scala

case class Person(name: String, age: Int = -1, address: String = "") extends Promethean[Person] {
  def identifyingFields = Seq(field("name"))
}

case class PhoneBookRecord(name: String, phoneNumber: Int, address: String) extends Promethean[Person] {
  def identifyingFields = Seq(field("name"))
}

```

Let's say we don't know Layton's address when we instantiate his `Person` class.
We can omit his address since we defined the default to be a blank string

```scala

val layton = Person(name = "Layton", age = 23)

// Perhaps we have another datasource of phone book records and we find one with Layton's address. 

val laytonRecord = PhoneBookRecord(name = "Layton", phoneNumber = 5555555555, address = "6 Columbia Lane")

```

We can use Promethean to consolidate data for us since both case classes represent the same concept and share field names.
Both `layton` and `laytonRecord` have the same value in their `identifyingFields`, making them equatable.

```scala

layton === laytonRecord // results in true

// consolidating data is done with `absorb`

val laytonFull = layton.absorb(laytonRecord)
// returns Person("Layton", 23, "6 Columbia Lane")

```

### Creating Reactive Data with reactiveFields

We can also define how case classes behave when exposed to certain scenarios. Defining `reactiveFields` 
in the case class definition allows the user to specify additional behavior of an absorbtion. 

For example, if we find out that Layton has a phone number, we can assume that he must have a phone. Instead of
definig that behavior elsewhere in our project, we can define that behavior by overriding the `reactiveFields` method.

```scala
// redefine case classes with reactiveFields


case class Person(name: String, age: Int -1, address: String = "", hasPhone: Boolean = false) extends Promethean[Person] {
  def identifyingFields = Seq(field("name"))
  def reactiveFields = Seq(
    AbsorbReaction(field = field("hasPhone"), precipitate = a => true, trigger = a => a.isInstanceOf[PhoneBookRecord])
  )
}

case class PhoneBookRecord(name: String, phoneNumber: Int, address: String) extends Promethean[Person] {
  def identifyingFields = Seq(field("name"))
}

```

Within the `reactiveFields` definition for `Person`, we create an `AbsorbReaction` which defines the rules for absorbtion.
This reaction recipe is evaluated each time and instance of `Person` absorbs another Promethean type class.
The three components to a `PrometheanReaction` type, of which include `AbsorbReaction` and `AutoReaction`, are the following:

1. `field`
    This defines which field in the parent class will be subject to change

2. `precipitate`
    This is the outcome of the reaction that will replace the current value of the specified field.
    This value is an anonymous function that must evaluate to a value of the same type expected by the field.
    The value can be static, as seen in this example, or derived from the class being absorbed (by using the LHS variable)

3. `trigger`
    This is an anonymous function that is evaluated to determine whether a new value will be precipitated by the reaction.
    In this example, anytime an instance of `Person` absorbs an instance of `PhoneBookRecord`, we then can say that the 
    person has a phone, thus we precipitate a value of `true` into the `hasPhone` of the absorbing `Person` instance.
    
`AutoReaction`s behave in a similar manner, although they are self reactions which are catalyzed with a call to 
the Promethean method `autocatalyze`.

### Case Class Conversions with hardenAs[_] and Autocatalysis

```scala

case class Person(name: String, age: Int -1, address: String = "", phoneNumber = -1, hasPhone: Boolean = false) extends Promethean[Person] {
  def identifyingFields = Seq(field("name"))
  def reactiveFields = Seq(
    AbsorbReaction(field = field("hasPhone"), precipitate = a => true, trigger = a => a.isInstanceOf[PhoneBookRecord]),
    AutoReaction(field = field("hasPhone"), precipitate = true, trigger = !clayFields.contains("phoneNumber"))
  )
}

case class PhoneBookRecord(name: String, phoneNumber: Int, address: String) extends Promethean[Person] {
  def identifyingFields = Seq(field("name"))
}

val layton = Person(name = "Layton", age = 23)
val laytonRecord = PhoneBookRecord(name = "Layton", phoneNumber = 5555555555, address = "6 Columbia Lane")

val personFromRecord = laytonRecord.hardenAs[Person].autocatalyze
// personFromRecord: Person = Person(name = "Layton", 
//                                   age = -1, 
//                                   address = "6 Columbia Lane", 
//                                   phoneNumber = 5555555555, 
//                                   hasPhone = true)
                                     
val fullPersonFromRecord = personFromRecord.absorb(layton)
// fullPersonFromRecord: Person = Person(name = "Layton", 
//                                       age = 23, 
//                                       address = "6 Columbia Lane", 
//                                       phoneNumber = 5555555555, 
//                                       hasPhone = true)

```

In this example we introduce the following

1. case class conversions using the `hardenAs[_]` method
2. `clayFields` and the importance of defaults
3. `Autoreaction`s

The `hardenAs[Person]` call converts the instance of `PhoneBookRecord` to an instance of `Person` using the shared field names
and their corresponding values, using defualt values to fill in the rest. This will fail at runtime however if there was no
default value for age since `PhoneBookRecord` does not define that field. However since the default for `Person.age`
is defined as `-1` it is able to complete the conversion. The method `canHardenAs[_]` can be used to check if a conversion
is possible.

`clayFields` returns a `Set[String]` that contains all of the field names that are still equal to their default values. In
this example, we are converting from a `PhoneBookRecord` to a person, the `phoneNumber` field differs from the default of
`-1`, thus it will not be found in the set returned by `clayFields`.

Since we converted from a `PhoneBookRecord` to a `Person`, we now have access to the `AutoReaction` defined in `Person`s
`reactiveFields`. The `AutoReaction` specifies that if the value for the `phoneNumber` field differs from `-1`---which 
means the field name will no longer be returned by `clayFields` and will now be found as a key in the Map returned 
by `clayValues`---then we can flip the hasPhone value to `true` when we call the `autocatalyze` method on the newly 
hardened person.
