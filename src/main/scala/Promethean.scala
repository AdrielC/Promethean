import java.lang.reflect.{Constructor, Field, Modifier}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.runtime.BoxesRunTime._
import scala.util.Try

object Promethean {
  def smash[A : TypeTag](obj: A): Clay =
    if(obj != null){
      Clay((Map[String, Any]() /: obj.getClass.getDeclaredFields) {
        (a, f) =>
          f.setAccessible(true)
          a + (f.getName -> f.get(obj))
      })
    } else Clay()

  def getFieldNamesAndTypes[A : TypeTag]: Map[String, Type] = PrometheanUtil.getFieldNamesAndTypes[A]
  def getDefaultFieldsAndVals[A : ClassTag]: Map[String, Any] = PrometheanUtil.getDefaultFieldsAndVals[A]
}

abstract class Promethean[P <: Promethean[P] : ClassTag : TypeTag] extends Product with Serializable with Equatable[Promethean[_]] with Mergeable[Promethean[_]] {

  private lazy val clay: Clay = Promethean.smash(this)

  def key: String = identifyingFields.map(f => s"${f.getName}::${f.get(this).toString}").reduce(_ + "__" + _)

  def clayFields: Set[String] = (Set[String]() /: clay.map){(s, m) => if(myDefaults.get(m._1).contains(m._2)) s + m._1 else s}
  def reactiveFields: Seq[PrometheanReaction] = Seq()
  def clayValues: Clay = clay.diff(myDefaults)
  def prometheanChildren: Clay = Clay(clay.map.filter(m => m._2.isInstanceOf[Promethean[_]]))
  def hardenAs[A: ClassTag : TypeTag]: A = clay.hardenAs[A]
  def canHardenFrom(otherClay: Clay): Boolean = (clay squish otherClay(clayFields)).canHardenAs[P]
  def merge(other: Promethean[_]): Promethean[P] = this.absorb(other)

  def absorb(other: Promethean[_]): P =
    if(this === other)
      (clayValues squish other.clayValues(clayFields) squish chainMerge(other) squish catalyze(other)).hardenAs[P]
    else this.asInstanceOf[P]

  def catalyze(other: Promethean[_]): Clay =
    if(reactiveFields.nonEmpty) {
      val precipitate = reactiveFields.collect { case reaction: AbsorbReaction => reaction.reactTo(other) }
      precipitate.reduceLeftOption(_ squish _).getOrElse(Clay())
    } else Clay()

  def autocatalyze: P =
    if(reactiveFields.nonEmpty) {
      val precipitate = reactiveFields.collect { case reaction: AutoReaction => reaction.react }
      val product = precipitate.reduceLeftOption(_ squish _).getOrElse(Clay())
      val productAutocatalysis = product.prometheanMap(_.autocatalyze)
      (clay squish prometheanChildren.prometheanMap(_.autocatalyze) squish product squish productAutocatalysis).hardenAs[P]
    } else (clay squish prometheanChildren.prometheanMap(_.autocatalyze)).hardenAs[P]

  override def isEquivalentTo(other: Promethean[_]): Boolean =
    identifyingFields.forall(field => field.get(this).equals(other.clay.map.getOrElse(field.getName, None)))

  private def myDefaults: Map[String, Any] = PrometheanUtil.getDefaultFieldsAndVals[P]
  private def chainMerge(other: Promethean[_]): Clay = clay.mergeAnyMergeables(other.clay)
}

sealed case class Clay(map: Map[String, Any]) {

  def apply(clayFields: Set[String]): Clay = {
    val stuckMap = clayFields.foldLeft[Map[String, _]](Map())((m , field) => {
      if(map.contains(field)) m ++ Map(field -> map(field)) else m
    })
    Clay(stuckMap)
  }

  def fieldNames: Set[String] = map.keys.toSet
  def prometheanMap[R](f: Promethean[_] => R): Clay = Clay(map.filter(m => m._2.isInstanceOf[Promethean[_]]).mapValues(st => f(st.asInstanceOf[Promethean[_]])))
  def squish(other: Clay): Clay = Clay(map ++ other.map)
  def diff(otherMap: Map[String, Any]): Clay = Clay((map.toSet diff otherMap.toSet).toMap)
  def filterFields(keys: Set[String]): Clay = Clay(map.filterKeys(keys))
  def applyToFields[A, B](fields: Set[String], f: A => B): Clay = Clay(map.transform((key, value) => if (fields(key)) f(value.asInstanceOf[A]) else value))


  def hardenAs[T : ClassTag : TypeTag]: T = {
    val ct = implicitly[ClassTag[T]]
    val fieldNamesAndTypes = PrometheanUtil.getFieldNamesAndTypes[T]
    val defaults = PrometheanUtil.getDefaultFieldsAndVals[T]
    val constructor = ct.runtimeClass.getConstructors.head.asInstanceOf[Constructor[Any]]
    val clayFields = fieldNamesAndTypes.filter(m => PrometheanUtil.isPrometheanMap(map.getOrElse(m._1, None), m._2))

    PrometheanUtil.hardenFieldsAndConstruct(map,
      clayFields,
      defaults,
      constructor,
      ct.runtimeClass.asInstanceOf[Class[Any]]).asInstanceOf[T]
  }

  def canHardenAs[T : ClassTag : TypeTag]: Boolean = {
    val defaultFieldNames = PrometheanUtil.getDefaultFieldsAndVals[T].keys.toSet
    val nonDefaults = PrometheanUtil.getFieldNamesAndTypes[T].filter(m => !defaultFieldNames.contains(m._1))
    nonDefaults.forall(m => map.contains(m._1) && PrometheanUtil.isOfExpectedType(map(m._1), m._2))
  }

  def mergeablesMap: Map[String, Mergeable[_] with Equatable[_]] =
    map.filter(m => m._2.isInstanceOf[Mergeable[_]] && m._2.isInstanceOf[Equatable[_]])
      .transform((k, v) => v.asInstanceOf[Mergeable[_] with Equatable[_]])

  def mergeAnyMergeables(other: Clay): Clay = {
    val mapList = this.mergeablesMap.toList ++ other.mergeablesMap.toList
    val merged = mapList.groupBy( _._1).map {
      case (k,v) => k -> v.map(_._2).reduce((a, b) =>
        equateAndMerge(a.asInstanceOf[Mergeable[a.type] with Equatable[a.type]],
          b.asInstanceOf[Mergeable[b.type] with Equatable[b.type]]))
    }
    Clay(merged)
  }

  def equateAndMerge[T, U](a: Mergeable[T] with Equatable[T], b: Mergeable[U] with Equatable[U]): Mergeable[T] with Equatable[T] =
    if(a.getClass == b.getClass && a === b.asInstanceOf[T]) {
      a.merge(b.asInstanceOf[T]).asInstanceOf[Mergeable[T] with Equatable[T]]
    } else a

  def hardenFromField[R : ClassTag : TypeTag](fieldName: String): R = {
    assert(map.contains(fieldName),
      s"$this does not contain supplied name: $fieldName")

    val fieldNamesAndTypes = PrometheanUtil.getFieldNamesAndTypes[R]
    val orderedFields = PrometheanUtil.classAccessors[R].toArray
    val fieldValues = map(fieldName).asInstanceOf[Array[_]]

    assert(fieldValues.zipWithIndex.forall(a => PrometheanUtil.isOfExpectedType(a._1, fieldNamesAndTypes(orderedFields(a._2)))),
      s"Failure to construct $typeTag[R] from ${fieldValues.toList}")

    Clay(fieldValues.zipWithIndex.foldLeft(Map[String, Any]())((m, k) => m + (orderedFields(k._2) -> k._1))).hardenAs[R]
  }

}

object Clay {
  def apply(): Clay = new Clay(Map[String, Any]())
}

sealed trait PrometheanReaction
sealed case class AbsorbReaction(field: Field, precipitate: Promethean[_] => Any, trigger: Promethean[_] => Boolean) extends PrometheanReaction {
  def reactTo(reactant: Promethean[_]): Clay = Clay(if(trigger(reactant)) Map[String, Any](field.getName -> precipitate(reactant)) else Map[String, Any]())
}
sealed case class AutoReaction(field: Field, precipitate: Any, trigger: Boolean = true) extends PrometheanReaction {
  def react: Clay = Clay(if(trigger) Map[String, Any](field.getName -> precipitate) else Map[String, Any]())
}

object PrometheanUtil {

  def hardenFieldsAndConstruct[P](fieldNamesAndVals: Map[String, Any],
                                  clayFields: Map[String, Type],
                                  defaults: Map[String, Any],
                                  constructor: Constructor[P],
                                  clazz: Class[P]): P = {

    val hardenedClayFieldVals = fieldNamesAndVals
      .filter(m => clayFields.contains(m._1))
      .transform((k, v) => hardenToPromethean(v, clayFields(k)).asInstanceOf[Promethean[_]])

    val combinedMap = fieldNamesAndVals ++ hardenedClayFieldVals
    val constructorArgsFromMap = clazz.getDeclaredFields
      .filter(f => Modifier.isFinal(f.getModifiers))
      .map(f => combinedMap.getOrElse(f.getName, defaults(f.getName)))

    try {
      constructor.newInstance(constructorArgsFromMap.asInstanceOf[Array[Object]]: _*)
    } catch {
      case _: java.lang.IllegalArgumentException =>
        throw new IllegalArgumentException(s"Could not construct $clazz given the following input args: " +
          s"\n${constructorArgsFromMap.toList.map(arg => (arg, arg.getClass.getSimpleName))}")
      case e: Exception => throw e
    }
  }

  def getDefaultFieldsAndVals[C : ClassTag]: Map[String, Any] = {
    val mirror: Mirror = runtimeMirror(getClass.getClassLoader)
    val tag = implicitly[ClassTag[C]]
    val companion = mirror.classSymbol(tag.runtimeClass).companion.asModule
    val instanceMirror = mirror.reflect(mirror.reflectModule(companion).instance)
    val mirrorType = instanceMirror.symbol.typeSignature
    val applyMethod = mirrorType.member(TermName("apply")).asMethod
    getDefaultValueByParam(companion, instanceMirror, mirrorType, applyMethod)
  }

  private case object NoDefaultValueFound

  def defaultValForMemberIndex(p: Symbol, i: Int, mirrorType: Type, instanceMirror: InstanceMirror): Map[String, Any] = {
    Map(p.name.toString -> {
      val defArg = mirrorType.member(TermName(s"apply$$default$$${i + 1}"))
      if (defArg != NoSymbol) {
        instanceMirror.reflectMethod(defArg.asMethod)()
      } else NoDefaultValueFound
    })
  }

  def getDefaultValueByParam(companion: ModuleSymbol,
                             instanceMirror: InstanceMirror,
                             mirrorType: Type,
                             applyMethod: MethodSymbol): Map[String, Any] = {
    (for (ps <- applyMethod.paramLists; p <- ps) yield p).zipWithIndex
      .map(p => defaultValForMemberIndex(p._1, p._2, mirrorType, instanceMirror)).fold(Map()) { (defMap, m) =>
      if (m.valuesIterator.contains(NoSymbol) || m.valuesIterator.isEmpty) defMap else defMap ++ m
    }.filter(_._2 != NoDefaultValueFound)
  }

  def getFieldNamesAndTypes[A: TypeTag]: Map[String, Type] = {
    val members = typeOf[A].members.sorted.collect {
      case m: MethodSymbol if m.isCaseAccessor => Map(m.name.encodedName.toString -> m.typeSignature.resultType)
    }
    if(members.nonEmpty) members.reduce(_ ++ _) else Map()
  }

  def classAccessors[T: TypeTag]: List[String] = typeOf[T].members.sorted.collect {
    case m: MethodSymbol if m.isCaseAccessor => m.name.encodedName.toString
  }

  def getReflectedType[T: TypeTag](obj: T): TypeTag[T] = typeTag[T]

  def isOfExpectedType[U, V](v: Any, tpe: Type): Boolean = {
    (v, tpe) match {
      case _ if isPrometheanMap(v, tpe) => Try(hardenToPromethean(v, tpe)).isSuccess
      case _ => compareAnyToRuntimeType(v, tpe)
    }
  }

  def isPrometheanMap(obj: Any, tpe: Type): Boolean = tpe <:< typeOf[Promethean[_]] && obj.isInstanceOf[Map[_, _]]

  def isRowLikeWithValues(obj: Any, tpe: Type): Boolean =
    obj != null && tpe <:< typeOf[AnyRef] && {
      val pMap = Promethean.smash(obj)
      pMap.fieldNames.contains("values") && pMap.map("values").asInstanceOf[Array[_]].nonEmpty
    }


  def reflectMethod(obj: Any, t: Type, name: String): MethodMirror = {
    val instanceMirror = cm.reflect(t.typeSymbol.companion.asModule)
    val method = t.decl(TermName(name)).asMethod
    instanceMirror.reflectMethod(method)
  }

  def hardenToPromethean(v: Any, t: Type): Any = {
    val clazz = cm.runtimeClass(t.typeSymbol.asClass).asInstanceOf[Class[Any]]
    val constructor = cm.runtimeClass(t).getConstructors.head.asInstanceOf[Constructor[Any]]
    val defaults = getDefaultFieldsAndVals(Manifest.classType(clazz))
    val valuesMap = v.asInstanceOf[Map[String, Any]]
    val fieldNamesAndTypes = getFieldNamesAndTypes(typeToTypeTag(t))
    val clayFields = fieldNamesAndTypes.filter(m => isPrometheanMap(valuesMap.getOrElse(m._1, None), m._2))

    hardenFieldsAndConstruct(valuesMap, clayFields, defaults, constructor, clazz)
  }

  def compareAnyToRuntimeType(obj: Any, t: Type): Boolean = {
    val clazz = cm.runtimeClass(t.typeSymbol.asClass)
    obj match {
      case obj: java.lang.Integer => unboxToInt(obj).getClass == clazz
      case obj: java.lang.Long => unboxToLong(obj).getClass == clazz
      case obj: java.lang.Boolean => unboxToBoolean(obj).getClass == clazz
      case obj: java.lang.Byte => unboxToByte(obj).getClass == clazz
      case obj: java.lang.Character => unboxToChar(obj).getClass == clazz
      case obj: java.lang.Float => unboxToFloat(obj).getClass == clazz
      case obj: java.lang.Short => unboxToShort(obj).getClass == clazz
      case obj: java.lang.Double => unboxToDouble(obj).getClass == clazz
      case other => Try(clazz.cast(other)).isSuccess
    }
  }

  def typeTagToClassTag[T: TypeTag]: ClassTag[_] = ClassTag[T](typeTag[T].mirror.runtimeClass(typeTag[T].tpe))

  def typeToTypeTag[T](tpe: Type, mirror: reflect.api.Mirror[reflect.runtime.universe.type] = cm): TypeTag[T] =
    TypeTag(mirror, new reflect.api.TypeCreator {
      def apply[U <: reflect.api.Universe with Singleton](m: reflect.api.Mirror[U]): U # Type = {
        assert(m eq mirror, s"TypeTag[$tpe] defined in $mirror cannot be migrated to $m.")
        tpe.asInstanceOf[U#Type]
      }
    })

  def typeToCompanionClass[A](tpe: Type): RuntimeClass = {
    import scala.reflect.runtime.{universe => ru}
    val clazz = typeToClazz(tpe)
    val m = ru.runtimeMirror(getClass.getClassLoader)
    cm.runtimeClass(m.classSymbol(clazz).companion.asModule.moduleClass.asClass)
  }

  def typeToClazz(tpe: Type): RuntimeClass = {
    val m = runtimeMirror(getClass.getClassLoader)
    m.runtimeClass(tpe.typeSymbol.asClass)
  }

  def getRuntimeClass(obj: Any): Class[_] = ClassTag(obj.getClass).runtimeClass

  def typeToStaticCompanion(tpe: Type): Any = {
    val mirror: Mirror = runtimeMirror(getClass.getClassLoader)
    val module = mirror.staticModule(typeToClazz(tpe).getName)
    mirror.reflectModule(module).instance
  }

  def clazzToType[T](clazz: Class[T])(implicit runtimeMirror: Mirror): Type = runtimeMirror.classSymbol(clazz).toType
}