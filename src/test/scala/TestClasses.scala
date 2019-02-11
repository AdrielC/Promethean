import java.lang.reflect.Field

/**
  * Classes and Types used for testing
  */

object Types extends scala.AnyRef {
  type ProductId = scala.Long
  type OptionId = scala.Long
  type TaxonomyId = scala.Long
  type RelevanceJudgement = scala.Double
  type Money = scala.Double
  type CustomerId = scala.Long
  type Userseed = scala.Long
  type OCode = scala.Predef.String
}

case class DummyProduct(id: Types.ProductId,
                        price: Option[Types.Money] = None,
                        wasPrice: Option[Types.Money] = None,
                        memberPrice: Option[Types.Money] = None,
                        subcatId: Option[Types.TaxonomyId] = None,
                        optionIds: Option[Seq[Types.OptionId]] = None,
                        name: Option[String] = None,
                        shortName: Option[String] = None,
                        proValueMessage: Option[String] = None,
                        onSaleExpirationDateSearch: Option[Seq[String]] = None,
                        subcatIds: Option[Seq[String]] = None,
                        mapPrice: Option[Types.Money] = None,
                        retailPrice: Option[Types.Money] = None,
                        reviewCount: Option[Int] = None,
                        favoriteCount: Option[Int] = None,
                        reviewRating: Option[Double] = None,
                        recommendedRank: Option[Long] = None,
                        searchPosition: Option[Int] = None,
                        positionInRow: Option[Int] = None,
                        quickView: Option[Boolean] = None,
                        marketplace: Option[Boolean] = None,
                        beforeEventDelivery: Option[Boolean] = None,
                        twoDayDelivery: Option[Boolean] = None,
                        couponApplied: Option[Boolean] = None,
                        topCallout: Option[String] = None,
                        holidayBanner: Option[String] = None) extends Promethean[DummyProduct] {
  override def identifyingFields: Seq[Field] = Seq(field("id"))
}


case class Thingy(id: Int,
                  size: Int,
                  seen: Boolean = false,
                  color: String = "unknown",
                  shape: String = "unknown") extends Promethean[Thingy] {
  override def identifyingFields: Seq[Field] = Seq(field("id"))
}

case class actionDoneToThingy(action: String,
                              id: Int,
                              size: Int,
                              seen: Boolean,
                              color: String,
                              shape: String) extends Promethean[actionDoneToThingy] {
  override def identifyingFields: Seq[Field] = Seq(field("id"))
}

case class TestProductResult(id: Long,
                             seen: Boolean = false,
                             searchPosition: Int = -1,
                             positionInRow: Int = -1,
                             price: Double = -1.0,
                             wasPrice: Double = -1.0,
                             topCallout: Option[String] = None,
                             holidayBanner: Option[String] = None)
  extends Promethean[TestProductResult] {
  override def identifyingFields: Seq[Field] = Seq(field("id"))
}

case class TestProductTableInfo(id: Long, price: Double) extends Promethean[TestProductTableInfo] {
  override def identifyingFields: Seq[Field] = Seq(field("id"))
}

case class TestImpressedProduct(id: Types.ProductId,
                                impressionCCID: String,
                                seen: Boolean,
                                searchPosition: Int,
                                positionInRow: Int,
                                topCallout: Option[String] = None,
                                holidayBanner: Option[String] = None)
  extends Promethean[TestImpressedProduct] {
  override def identifyingFields: Seq[Field] = Seq(field("id"))
}

case class Gizmo(id: Int,
                 seen: Boolean = false,
                 size: Int = -1,
                 color: String = "",
                 shape: String = "") extends Promethean[Gizmo] {
  override def identifyingFields = Seq(field("id"))
  override def reactiveFields = Seq(
    AbsorbReaction(field("seen"), b => true, b => b.isInstanceOf[ActionDoneToGizmo]),
    AutoReaction(field("color"), "Red", shape.contains("Trapezoid")),
    AutoReaction(field("seen"), {val sf = clayFields; !sf.contains("size") || !sf.contains("color") || !sf.contains("shape")}))
}

case class ActionDoneToGizmo(id: Int,
                             action: String,
                             size: Int,
                             color: String = "",
                             shape: String = "") extends Promethean[ActionDoneToGizmo] {
  override def identifyingFields: Seq[Field] = Seq(field("id"))
}

case class GizmoContainer(containerId: Int,
                          gizmo: Gizmo,
                          action: ActionDoneToGizmo,
                          moveActionMade: Boolean = false) extends Promethean[GizmoContainer] {
  override def identifyingFields: Seq[Field] = Seq(field("containerId"))
  override def reactiveFields = Seq(
    AutoReaction(field("moveActionMade"), action.action.contains("move")))
}

case class SuperGizmoContainer(superContainerId: Int,
                               gizmoContainer: GizmoContainer,
                               location: String = "warehouse") extends Promethean[SuperGizmoContainer] {
  override def identifyingFields: Seq[Field] = Seq(field("superContainerId"))
}

case class BigChungus(chungusId: Int,
                      superGizmoOne: SuperGizmoContainer,
                      superGizmoTwo: SuperGizmoContainer,
                      superGizmoThree: SuperGizmoContainer,
                      arrested: Boolean = false) extends Promethean[BigChungus] {
  override def identifyingFields = Seq(field("chungusId"))
  override def reactiveFields = Seq(
    AbsorbReaction(field("arrested"), b => b.asInstanceOf[ChungusCourtCase].arrestMade, b => b.isInstanceOf[ChungusCourtCase]),
    AutoReaction(field("superGizmoOne"), superGizmoOne.absorb(superGizmoTwo).absorb(superGizmoThree)),
    AutoReaction(field("superGizmoOne"), superGizmoOne.absorb(superGizmoTwo).absorb(superGizmoThree)),
    AutoReaction(field("superGizmoTwo"), superGizmoOne),
    AutoReaction(field("superGizmoThree"), superGizmoOne))
}

case class ChungusCourtCase(chungusId: Int, arrestMade: Boolean = true) extends Promethean[ChungusCourtCase] {
  override def identifyingFields = Seq(field("chungusId"))
}

case class ValuesClass(values: Array[_])

case class MultiIDFieldsA(id1: String,
                          id2: Int,
                          id3: Boolean) extends Promethean[MultiIDFieldsA] {
  override def identifyingFields: Seq[Field] = Seq(field("id1"), field("id2"), field("id3"))
}

case class MultiIDFieldsB(id1: String,
                          id2: Int,
                          id3: Boolean,
                          name: String) extends Promethean[MultiIDFieldsB] {
  override def identifyingFields: Seq[Field] = Seq(field("id1"), field("id2"), field("id3"))
}
