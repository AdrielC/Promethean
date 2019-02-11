import org.scalatest.FunSpec

import scala.reflect.runtime.universe._

class PrometheanTest extends FunSpec {

  describe("clayFields and clayValues") {
    val productOne = TestProductResult(id = 1)
    val productInfoOne = TestProductTableInfo(id = 1, price = 29.99)
    val impressionOne = TestImpressedProduct(id = 1, impressionCCID = "ccid1", seen = true, searchPosition = 10, positionInRow = 1, None, Some("Get it by Christmas"))
    val sameProductOne = TestProductResult(id = 1, wasPrice = 39.99)

    it("should stick fields that are equal to their default values") {
      val stuckTogether = productOne.absorb(productInfoOne).absorb(impressionOne).absorb(sameProductOne)

      val expected = TestProductResult(1, seen = true, 10, 1, 29.99, 39.99, None, Some("Get it by Christmas"))

      assert(stuckTogether === expected)
    }

    it("should use defaults from the chosen class of hardened clay to determine forming behavior") {
      val productIdClay = Clay(Map("id" -> 1))
      val prometheanClasses: Seq[Promethean[_]] = Seq(productOne, productInfoOne, impressionOne, sameProductOne)

      val stuckTogether = prometheanClasses.foldLeft[TestProductResult](productIdClay.hardenAs[TestProductResult])((a, b) => a.absorb(b))

      val expected = TestProductResult(1, seen = true, 10, 1, 29.99, 39.99, None, Some("Get it by Christmas"))

      assert(stuckTogether === expected)
    }

    it("should favor left hand side promethean values from hardened clay when forming") {
      val productIdClay = Clay(Map("id" -> 1, "price" -> 24.99))
      val prometheanClasses: Seq[Promethean[_]] = Seq(productOne, productInfoOne, impressionOne, sameProductOne)

      val stuckTogether = prometheanClasses.foldLeft[TestProductResult](productIdClay.hardenAs[TestProductResult])((a, b) => a.absorb(b))

      val expected = TestProductResult(1, seen = true, 10, 1, 24.99, 39.99, None, Some("Get it by Christmas"))

      assert(stuckTogether === expected)
    }
  }

  describe("Forming similar classes together") {

    val product = DummyProduct(id = 1)

    it("should combine the products into one master info product with price and wasPrice") {
      val productPrice = DummyProduct(id = 1, price = Some(22.99))
      val productWasPrice = DummyProduct(id = 1, wasPrice = Some(30.99))
      val productCouponApplied = DummyProduct(id = 1, couponApplied = Some(false))

      val stuckProducts = product.absorb(productPrice).absorb(productWasPrice).absorb(productCouponApplied)

      assert(stuckProducts.price.contains(22.99))
      assert(stuckProducts.wasPrice.contains(30.99))
    }

    it("should ignore information with same field name from RHS of absorb") {
      val productPrice = DummyProduct(id = 1, price = Some(22.99))
      val productDuplicate = DummyProduct(id = 1, price = Some(1.00))

      val stuckProducts = product.absorb(productPrice).absorb(productDuplicate)

      assert(stuckProducts.price.contains(22.99))
    }

    it("should not form two classes with different identifying fields together") {
      val differentId = DummyProduct(id = 9, price = Some(1.00))

      val stuckProducts = product.absorb(differentId)

      assert(stuckProducts.price.isEmpty)
    }
  }

  describe("Promethean with dissimilar classes") {
    val thingyA = Thingy(id = 1, size = 10)
    val actionOnThingyA = actionDoneToThingy(action = "move", id = 1, size = 10, seen = true, color = "blue", shape = "circle")

    it("should absorb other promethean classes") {
      val stuckTogether = thingyA.absorb(actionOnThingyA)

      assert(stuckTogether.seen === true)
      assert(stuckTogether.color === "blue")
      assert(stuckTogether.shape === "circle")
    }

    it("should harden into other Promethean classes") {
      val hardened = actionOnThingyA.hardenAs[Thingy]
      assert(hardened.isInstanceOf[Thingy])
    }
  }

  describe("Clay functionality") {

    it("Should construct case classes from Maps") {
      val productDataMap = Map("id" -> 1)
      val blob = Clay(productDataMap)
      val hardenedClay = blob.hardenAs[TestProductResult]

      val expected = TestProductResult(1)

      assert(hardenedClay === expected)
    }

    it("Should squish clay that can harden to a class") {
      val priceClay = Clay(Map("id" -> 1, "price" -> 29.99))
      val wasPriceClay = Clay(Map("wasPrice" -> 39.99))

      val squishedClay = (priceClay squish wasPriceClay).hardenAs[TestProductResult]

      val expected = TestProductResult(1, price = 29.99, wasPrice = 39.99)

      assert(squishedClay == expected)
    }

    it("Should ignore non-constructor fields when hardening into a specific class") {
      val blobs: Seq[Clay] = Seq(
        Clay(Map("id" -> 2, "searchPosition" -> 100)),
        Clay(Map("impressionCCID" -> "ccid1", "parameters" -> Seq("Header", "chairs"))),
        Clay(Map("seen" -> true, "positionInRow" -> 0, "uselessinfo" -> "uninteresting information")))

      val squishedAndHardened = blobs.reduce(_ squish _).hardenAs[TestImpressedProduct]

      val expected = TestImpressedProduct(2, "ccid1", seen = true, 100, 0)

      assert(squishedAndHardened == expected)
    }

    it("Should apply a function to specified fields") {
      val clay = Clay(Map("id" -> 1, "name" -> "Bruce", "price" -> 9))

      val transformedClay = clay.applyToFields(Set("price", "id"), (v: Int) => v + 20)

      assert(transformedClay.map("id") === 21)
      assert(transformedClay.map("price") === 29)
    }
  }

  describe("Reactive fields will react accordingly") {
    it("should trigger a value change reaction in seen column") {
      val gizmoA = Gizmo(id = 1)
      val actionOnGizmoA = ActionDoneToGizmo(id = 1, action = "move", size = 10, color = "blue")

      val reacted = gizmoA.absorb(actionOnGizmoA)

      assert(reacted.seen)
    }

    it("should retain post-reaction values for subsequent forms") {
      val gizmoB = Gizmo(id = 2)
      val actionOnGizmoB = ActionDoneToGizmo(id = 2, action = "move", size = 10, color = "blue")
      val gizmoC = Gizmo(id = 2, size = 10)

      val reacted = gizmoB.absorb(actionOnGizmoB).absorb(gizmoC)

      val expected = Gizmo(id = 2, seen = true, size = 10, color = "blue")

      assert(reacted == expected)
    }

    it("should autocatalyze if Gizmo is square") {
      val gizmoD = Gizmo(id = 3, shape = "Trapezoid")

      val reacted = gizmoD.autocatalyze

      val expected = Gizmo(id = 3, seen = true, shape = "Trapezoid", color = "Red")

      assert(reacted == expected)
    }

    it("should catalyze a chain reaction to form together all equatable promethean field values") {
      val gizmoA = Gizmo(id = 3)
      val gizmoB = Gizmo(id = 3, size = 5)
      val expectedGizmo = Gizmo(id = 3, size = 5)

      val actionDoneToGizmoA = ActionDoneToGizmo(id = 3, action = "ship", size = 5, color = "red")
      val actionDoneToGizmoB = ActionDoneToGizmo(id = 3, action = "ship", size = 5, shape = "square")
      val expectedActionDoneToGizmo = ActionDoneToGizmo(id = 3, action = "ship", size = 5, color = "red", shape = "square")

      val gizmoContainerA = GizmoContainer(1, gizmoA, actionDoneToGizmoA)
      val gizmoContainerB = GizmoContainer(1, gizmoB, actionDoneToGizmoB)

      val stuckContainers = gizmoContainerA.absorb(gizmoContainerB)

      assert(stuckContainers.gizmo == expectedGizmo)
      assert(stuckContainers.action == expectedActionDoneToGizmo)
    }

    it("chain autocatalysis should occur N-levels down") {
      val bigChungus =
        Clay(Map(
          "chungusId" -> 1,

          "superGizmoOne" -> Map("superContainerId" -> 1,
            "gizmoContainer" -> Map("containerId" -> 1,
              "gizmo" -> Map("id" -> 1),
              "action" -> Map("id" -> 2, "action" -> "move", "size" -> 10, "color" -> "blue")),
            "location" -> "warehouse"),

          "superGizmoTwo" -> Map("superContainerId" -> 1,
            "gizmoContainer" -> Map("containerId" -> 1,
              "gizmo" -> Map("id" -> 1),
              "action" -> Map("id" -> 2, "action" -> "move", "size" -> 10, "shape" -> "circle")),
            "location" -> "boston"),

          "superGizmoThree" -> Map("superContainerId" -> 1,
            "gizmoContainer" -> Map("containerId" -> 1,
              "gizmo" -> Map("id" -> 1, "shape" -> "Trapezoid"),
              "action" -> Map("id" -> 2, "action" -> "move", "size" -> 10)),
            "location" -> ""),

          "arrested" -> false
        ))

      val chungusOutcome = bigChungus.hardenAs[BigChungus].autocatalyze

      assert(chungusOutcome.superGizmoOne.gizmoContainer.gizmo.seen)
    }
  }

  describe("Promethean keys") {
    it("should create equivalent keys for equatable Promethean class instances") {
      val idA1 = MultiIDFieldsA("A", 1, id3 = true)
      val idA2 = MultiIDFieldsA("A", 1, id3 = true)
      val idA3 = MultiIDFieldsA("B", 1, id3 = false)

      assert(idA1.key == idA2.key)
      assert(idA1 === idA2)
      assert(idA1.key != idA3.key)
      assert(idA1 !== idA3)
    }

    it("should create equivalent keys for mergable instances of different types of Promethean classes") {
      val idA1 = MultiIDFieldsA("A", 1, id3 = true)
      val idB1 = MultiIDFieldsB("A", 1, id3 = true, "carl")

      assert(idA1.key == idB1.key)
      assert(idA1 === idB1)
      assert(idA1.absorb(idB1).isInstanceOf[MultiIDFieldsA])
    }
  }

  describe("PrometheanSets") {
    it("Should use absorb to merge into a merging set") {
      val gizmoA = Gizmo(id = 9)
      val gizmoB = Gizmo(id = 9, seen = true)
      val gizmoC = Gizmo(id = 9, size = 10)
      val gizmoD = Gizmo(id = 10, color = "Blue")
      val gizmoE = Gizmo(id = 9, color = "Red")

      val mergedSet = new MergingSet[Promethean[_]] + gizmoA + gizmoB + gizmoC + gizmoD + gizmoE
      val expected = Gizmo(id = 9, seen = true, size = 10, color = "Red")
      val mergedA = mergedSet.filter(_.clayValues.map("id") == 9).head

      assert(mergedA == expected)
    }

    it("Should form different promethean classes together in a Promethean Merging Set") {
      val gizmoA = Gizmo(id = 10)
      val gizmoB = Gizmo(id = 21)
      val actionGizmoA = ActionDoneToGizmo(id = 10, action = "move", size = 10, shape = "Trapezoid")
      val actionGizmoB = ActionDoneToGizmo(id = 21, action = "ship", size = 300)
      val unstickableSuperGizmo = SuperGizmoContainer(superContainerId = 10, gizmoContainer = GizmoContainer(1, gizmoA, actionGizmoA))

      val mergedSet = (new MergingSet[Promethean[_]] + gizmoA + gizmoB + actionGizmoA + actionGizmoB + unstickableSuperGizmo).map(_.autocatalyze)

      val expectedA = Gizmo(id = 10, seen = true, size = 10, shape = "Trapezoid", color = "Red")
      val expectedB = Gizmo(id = 21, seen = true, size = 300)

      assert(mergedSet.size === 3)
      assert(mergedSet.contains(expectedA))
      assert(mergedSet.contains(expectedB))
    }
  }

  describe("canHardenAs should be reliable for constructing from Maps") {
    it("should be able to harden from simple clay") {
      val testClay = Clay(Map("id" -> 1))

      assert(testClay.canHardenAs[Gizmo])
    }

    it("should harden from values found in field named 'values'") {
      val values: Any = ValuesClass(Array(1))
      val expected = Gizmo(1)

      val result = Promethean.smash(values).hardenFromField[Gizmo]("values")

      assert(result == expected)
    }

    it("Should distinguish between primitives") {
      val testClay = Clay(Map("id" -> 1L))
      val testClay1 = Clay(Map("id" -> 1))

      assert(testClay.canHardenAs[DummyProduct])
      assert(!testClay1.canHardenAs[DummyProduct])
    }

    it("Should handle case class values") {
      val testClayGizmo = Clay(Map("containerId" -> 1, "gizmo" -> Gizmo(1), "action" -> ActionDoneToGizmo(1, "move", 10)))
      val testClayGizmoB = Clay(Map("containerId" -> 1, "gizmo" -> ActionDoneToGizmo(1, "move", 10), "action" -> Gizmo(1)))

      val expected = GizmoContainer(1, Gizmo(1), ActionDoneToGizmo(1, "move", 10))


      assert(testClayGizmo.hardenAs[GizmoContainer] == expected)
      assert(testClayGizmo.canHardenAs[GizmoContainer])
      assert(!testClayGizmoB.canHardenAs[GizmoContainer])
    }

    it("Should handle unhardened maps within maps") {
      val testClaySoft = Clay(Map("containerId" -> 1, "gizmo" -> Map("id" -> 1), "action" -> Map("id" -> 1, "action" -> "move", "size" -> 10)))
      val testClaySoftA = Clay(Map("containerId" -> 1, "gizmo" -> Map("id" -> 1L), "action" -> Map("id" -> 1L, "action" -> "move", "size" -> 10)))

      val expected = GizmoContainer(1, Gizmo(1), ActionDoneToGizmo(1, "move", 10))

      assert(testClaySoft.hardenAs[GizmoContainer] == expected)
      assert(testClaySoft.canHardenAs[GizmoContainer])
      assert(!testClaySoftA.canHardenAs[GizmoContainer])
    }

    it("Should handle nested promethean containters") {
      val superClaySoft = Clay(Map("superContainerId" -> 1,
        "gizmoContainer" -> Map("containerId" -> 1, "gizmo" -> Map("id" -> 1), "action" -> Map("id" -> 1, "action" -> "move", "size" -> 10)),
        "location" -> "store"))

      val expected = SuperGizmoContainer(1, GizmoContainer(1, Gizmo(1), ActionDoneToGizmo(1, "move", 10)), "store")

      assert(superClaySoft.hardenAs[SuperGizmoContainer].isInstanceOf[SuperGizmoContainer])
      assert(superClaySoft.hardenAs[SuperGizmoContainer] == expected)
      assert(superClaySoft.canHardenAs[SuperGizmoContainer])
    }

    it("Should handle the BIGCHUNGUS: Three promethean container levels") {
      val bigChungus =
        Clay(Map(
          "chungusId" -> 1,

          "superGizmoOne" -> Map("superContainerId" -> 1,
            "gizmoContainer" -> Map("containerId" -> 1,
              "gizmo" -> Map("id" -> 1),
              "action" -> Map("id" -> 2, "action" -> "move", "size" -> 10, "color" -> "blue")),
            "location" -> "warehouse"),

          "superGizmoTwo" -> Map("superContainerId" -> 1,
            "gizmoContainer" -> Map("containerId" -> 1,
              "gizmo" -> Map("id" -> 1),
              "action" -> Map("id" -> 2, "action" -> "move", "size" -> 10, "shape" -> "circle")),
            "location" -> "boston"),

          "superGizmoThree" -> Map("superContainerId" -> 1,
            "gizmoContainer" -> Map("containerId" -> 1,
              "gizmo" -> Map("id" -> 1, "shape" -> "Trapezoid"),
              "action" -> Map("id" -> 2, "action" -> "move", "size" -> 10)),
            "location" -> ""),

          "arrested" -> false
        ))

      val chungusCourtDate = Clay(Map("chungusId" -> 1, "arrestMade" -> true))

      val expected =
        BigChungus(1,
          SuperGizmoContainer(1,
            GizmoContainer(1,
              Gizmo(1,seen = true,-1,"Red","Trapezoid"),
              ActionDoneToGizmo(2,"move",10,"blue","circle"),
              moveActionMade = true),
            "boston"),
          SuperGizmoContainer(1,
            GizmoContainer(1,
              Gizmo(1,seen = true,-1,"Red","Trapezoid"),
              ActionDoneToGizmo(2,"move",10,"blue","circle"),
              moveActionMade = true),
            "boston"),
          SuperGizmoContainer(1,
            GizmoContainer(1,
              Gizmo(1,seen = true,-1,"Red","Trapezoid"),
              ActionDoneToGizmo(2,"move",10,"blue","circle"),
              moveActionMade = true),
            "boston"),
          arrested = true)


      val hardenedChungus = bigChungus.hardenAs[BigChungus]
      val hardenedCourtCase = chungusCourtDate.hardenAs[ChungusCourtCase]
      val chungusOutcome = hardenedChungus.absorb(hardenedCourtCase).autocatalyze.autocatalyze

      assert(chungusOutcome == expected)
    }
  }

  describe("Type comparisons") {

    it("Should Unbox primitives and compare") {
      val erasedInt: Any = 1
      val tpe: Type = typeOf[Int]

      assert(PrometheanUtil.compareAnyToRuntimeType(erasedInt, tpe))
    }


    it("Should equate Maps Properly") {
      val map: Any = Map[String, String]("Number Of The Beast" -> {"Six" * 3})
      val tpe: Type = typeOf[Map[String, String]]

      assert(PrometheanUtil.compareAnyToRuntimeType(map, tpe))
    }
  }

  describe("Exception handling") {

    it("Should print out the object array if construction fails") {
      val clay = Clay(Map("id" -> "1"))

      val message = try {
        clay.hardenAs[TestProductResult]
      } catch {
        case iae: IllegalArgumentException => iae.getMessage
        case e: Exception => throw e
      }

      assert(message.toString.contains("Could not construct class com.overstock.collections.TestProductResult given the following input args:"))
    }
  }
}