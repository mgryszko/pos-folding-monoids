import org.scalatest.funsuite.AnyFunSuite

object PosFoldOverPricers {
  type Quantity = Int
  type Amount = BigDecimal
  type Pricer = Quantity => (Quantity, Amount)

  def total(pricings: List[(Quantity, List[Pricer])]): Amount = {
    pricings.foldLeft(BigDecimal(0)) { case (grandTotal, (quantity, pricers)) =>
      grandTotal + total(quantity, pricers)
    }
  }

  val priceByTier: (Quantity, Amount) => Pricer = { (quantityPerTier, tierPrice) => { quantity =>
    val tiers = quantity / quantityPerTier
    (quantity % quantityPerTier, tiers * tierPrice)
  } }

  val priceByUnit: Amount => Pricer = { unitPrice => { quantity =>
    (0, quantity * unitPrice)
  } }

  private def total(quantity: Quantity, pricers: List[Pricer])  = {
    val (_, total) = pricers.foldLeft((quantity, BigDecimal(0))) {
      case ((quantity, total), pricer) =>
        val (remainingQuantity, amount) = pricer(quantity)
        (remainingQuantity, total + amount)
    }
    total
  }
}

class PosFoldOverPricersTest extends AnyFunSuite {
  import PosFoldOverPricers._

  val croissantUnitPrice = priceByUnit(BigDecimal("1.10"))
  val croissantTierPrice = priceByTier(3, BigDecimal("2.65"))
  val baguetteUnitPrice = priceByUnit(BigDecimal("0.75"))
  val baguetteTierPrice = priceByTier(5, BigDecimal("3.00"))

  test("one croissant") {
    assert(total(List(1.croissants)) == BigDecimal("1.10"))
  }

  test("two croissants") {
    assert(total(List(2.croissants)) == BigDecimal("2.20"))
  }

  test("one croissant, one baguette") {
    assert(total(List(1.croissants, 1.baguettes)) == BigDecimal("1.85"))
  }

  test("two croissants, two baguettes") {
    assert(total(List(2.croissants, 2.baguettes)) == BigDecimal("3.70"))
  }

  test("three croissants") {
    assert(total(List(3.croissants)) == BigDecimal("2.65"))
  }

  test("four croissants") {
    assert(total(List(4.croissants)) == BigDecimal("3.75"))
  }

  test("five croissants") {
    assert(total(List(5.croissants)) == BigDecimal("4.85"))
  }

  test("six croissants") {
    assert(total(List(6.croissants)) == BigDecimal("5.30"))
  }

  test("three croissants, four baguettes") {
    assert(total(List(3.croissants, 4.baguettes)) == BigDecimal("5.65"))
  }

  test("three croissants, five baguettes") {
    assert(total(List(3.croissants, 5.baguettes)) == BigDecimal("5.65"))
  }

  implicit class PricingOps(quantity: Int) {
    val croissants = (quantity, List(croissantTierPrice, croissantUnitPrice))
    val baguettes = (quantity, List(baguetteTierPrice, baguetteUnitPrice))
  }
}
