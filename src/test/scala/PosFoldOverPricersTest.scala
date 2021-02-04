import org.scalatest.funsuite.AnyFunSuite

object PosFoldOverPricers {
  case class Pricing(quantity: Int, unitPrice: UnitPrice, tierPrice: TierPrice)

  case class UnitPrice(price: BigDecimal)

  case class TierPrice(price: BigDecimal, quantity: Int)

  type Quantity = Int
  type Amount = BigDecimal
  type Pricer = Quantity => (Quantity, Amount)

  def total(pricings: List[Pricing]): Amount =
    pricings.foldLeft(BigDecimal(0)) { (grandTotal, pricing) =>
      val pricers = List(
        priceByTier(pricing.tierPrice.quantity, pricing.tierPrice.price),
        priceByUnit(pricing.unitPrice.price)
      )
      grandTotal + total(pricing.quantity, pricers)
    }

  val priceByTier: (Quantity, Amount) => Pricer = { (quantityPerTier, tierPrice) => { quantity =>
    val tiers = quantity / quantityPerTier
    val tierPriceAmount = tiers * tierPrice
    val remainingQuantityAfterTierPrice = quantity % quantityPerTier
    (remainingQuantityAfterTierPrice, tierPriceAmount)
  } }

  val priceByUnit: Amount => Pricer = { unitPrice => { quantity =>
    val unitPriceAmount = quantity * unitPrice
    val remainingQuantityAfterUnitPrice = 0
    (remainingQuantityAfterUnitPrice, unitPriceAmount)
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

  val croissantUnitPrice = UnitPrice(BigDecimal("1.10"))
  val croissantTierPrice = TierPrice(BigDecimal("2.65"), 3)
  val baguetteUnitPrice = UnitPrice(BigDecimal("0.75"))
  val baguetteTierPrice = TierPrice(BigDecimal("3.00"), 5)

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
    val croissants = Pricing(quantity, croissantUnitPrice, croissantTierPrice)
    val baguettes = Pricing(quantity, baguetteUnitPrice, baguetteTierPrice)
  }
}
