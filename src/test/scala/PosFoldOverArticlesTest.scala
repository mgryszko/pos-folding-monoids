import org.scalatest.funsuite.AnyFunSuite

object PosFoldOverArticles {
  case class Pricing(val quantity: Int, val unitPrice: UnitPrice, val tierPrice: TierPrice)

  case class UnitPrice(val price: BigDecimal)

  case class TierPrice(val price: BigDecimal, val quantity: Int)

  def total(pricings: List[Pricing]): BigDecimal =
    pricings.foldLeft(BigDecimal(0)) { (grandTotal, pricing) => grandTotal + total(pricing) }

  private def total(pricing: Pricing): BigDecimal = {
    val tiers = pricing.quantity / pricing.tierPrice.quantity
    val regularPricedArticles = pricing.quantity % pricing.tierPrice.quantity
    tiers * pricing.tierPrice.price + regularPricedArticles * pricing.unitPrice.price
  }
}

class PosFoldOverArticlesTest extends AnyFunSuite {
  import PosFoldOverArticles._

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
