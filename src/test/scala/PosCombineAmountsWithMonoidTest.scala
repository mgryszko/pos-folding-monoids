import cats.kernel.Monoid
import cats.syntax.monoid._
import org.javamoney.moneta.FastMoney
import org.scalatest.funsuite.AnyFunSuite

object PosCombineAmountsWithMonoid {
  type Quantity = Int
  type Pricer[A] = Quantity => (Quantity, A)

  def total[A: Monoid](pricings: List[(Quantity, List[Pricer[A]])]): A = {
    pricings.foldLeft(Monoid[A].empty) { case (grandTotal, (quantity, pricers)) =>
      grandTotal |+| total(quantity, pricers)
    }
  }

  private def total[A: Monoid](quantity: Quantity, pricers: List[Pricer[A]]): A = {
    val (_, total) = pricers.foldLeft((quantity, Monoid[A].empty)) {
      case ((quantity, total), pricer) =>
        val (remainingQuantity, amount) = pricer(quantity)
        (remainingQuantity, total |+| amount)
    }
    total
  }

  def priceByTier[A: Monoid](quantityPerTier: Quantity, tierPrice: A): Pricer[A] = { quantity =>
    val tiers = quantity / quantityPerTier
    (quantity % quantityPerTier, tierPrice.combineN(tiers))
  }

  def priceByUnit[A: Monoid](unitPrice: A): Pricer[A] = { quantity =>
    (0, unitPrice.combineN(quantity))
  }
}

class PosCombineAmountsWithMonoidTest extends AnyFunSuite {
  import PosCombineAmountsWithMonoid._

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

class FastMoneyMonoidTest extends AnyFunSuite {
  implicit def monoidInstanceForFastMoney: Monoid[FastMoney] = new Monoid[FastMoney] {
    override def empty: FastMoney = FastMoney.of(0, "EUR")

    override def combine(x: FastMoney, y: FastMoney): FastMoney = x add y
  }

  implicit class IntToFastMoneyConversions(amount: Int) {
    val eur: FastMoney = FastMoney.of(amount, "EUR")
  }

  val (x, y, z) = (1.eur, 2.eur, 3.eur)

  test("zero") {
    assert(Monoid[FastMoney].empty == 0.eur)
  }

  test("associativity") {
    assert(((x |+| y) |+| z) == (x |+| (y |+| z)))
  }

  test("left identity") {
    assert((Monoid.empty |+| x) == x)
  }

  test("right identity") {
    assert((x |+| Monoid.empty) == x)
  }
}