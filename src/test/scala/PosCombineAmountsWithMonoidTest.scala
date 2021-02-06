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

  implicit def monoidInstanceForFastMoney: Monoid[FastMoney] = new Monoid[FastMoney] {
    override def empty: FastMoney = FastMoney.of(0, "EUR")

    override def combine(x: FastMoney, y: FastMoney): FastMoney = x add y
  }

  implicit class StringToFastMoneyConversions(amount: String) {
    val eur: FastMoney = FastMoney.of(BigDecimal(amount), "EUR")
  }

  val croissantUnitPrice = priceByUnit("1.10".eur)
  val croissantTierPrice = priceByTier(3, "2.65".eur)
  val baguetteUnitPrice = priceByUnit("0.75".eur)
  val baguetteTierPrice = priceByTier(5, "3.00".eur)

  test("one croissant") {
    assert(total(List(1.croissants)) == "1.10".eur)
  }

  test("two croissants") {
    assert(total(List(2.croissants)) == "2.20".eur)
  }

  test("one croissant, one baguette") {
    assert(total(List(1.croissants, 1.baguettes)) == "1.85".eur)
  }

  test("two croissants, two baguettes") {
    assert(total(List(2.croissants, 2.baguettes)) == "3.70".eur)
  }

  test("three croissants") {
    assert(total(List(3.croissants)) == "2.65".eur)
  }

  test("four croissants") {
    assert(total(List(4.croissants)) == "3.75".eur)
  }

  test("five croissants") {
    assert(total(List(5.croissants)) == "4.85".eur)
  }

  test("six croissants") {
    assert(total(List(6.croissants)) == "5.30".eur)
  }

  test("three croissants, four baguettes") {
    assert(total(List(3.croissants, 4.baguettes)) == "5.65".eur)
  }

  test("three croissants, five baguettes") {
    assert(total(List(3.croissants, 5.baguettes)) == "5.65".eur)
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