package highperfscala.orderbook

import java.time.Instant

import org.scalacheck.{Arbitrary, Gen}

case class Price(value: BigDecimal) extends AnyVal

object Price {
  implicit val genPrice: Gen[Price] = Gen.posNum[Double].map(d => Price(BigDecimal(d)))
  implicit val ordering: Ordering[Price] = (x: Price, y: Price) => Ordering.BigDecimal.compare(x.value, y.value)
}

case class OrderId(value: Long) extends AnyVal

object OrderId {
  implicit val genOrderId: Gen[OrderId] = Gen.posNum[Long].map(OrderId.apply)
}

case class Quantity(value: BigDecimal) extends AnyVal

object Quantity {
  implicit val genQuantity: Gen[Quantity] = Gen.posNum[Double].map(d => Quantity(BigDecimal(d)))
  implicit val ordering: Ordering[Quantity] = (x: Quantity, y: Quantity) => Ordering.BigDecimal.compare(x.value, y.value)
}

object OrderSide extends Enumeration {
  val Ask, Bid = Value
}

case class Order(id: OrderId, timestamp: Instant, price: Price, quantity: Quantity, orderSide: OrderSide.Value, isLimit: Boolean)
object Order {
  implicit val genOrder: Gen[Order] = Arbitrary(
    for {
      id <- OrderId.genOrderId
      price <- Price.genPrice
      timestamp <- Arbitrary.arbCalendar.arbitrary.map(c => c.toInstant)
      quantity <- Quantity.genQuantity
      orderType <- Gen.oneOf(OrderSide.Ask, OrderSide.Bid)
      isLimit <- Arbitrary.arbBool.arbitrary
    } yield Order(id, timestamp, price, quantity, orderType, isLimit)
  ).arbitrary

  implicit val ordering: Ordering[Order] = Ordering.by(o => o.timestamp)
}
case class Execution(order: Order)