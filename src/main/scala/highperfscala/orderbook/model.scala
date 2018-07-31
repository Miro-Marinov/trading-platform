package highperfscala.orderbook

import java.time.Instant

import org.scalacheck.{Arbitrary, Gen}

case class Price(value: BigDecimal)

object Price {
  implicit val genPrice: Gen[Price] = Gen.posNum[Double].map(d => Price(BigDecimal(d)))
  implicit val ordering: Ordering[Price] = (x: Price, y: Price) => Ordering.BigDecimal.compare(x.value, y.value)
}

case class OrderId(value: Long)

object OrderId {
  implicit val genOrderId: Gen[OrderId] = Gen.posNum[Long].map(OrderId.apply)
}

case class Quantity(value: BigDecimal)

object Quantity {
  implicit val genQuantity: Gen[Quantity] = Gen.posNum[Double].map(d => Quantity(BigDecimal(d)))
  implicit val ordering: Ordering[Quantity] = (x: Quantity, y: Quantity) => Ordering.BigDecimal.compare(x.value, y.value)
}

object OrderSide extends Enumeration {
  val Ask, Bid = Value
}

sealed trait Order {
  def id: OrderId

  def timestamp: Instant

  def price: Price

  def quantity: Quantity

  def orderSide: OrderSide.Value
}

case class LimitOrder(id: OrderId, timestamp: Instant, price: Price, quantity: Quantity, orderSide: OrderSide.Value) extends Order

case class MarketOrder(id: OrderId, timestamp: Instant, price: Price, quantity: Quantity, orderSide: OrderSide.Value) extends Order

object LimitOrder {
  implicit val genLimitOrder: Gen[LimitOrder] = Arbitrary(
    for {
      id <- OrderId.genOrderId
      price <- Price.genPrice
      timestamp <- Arbitrary.arbCalendar.arbitrary.map(c => c.toInstant)
      quantity <- Quantity.genQuantity
      orderType <- Gen.oneOf(OrderSide.Ask, OrderSide.Bid)
    } yield LimitOrder(id, timestamp, price, quantity, orderType)
  ).arbitrary
}

object MarketOrder {
  implicit val genMarketOrder: Gen[MarketOrder] = Arbitrary(
    for {
      id <- OrderId.genOrderId
      price <- Price.genPrice
      timestamp <- Arbitrary.arbCalendar.arbitrary.map(c => c.toInstant)
      quantity <- Quantity.genQuantity
      orderType <- Gen.oneOf(OrderSide.Ask, OrderSide.Bid)
    } yield MarketOrder(id, timestamp, price, quantity, orderType)
  ).arbitrary
}

object Order {
  implicit val genOrder: Gen[Order] = Gen.oneOf(MarketOrder.genMarketOrder, LimitOrder.genLimitOrder)
}

case class Execution(order: Order)