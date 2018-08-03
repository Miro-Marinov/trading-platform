package parity

import parity.Side.Side

class PriceLevel(side: Side, price: Long, orders: java.util.ArrayList[Order]) {

  def this(side: Side, price: Long) = {
    this(side, price, new java.util.ArrayList[Order])
  }

  def getSide: Side = side

  def getPrice: Long = price

  def isEmpty: Boolean = orders.isEmpty

  def add(orderId: Long, size: Long): Order = {
    val order = new Order(this, orderId, size)
    orders.add(order)
    order
  }

  def matchOrder(orderId: Long, side: Side, quantity: Long, listener: OrderBookListener): Long = {
    var matchedQuantity = quantity

    while (matchedQuantity > 0 && !orders.isEmpty) {
      val order = orders.get(0)
      val orderQuantity = order.remainingQuantity
      if (orderQuantity > matchedQuantity) {
        order.reduce(matchedQuantity)
        listener.matched(order.id, orderId, side, price, matchedQuantity, order.remainingQuantity)
        matchedQuantity = 0
      }
      else {
        orders.remove(0)
        listener.matched(order.id, orderId, side, price, orderQuantity, 0)
        matchedQuantity -= orderQuantity
      }
    }
    matchedQuantity
  }

  def delete(order: Order): Unit = {
    orders.remove(order)
  }
}
