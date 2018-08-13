package parity

import parity.Side.Side

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class PriceLevel(side: Side, price: Long, orders: ArrayBuffer[Order], volume: Long) {

  def this(side: Side, price: Long) = {
    this(side, price, ArrayBuffer.empty[Order])
  }

  def isEmpty: Boolean = orders.isEmpty

  def addOrder(orderId: Long, size: Long): Order = {
    val order = new Order(this, orderId, size)
    orders += order
    volume += size
    order
  }

  def matchOrder(orderId: Long, side: Side, size: Long, listener: OrderBookListener): Long = {
    @tailrec
    def matchOrderInner(matchedSize: Long): Long = {
      if (matchedSize > 0 && orders.nonEmpty) {
        val order = orders(0)
        val orderSize = order.size
        if (orderSize > matchedSize) {
          order.reduceBy(matchedSize)
          volume -= matchedSize
          listener.matched(order.id, orderId, side, price, matchedSize, order.size)
          0L
        }
        else {
          orders.remove(0)
          volume -= orderSize
          listener.matched(order.id, orderId, side, price, orderSize, 0)
          matchOrderInner(matchedSize - orderSize)
        }
      } else {
        matchedSize
      }
    }

    matchOrderInner(size)
  }

  def delete(order: Order): Unit = {
    orders -= order
    volume -= order.size
  }
}