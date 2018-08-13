package parity

import parity.Side.Side

trait OrderBookListener {
  /**
    * Match an incoming order to a resting order in the order book. The match
    * occurs at the price of the order in the order book.
    *
    * @param restingOrderId    the order identifier of the resting order
    * @param incomingOrderId   the order identifier of the incoming order
    * @param incomingSide      the side of the incoming order
    * @param price             the execution price
    * @param executedQuantity  the executed quantity
    * @param remainingQuantity the remaining quantity of the resting order
    */
  def matched(restingOrderId: Long,
              incomingOrderId: Long,
              incomingSide: Side,
              price: Long,
              executedQuantity: Long,
              remainingQuantity: Long): Unit

  /**
    * Add an order to the order book.
    *
    * @param orderId the order identifier
    * @param side    the side
    * @param price   the limit price
    * @param size    the size
    */
  def added(orderId: Long, side: Side, price: Long, size: Long): Unit

  /**
    * Cancel a quantity of an order.
    *
    * @param orderId           the order identifier
    * @param canceledQuantity  the canceled quantity
    * @param remainingQuantity the remaining quantity
    */
  def cancelled(orderId: Long, canceledQuantity: Long, remainingQuantity: Long): Unit
}
