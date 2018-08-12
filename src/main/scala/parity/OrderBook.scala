package parity

import it.unimi.dsi.fastutil.longs.{Long2ObjectOpenHashMap, Long2ObjectRBTreeMap, LongComparators}
import parity.Side.Side

class OrderBook(listener: OrderBookListener, bids: Long2ObjectRBTreeMap[PriceLevel],
                asks: Long2ObjectRBTreeMap[PriceLevel], orders: Long2ObjectOpenHashMap[Order]) {

  def this(listener: OrderBookListener) = {
    this(listener,
      new Long2ObjectRBTreeMap[PriceLevel](LongComparators.OPPOSITE_COMPARATOR),
      new Long2ObjectRBTreeMap[PriceLevel](LongComparators.NATURAL_COMPARATOR),
      new Long2ObjectOpenHashMap[Order]
    )
  }

  /**
    * Enter an order to this order book.
    *
    * <p>The incoming order is first matched against resting orders in this
    * order book. This operation results in zero or more Match events.</p>
    *
    * <p>If the remaining quantity is not zero after the matching operation,
    * the remaining quantity is added to this order book and an Add event is
    * triggered.</p>
    *
    * <p>If the order identifier is known, do nothing.</p>
    *
    * @param orderId an order identifier
    * @param side    the side
    * @param price   the limit price
    * @param size    the size
    */
  def enter(orderId: Long, side: Side, price: Long, size: Long): Unit = {
    if (orders.containsKey(orderId)) return
    if (side == Side.BUY) buy(orderId, price, size)
    else sell(orderId, price, size)
  }

  private def buy(orderId: Long, price: Long, size: Long): Unit = {
    var remainingQuantity = size
    var bestLevel = getBestLevel(asks)
    while (remainingQuantity > 0 && bestLevel != null && bestLevel.getPrice <= price) {
      remainingQuantity = bestLevel.matchOrder(orderId, Side.BUY, remainingQuantity, listener)
      if (bestLevel.isEmpty) asks.remove(bestLevel.getPrice)
      bestLevel = getBestLevel(asks)
    }
    if (remainingQuantity > 0) {
      orders.put(orderId, add(bids, orderId, Side.BUY, price, remainingQuantity))
      listener.added(orderId, Side.BUY, price, remainingQuantity)
    }
  }

  private def sell(orderId: Long, price: Long, size: Long): Unit = {
    var remainingQuantity = size
    var bestLevel = getBestLevel(bids)
    while (remainingQuantity > 0 && bestLevel != null && bestLevel.getPrice >= price) {
      remainingQuantity = bestLevel.matchOrder(orderId, Side.SELL, remainingQuantity, listener)
      if (bestLevel.isEmpty) bids.remove(bestLevel.getPrice)
      bestLevel = getBestLevel(bids)
    }
    if (remainingQuantity > 0) {
      orders.put(orderId, add(asks, orderId, Side.SELL, price, remainingQuantity))
      listener.added(orderId, Side.SELL, price, remainingQuantity)
    }
  }

  /**
    * Cancel a quantity of an order in this order book. The size refers
    * to the new order size. If the new order size is set to zero, the
    * order is deleted from this order book.
    *
    * <p>A Cancel event is triggered.</p>
    *
    * <p>If the order identifier is unknown, do nothing.</p>
    *
    * @param orderId the order identifier
    * @param size    the new size
    */
  def cancel(orderId: Long, size: Long): Unit = {
    val order = orders.get(orderId)
    if (order == null) return
    val remainingQuantity = order.remainingQuantity
    if (size >= remainingQuantity) return
    if (size > 0) order.resize(size)
    else {
      delete(order)
      orders.remove(orderId)
    }
    listener.cancelled(orderId, remainingQuantity - size, size)
  }

  private def getBestLevel(levels: Long2ObjectRBTreeMap[PriceLevel]): PriceLevel = {
    if (levels.isEmpty) return null
    levels.get(levels.firstLongKey)
  }

  private def add(levels: Long2ObjectRBTreeMap[PriceLevel], orderId: Long, side: Side, price: Long, size: Long) = {
    var level = levels.get(price)
    if (level == null) {
      level = new PriceLevel(side, price)
      levels.put(price, level)
    }
    level.add(orderId, size)
  }

  private def delete(order: Order): Unit = {
    val level = order.level
    level.delete(order)
    if (level.isEmpty) delete(level)
  }

  private def delete(level: PriceLevel): Unit = {
    level.getSide match {
      case Side.BUY =>
        bids.remove(level.getPrice)
      case Side.SELL =>
        asks.remove(level.getPrice)
    }
  }
}
