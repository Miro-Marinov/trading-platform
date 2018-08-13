package parity

import it.unimi.dsi.fastutil.longs._
import javax.annotation.concurrent.NotThreadSafe
import parity.Side.Side

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

//TODO: How to work with BigDecimals

@NotThreadSafe
case class OrderBook(listener: OrderBookListener,
                     bids: Long2ObjectRBTreeMap[PriceLevel],
                     asks: Long2ObjectRBTreeMap[PriceLevel],
                     orders: Long2ObjectOpenHashMap[Order]) {

  def this(listener: OrderBookListener) = {
    this(listener,
      new Long2ObjectRBTreeMap[PriceLevel](LongComparators.OPPOSITE_COMPARATOR),
      new Long2ObjectRBTreeMap[PriceLevel](LongComparators.NATURAL_COMPARATOR),
      new Long2ObjectOpenHashMap[Order]())
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
  def enter(orderId: Long, side: Side, price: Long, size: Long, isMarket: Boolean): Unit = {
    if (orders.containsKey(orderId)) return
    if (isMarket) {
      val marketPrice =  getMarketPriceForSize(side, size)
      if (side == Side.BUY && marketPrice > price ||
        side == Side.SELL && marketPrice < price) return // TODO: Listener
    }
    if (side == Side.BUY) buy(orderId, price, size)
    else sell(orderId, price, size)
  }

  def getMarketPriceForSize(side: Side, size: Long): Long = {
    val sideLevels = if (side == Side.BUY) asks else bids
    if (sideLevels.isEmpty) return 0L

    // Weighted price according to each price level
    @tailrec
    def inner(level: Long, acc: Long, pricesWithVolume: ArrayBuffer[(Long, Long)]): ArrayBuffer[(Long, Long)] = {
      if (acc > size) pricesWithVolume += ((sideLevels(level).price, acc - size))
      else if (acc == size) pricesWithVolume
      else inner(level + 1, acc + sideLevels(level).volume, pricesWithVolume += ((sideLevels(level).price, sideLevels(level).volume)))
    }

    inner(0, sideLevels(0).volume, ArrayBuffer.empty[(Long, Long)])
      .foldLeft(0L) { case (acc, (l, s)) => acc + l * s }
  }

  @tailrec
  private def executeOrder(orderId: Long, price: Long, size: Long, bestLevel: PriceLevel, side: Side)
                          (priceLevelIsWithinPrice: (PriceLevel, Long) => Boolean): Long = {
    if (size > 0 && bestLevel != null && priceLevelIsWithinPrice(bestLevel, price)) {
      val newSize = bestLevel.matchOrder(orderId, side, size, listener)
      if (bestLevel.isEmpty) asks.remove(bestLevel.price)
      executeOrder(orderId, price, newSize, getBestLevel(asks), side)(priceLevelIsWithinPrice)
    } else {
      size
    }
  }

  private def buy(orderId: Long, price: Long, size: Long): Unit = {
    val remainingQuantity =
      executeOrder(orderId, price, size, getBestLevel(asks), Side.BUY)((priceLevel, price) => priceLevel.price <= price)
    if (remainingQuantity > 0) {
      val addedBid = add(bids, orderId, Side.BUY, price, remainingQuantity)
      orders.put(orderId, addedBid)
      listener.added(orderId, Side.BUY, price, remainingQuantity)
    }
  }

  private def sell(orderId: Long, price: Long, size: Long): Unit = {
    val remainingQuantity =
      executeOrder(orderId, price, size, getBestLevel(asks), Side.SELL)((priceLevel, price) => priceLevel.price >= price)
    if (remainingQuantity > 0) {
      val addedAsk = add(asks, orderId, Side.SELL, price, remainingQuantity)
      orders.put(orderId, addedAsk)
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
    val remainingSize = order.size
    if (size >= remainingSize) return
    if (size > 0) order.resize(size)
    else delete(order)
    listener.cancelled(orderId, remainingSize - size, size)
  }

  private def getBestLevel(sideLevels: Long2ObjectRBTreeMap[PriceLevel]): PriceLevel = {
    if (sideLevels.isEmpty) return null
    sideLevels.get(sideLevels.firstLongKey)
  }

  private def add(levels: Long2ObjectRBTreeMap[PriceLevel], orderId: Long, side: Side, price: Long, size: Long) = {
    val level = levels.putIfAbsent(price, new PriceLevel(side, price))
    level.addOrder(orderId, size)
  }

  private def delete(order: Order): Unit = {
    val level = order.priceLevel
    level.delete(order)
    if (level.isEmpty) delete(level)
    orders.remove(order.id)
  }

  private def delete(level: PriceLevel): Unit = {
    level.side match {
      case Side.BUY =>
        bids.remove(level.price)
      case Side.SELL =>
        asks.remove(level.price)
    }
  }
}