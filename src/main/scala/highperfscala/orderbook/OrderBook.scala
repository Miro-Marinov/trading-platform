package scala.highperfscala.orderbook

import highperfscala.orderbook._

import scala.collection.immutable.TreeMap
import scala.highperfscala.orderbook.Commands.{AddOrder, CancelOrder, Command}
import scala.highperfscala.orderbook.Events.{Event, LimitOrderAdded, OrderCanceled, OrderExecuted}

//noinspection SimplifyBooleanMatch
// http://web.archive.org/web/20110312023826/http://www.quantcup.org/home/howtohft_howtobuildafastlimitorderbook
// TreeMap is not cache friendly
case class OrderBook(bids: TreeMap[Price, Vector[Order]],
                     asks: TreeMap[Price, Vector[Order]]) {

  def bestBid: Option[Order] = bids.lastOption.flatMap(_._2.headOption)

  def bestAsk: Option[Order] = asks.headOption.flatMap(_._2.headOption)

  // Could make sense to handle with State monad
  def handle(c: Command): (OrderBook, Event) = c match {
    case AddOrder(o) if o.isLimit => handleLimitOrder(o)
    case CancelOrder(id) => handleCancelOrder(id)
    case AddOrder(o) => handleMarketOrder(o)
  }

  //TODO: Implement
  private def handleMarketOrder(marketOrder: Order): (OrderBook, Event) = (this, Event.orderCancelRejected)

  private def handleLimitOrder(limitOrder: Order) = {
    limitOrder.orderSide match {
      case OrderSide.Bid =>
        bestAsk.exists(limitOrder.price.value >= _.price.value) match {
          case true => crossBookBuy(limitOrder, this)
          case false =>
            val orders = bids.getOrElse(limitOrder.price, Vector.empty)
            val newBid = limitOrder.price -> (orders :+ limitOrder)
            (copy(bids = bids + newBid), LimitOrderAdded)
        }
      case OrderSide.Ask =>
        bestBid.exists(limitOrder.price.value <= _.price.value) match {
          case true => crossBookSell(limitOrder, this)
          case false =>
            val orders = asks.getOrElse(limitOrder.price, Vector.empty)
            val newAsk = limitOrder.price -> (orders :+ limitOrder)
            (copy(asks = asks + newAsk), LimitOrderAdded)
        }
    }
  }

  // Very ugly
  private def handleCancelOrder(id: OrderId): (OrderBook, Event) = {
    bids.find { case (p, q) => q.exists(_.id == id) }.fold(
      asks.find { case (p, q) => q.exists(_.id == id) }.fold(
        this -> Event.orderCancelRejected) { case (p, q) =>
        // It's awkward to duplicate the queue remove logic, but the different
        // types for bid vs ask make it difficult to share the code
        val updatedQ = q.filter(_.id != id)
        copy(asks = updatedQ.nonEmpty match {
          case true => asks + (p -> updatedQ)
          case false => asks - p
        }) -> OrderCanceled
      }) { case (p, q) =>
      val updatedQ = q.filter(_.id != id)
      copy(bids = updatedQ.nonEmpty match {
        case true => bids + (p -> updatedQ)
        case false => bids - p
      }) -> OrderCanceled
    }
  }

  private def crossBookBuy(bid: Order, ob: OrderBook): (OrderBook, Event) =
  // TODO: At this point asks cannot be empty so the handleOrder code is redundant
    asks.headOption.fold(handleLimitOrder(bid)) { case (_, queue) =>
      val matched = queue(0)
      val newQueue = queue.drop(1)
      val price = matched.price
      val newAsks = newQueue.isEmpty match {
        case true => asks - price
        case false => asks + (price -> newQueue)
      }
      val newOb = ob.copy(asks = newAsks)
      matched.price.value.compare(bid.price.value) match {
        case 1 => // matched is more
          val newPrice = matched.quantity.value - bid.quantity.value
          val newAsk = matched.copy(quantity = Quantity(newPrice))
          (ob.copy(asks = asks + (price -> (newAsk +: newQueue))), OrderExecuted(Execution(bid), Execution(matched)))
        case -1 => // matched is less
          val newQuantity = bid.quantity.value - matched.quantity.value
          val newBid = bid.copy(quantity = Quantity(newQuantity))
          newOb.handleLimitOrder(newBid)
        case 0 => // equal
          (newOb, OrderExecuted(Execution(bid), Execution(matched)))
      }
    }

  private def crossBookSell(ask: Order, ob: OrderBook): (OrderBook, Event) =
  // TODO: At this point asks cannot be empty so the handleOrder code is redundant
    bids.lastOption.fold(handleLimitOrder(ask)) { case (_, queue) =>
      val matched = queue(0)
      val newQueue = queue.drop(1)
      val newBids = newQueue.isEmpty match {
        case true => bids - matched.price
        case false => bids + (matched.price -> newQueue)
      }
      val newOb = ob.copy(bids = newBids)
      matched.price.value.compare(ask.price.value) match {
        case 1 => // matched is more
          val newPrice = matched.quantity.value - ask.quantity.value
          val newBid = matched.copy(quantity = Quantity(newPrice))
          (ob.copy(bids = bids + (matched.price -> (newBid +: newQueue))), OrderExecuted(Execution(matched), Execution(ask)))
        case -1 => // matched is less
          val newQuantity = ask.quantity.value - matched.quantity.value
          val newAsk = ask.copy(quantity = Quantity(newQuantity))
          newOb.handleLimitOrder(newAsk)
        case 0 => // equal
          (newOb, OrderExecuted(Execution(matched), Execution(ask)))
      }
    }
}

object OrderBook {
  val noEvent: Option[Event] = None
  val empty: OrderBook = OrderBook(
    TreeMap.empty[Price, Vector[Order]],
    TreeMap.empty[Price, Vector[Order]])
}

object Commands {

  sealed trait Command

  case class AddOrder(o: Order) extends Command

  case class CancelOrder(id: OrderId) extends Command

}

object Events {

  sealed trait Event

  object Event {
    val orderCancelRejected: Event = OrderCancelRejected
  }

  case class OrderExecuted(buy: Execution, sell: Execution) extends Event

  case object LimitOrderAdded extends Event

  case object OrderCancelRejected extends Event

  case object OrderCanceled extends Event

}
