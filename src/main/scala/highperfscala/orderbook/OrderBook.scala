package scala.highperfscala.orderbook

import highperfscala.orderbook._

import scala.collection.immutable.{Queue, TreeMap}
import scala.highperfscala.orderbook.Commands.{AddLimitOrder, AddMarketOrder, CancelOrder, Command}
import scala.highperfscala.orderbook.Events.{Event, LimitOrderAdded, OrderCanceled, OrderExecuted}

//noinspection SimplifyBooleanMatch
// http://web.archive.org/web/20110312023826/http://www.quantcup.org/home/howtohft_howtobuildafastlimitorderbook
// TreeMap is not cache friendly
case class OrderBook(bids: TreeMap[Price, Queue[Order]],
                     asks: TreeMap[Price, Queue[Order]]) {

  def bestBid: Option[Order] = bids.lastOption.flatMap(_._2.headOption)

  def bestAsk: Option[Order] = asks.headOption.flatMap(_._2.headOption)

  // Could make sense to handle with State monad
  def handle(c: Command): (OrderBook, Event) = c match {
    case AddLimitOrder(o) => handleLimitOrder(o)
    case AddMarketOrder(o) => handleMarketOrder(o)
    case CancelOrder(id) => handleCancelOrder(id)
  }

  //TODO: Implement
  private def handleMarketOrder(marketOrder: MarketOrder): (OrderBook, Event) = (this, Event.orderCancelRejected)

  private def handleLimitOrder(limitOrder: LimitOrder) = {
    limitOrder.orderSide match {
      case OrderSide.Bid =>
        bestAsk.exists(limitOrder.price.value >= _.price.value) match {
          case true => crossBookBuy(limitOrder)
          case false =>
            val orders = bids.getOrElse(limitOrder.price, Queue.empty)
            val newBid = limitOrder.price -> orders.enqueue(limitOrder)
            (copy(bids = bids + newBid), LimitOrderAdded)
        }
      case OrderSide.Ask =>
        bestBid.exists(limitOrder.price.value <= _.price.value) match {
          case true => crossBookSell(limitOrder)
          case false =>
            val orders = asks.getOrElse(limitOrder.price, Queue.empty)
            val newAsk = limitOrder.price -> orders.enqueue(limitOrder)
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

  private def crossBookBuy(bid: LimitOrder): (OrderBook, Event) =
  // TODO: At this point asks cannot be empty so the handleOrder code is redundant
    asks.headOption.fold(handleLimitOrder(bid)) { case (_, queue) =>
      val (matched, newQueue) = queue.dequeue
      val newAsks = newQueue.isEmpty match {
        case true => asks - matched.price
        case false => asks + (matched.price -> newQueue)
      }
      (copy(asks = newAsks), OrderExecuted(Execution(bid), Execution(matched)))
    }

  private def crossBookSell(ask: LimitOrder): (OrderBook, Event) =
  // TODO: At this point asks cannot be empty so the handleOrder code is redundant
    bids.lastOption.fold(handleLimitOrder(ask)) { case (_, xs) =>
      val (matched, qq) = xs.dequeue
      val newBids = qq.isEmpty match {
        case true => bids - matched.price
        case false => bids + (matched.price -> qq)
      }
      (copy(bids = newBids), OrderExecuted(Execution(matched), Execution(ask)))
    }
}

object OrderBook {
  val noEvent: Option[Event] = None
  val empty: OrderBook = OrderBook(
    TreeMap.empty[Price, Queue[Order]],
    TreeMap.empty[Price, Queue[Order]])
}

object Commands {
  sealed trait Command

  case class AddLimitOrder(o: LimitOrder) extends Command

  case class AddMarketOrder(o: MarketOrder) extends Command

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
