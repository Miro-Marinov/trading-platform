package scala.highperfscala.benchmarks

import java.time.Instant
import java.util.concurrent.TimeUnit

import highperfscala.orderbook._
import org.openjdk.jmh.annotations.Mode._
import org.openjdk.jmh.annotations._

import scala.highperfscala.benchmarks.CancelBenchmarks.BookWithLargeQueue
import scala.highperfscala.orderbook.Commands.{AddLimitOrder, CancelOrder}
import scala.highperfscala.orderbook.Events.Event
import scala.highperfscala.orderbook.OrderBook

// Run via
// sbt ';project performance;jmh:run .*CancelBenchmarks -wi 3 -i 10 -f 1 -wbs 100000 -bs 100000 -jvmArgs "-Xmx1G -Xms1G" -foe true -p enqueuedOrderCount=10,100,1000,5000'
@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class CancelBenchmarks {

  @Benchmark
  def cancelLastOrderInLine(b: BookWithLargeQueue): (OrderBook, Event) =
    b.book.handle(b.cancelLast)

  @Benchmark
  def cancelFirstOrderInLine(b: BookWithLargeQueue): (OrderBook, Event) =
    b.book.handle(b.cancelFirst)

  @Benchmark
  def cancelNonexistentOrder(b: BookWithLargeQueue): (OrderBook, Event) =
    b.book.handle(b.cancelNonexistent)
}

object CancelBenchmarks {

  @State(Scope.Benchmark)
  class BookWithLargeQueue {
    private val p = Price(BigDecimal(1.00))
    private val firstId: Int = 1
    private val defaultCancelLast = CancelOrder(OrderId(-1))

    @Param(Array("1", "100", "1000"))
    var enqueuedOrderCount: Int = 0

    var book: OrderBook = OrderBook.empty

    @Setup(Level.Trial)
    def setup(): Unit = {
      if (enqueuedOrderCount < 0)
        sys.error(s"Invalid enqueued order count = $enqueuedOrderCount")
      assert(book == OrderBook.empty)
      assert(cancelLast == defaultCancelLast)

      cancelLast = CancelOrder(OrderId(enqueuedOrderCount))
      book = {
        (firstId to enqueuedOrderCount).foldLeft(OrderBook.empty) {
          case (ob, i) =>
            ob.handle(AddLimitOrder(LimitOrder(OrderId(i), Instant.now, p, Quantity(15.2), OrderSide.Ask)))._1
        }
      }

      assert(cancelLast != defaultCancelLast)
      if (enqueuedOrderCount > 0)
        assert(book.bids.head._2.size == enqueuedOrderCount,
          s"Book built incorrectly! Expected book to contain " +
            s"$enqueuedOrderCount bids for $p, but actual book is $book")
    }

    var cancelLast: CancelOrder = defaultCancelLast
    val cancelFirst: CancelOrder = CancelOrder(OrderId(firstId))
    val cancelNonexistent: CancelOrder = CancelOrder(OrderId(-1))
  }

}
