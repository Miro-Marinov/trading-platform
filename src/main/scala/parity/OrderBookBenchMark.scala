package parity

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import parity.Side.Side

@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 3, jvmArgsAppend = Array[String]("-XX:+UseParallelGC", "-Xms1G", "-Xmx1G"))
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.SampleTime))
class OrderBookBenchMark {
  private var book: OrderBook = _

  private var nextOrderId = 0L

  @Setup(Level.Iteration) def prepare(): Unit = {
    val orderBookListener = new OrderBookListener() {
      override def matched(restingOrderId: Long, incomingOrderId: Long, incomingSide: Side, price: Long, executedQuantity: Long, remainingQuantity: Long): Unit = {
      }

      override def added(orderId: Long, side: Side, price: Long, size: Long): Unit = {
      }

      override def cancelled(orderId: Long, canceledQuantity: Long, remainingQuantity: Long): Unit = {
      }
    }

    book = new OrderBook(orderBookListener)
    nextOrderId = 0
  }

  @Benchmark def enter(): Unit = {
    book.enter({
      nextOrderId += 1
      nextOrderId - 1
    }, Side.BUY, 100000, 100)
  }

  @Benchmark def enterAndCancel(): Unit = {
    val orderId = {
      nextOrderId += 1
      nextOrderId - 1
    }
    book.enter(orderId, Side.BUY, 100000, 100)
    book.cancel(orderId, 0)
  }

}
