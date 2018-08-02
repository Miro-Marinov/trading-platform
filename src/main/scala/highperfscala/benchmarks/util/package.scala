package scala.highperfscala.benchmarks

import com.codahale.metrics.Snapshot
import com.typesafe.scalalogging.LazyLogging

import scala.highperfscala.orderbook.Commands.Command
import scala.highperfscala.orderbook.OrderBook

package object util extends LazyLogging {

  def infiniteCommands(sample: List[Command]): Stream[Command] =
    Stream.continually(sample.toStream).flatten

  def generateCount(sample: List[Command], count: Int): List[Command] =
    infiniteCommands(sample).take(count).toList

  def jvmWarmUp(sample: List[Command]): Unit = {
    logger.info("Begin warm up")
    val commands = util.generateCount(sample, 100000)
    commands.foldLeft(OrderBook.empty) {
      case (book, command) => book.handle(command)._1
    }
    logger.info("End warm up")
  }

  def printSnapshot(s: Snapshot): Unit = println {
    s"""
       |Processed ${s.size} commands
       |99p latency: ${s.get99thPercentile()} ms
       |99.9p latency: ${s.get999thPercentile()} ms
       |Maximum latency: ${s.getMax} ms
        """.stripMargin
  }


}
