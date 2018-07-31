package scala.highperfscala.benchmarks

import java.io.File

import scala.highperfscala.benchmarks.util._
import scala.highperfscala.orderbook.OrderBook

object ThroughputBenchmark {

  def main(args: Array[String]): Unit = {

    val commandSample = DataCodec.read(new File(args(0)))
    val commandCount = args(1).toInt

    jvmWarmUp(commandSample)

    val commands = generateCount(commandSample, commandCount)

    val start = System.currentTimeMillis()
    commands.foldLeft(OrderBook.empty) { case (ob, cmd) => ob.handle(cmd)._1 }
    val end = System.currentTimeMillis()
    val delayInSeconds = (end - start) / 1000.0

    println {
      s"""
         |Processed ${commands.size} commands
         |in $delayInSeconds seconds
         |Throughput: ${commands.size / delayInSeconds} operations/sec"""
        .stripMargin
    }
  }
}
