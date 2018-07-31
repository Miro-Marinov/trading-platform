package simpleexchange

import java.io.File

import com.typesafe.scalalogging.LazyLogging

import scala.highperfscala.benchmarks.util._

object ThroughputBenchmark extends LazyLogging {

  val ob = new OrderBook("symbol")

  def main(args: Array[String]): Unit = {

    val commandSample = DataCodec.read[OrderBookRequest](new File(args(0)))
    val commandCount = args(1).toInt

    jvmWarmUp(commandSample)

    val commands = generateCount(commandSample, commandCount)

    val start = System.currentTimeMillis()
    commands.foreach(c => ob.processOrderBookRequest(c))
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


  def infiniteCommands[T](sample: List[T]): Stream[T] =
    Stream.continually(sample.toStream).flatten

  def generateCount[T](sample: List[T], count: Int): List[T] =
    infiniteCommands(sample).take(count).toList

  def jvmWarmUp(sample: List[OrderBookRequest]): Unit = {
    logger.debug("Begin warm up")
    val commands = generateCount[OrderBookRequest](sample, 100000)

    commands.foreach { c => ob.processOrderBookRequest(c) }
    logger.debug("End warm up")
  }
}
