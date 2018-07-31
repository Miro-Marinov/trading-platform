package simpleexchange

import java.io.File

import com.codahale.metrics.Snapshot
import com.typesafe.scalalogging.LazyLogging
import org.mpierce.metrics.reservoir.hdrhistogram.HdrHistogramReservoir
import simpleexchange.ThroughputBenchmark.ob

import scala.annotation.tailrec
import scala.highperfscala.benchmarks.util.DataCodec

object FinalLatencyBenchmark extends LazyLogging {

  case class CommandsPerSecond(value: Int) extends AnyVal

  case class BenchmarkIterationCount(value: Int) extends AnyVal

  case class CommandSentTimestamp(value: Long) extends AnyVal

  def runBenchmark(
                    sampleCommands: List[OrderBookRequest],
                    cps: CommandsPerSecond,
                    count: BenchmarkIterationCount): Unit = {
    val totalCommandCount = cps.value * count.value

    jvmWarmUp(sampleCommands)

    @tailrec
    def sendCommands(
                      xs: List[(OrderBookRequest, Int)],
                      ob: OrderBook,
                      testStart: Long,
                      histogram: HdrHistogramReservoir): (OrderBook, HdrHistogramReservoir) =
      xs match {
        case head :: tail =>
          val (command, offsetInMs) = head
          val shouldStart = testStart + offsetInMs

          while (shouldStart > System.currentTimeMillis()) {
            // keep the thread busy while waiting for the next batch to be sent
          }

          ob.processOrderBookRequest(command)
          val operationEnd = System.currentTimeMillis()
          histogram.update(operationEnd - shouldStart)

          sendCommands(tail, ob, testStart, histogram)
        case Nil => (ob, histogram)
      }

    val (_, histogram) = sendCommands(
      generateCount(sampleCommands, totalCommandCount)
        .grouped(cps.value)
        .toList.zipWithIndex
        .flatMap {
          case (secondBatch, sBatchIndex) =>
            val batchOffsetInMs = sBatchIndex * 1000
            val commandIntervalInMs = 1000.0 / cps.value
            secondBatch.zipWithIndex.map {
              case (command, commandIndex) =>
                val commandOffsetInMs =
                  Math.floor(commandIntervalInMs * commandIndex).toInt
                (command, batchOffsetInMs + commandOffsetInMs)
            }
        },
      new OrderBook("symbol"),
      System.currentTimeMillis(),
      new HdrHistogramReservoir())

    printSnapshot(histogram.getSnapshot)
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

  def printSnapshot(s: Snapshot): Unit = println {
    s"""
       |Processed ${s.size} commands
       |99p latency: ${s.get99thPercentile()} ms
       |99.9p latency: ${s.get999thPercentile()} ms
       |Maximum latency: ${s.getMax} ms
        """.stripMargin
  }

  def main(args: Array[String]): Unit = {
    runBenchmark(DataCodec.read(new File(args(0))),
      CommandsPerSecond(args(1).toInt),
      BenchmarkIterationCount(args(2).toInt))
  }
}
