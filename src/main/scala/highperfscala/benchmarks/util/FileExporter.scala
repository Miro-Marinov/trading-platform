package highperfscala.benchmarks.util

import java.io._
import java.nio.file.{Files, Paths}

import highperfscala.orderbook.{LimitOrder, OrderId}

import scala.highperfscala.orderbook.Commands.{AddLimitOrder, CancelOrder}
import scala.util.Random

/**
  *
  */
object FileExporter {

  val ids = scala.collection.mutable.Set.empty[OrderId]
  val oos = new ObjectOutputStream(new FileOutputStream("orders-data"))
  val r: Random.type = scala.util.Random
  def main(args: Array[String]): Unit = {
//    Files.deleteIfExists(Paths.get("orders-data"))


    (1 to 250000).foreach { _ =>
      LimitOrder.genLimitOrder.sample.foreach { limitOrder =>
        val addLimitOrder = AddLimitOrder(limitOrder)
        oos.writeObject(addLimitOrder)
        ids.add(limitOrder.id)
      }

      r.nextDouble() match {
        case p if p > 0.8 =>
          val cancelOrder = CancelOrder(random(ids))
          oos.writeObject(cancelOrder)
          println(cancelOrder)
        case p if p < 0.1 =>
          val cancelOrder = CancelOrder(OrderId(-1 * r.nextInt(100).longValue()))
          oos.writeObject(cancelOrder)
          println(cancelOrder)
        case _ => Unit
      }
    }
    oos.close()
  }


  def random[T](s: scala.collection.mutable.Set[T]): T = {
    val n = r.nextInt(s.size)
    s.iterator.drop(n).next
  }

}
