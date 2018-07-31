package simpleexchange

import java.io._
import java.nio.file.{Files, Paths}

import org.scalacheck.{Arbitrary, Gen}

import scala.util.Random

/**
  *
  */
object FileExporter2 {

  //case class Order(timestamp: Long, tradeID: String, symbol: String, var qty: Long, isBuy: Boolean, var price: Option[Double], newOrderEvent: NewOrder)
  implicit lazy val arbOrder: Arbitrary[Order] =
    Arbitrary(
      for {
        timestamp <- Gen.calendar.map(cal => cal.toInstant.getEpochSecond)
        symbol <- Gen.const("symbol")
        id <- Gen.identifier
        qty <- Arbitrary.arbLong.arbitrary
        isBuy <- Arbitrary.arbBool.arbitrary
        price <- Gen.some(Arbitrary.arbDouble.arbitrary)
      } yield {
        Order(timestamp, id, symbol, qty, isBuy, price)
      }
    )

  val placedOrders = scala.collection.mutable.Set.empty[Order]
  val oos = new ObjectOutputStream(new FileOutputStream("simple-orders-data"))
  val r: Random.type = scala.util.Random

  def main(args: Array[String]): Unit = {
//    Files.deleteIfExists(Paths.get("simple-orders-data"))

    (1 to 250000).foreach { _ =>
      arbOrder.arbitrary.sample.foreach { order =>
        oos.writeObject(order)
        placedOrders.add(order)
      }

      r.nextDouble() match {
        case p if p > 0.8 =>
          val placedOrder = random(placedOrders)
          val cancelOrder = Cancel(placedOrder.timestamp + r.nextInt(5000).longValue(), placedOrder)
          oos.writeObject(cancelOrder)
          println(cancelOrder)
        case p if p < 0.1 =>
          Gen.calendar.map(cal => cal.toInstant.getEpochSecond).sample.foreach { timestamp =>
            val placedOrder = Cancel(timestamp, Order(timestamp, "nonexistent", "symbol", 100, r.nextBoolean(), None))
            oos.writeObject(placedOrder)
            println(placedOrder)
          }
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
