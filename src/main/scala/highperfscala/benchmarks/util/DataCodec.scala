package scala.highperfscala.benchmarks.util

import java.io._

import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.ListBuffer

/**
  * This module provides functions to persist to/load from disk a list of
  * commands.
  */
object DataCodec extends LazyLogging {

  def write[T](cs: List[T], output: File): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(output))
    cs.foreach(oos.writeObject)
    oos.close()
  }

  def read[T](input: File): List[T] = {
    val fis = new FileInputStream(input)
    val ois = new ObjectInputStream(fis)
    val commandBuilder = ListBuffer[T]()
    while (fis.available() != 0) {
      commandBuilder.append(ois.readObject().asInstanceOf[T])
    }
    ois.close()
    fis.close()

    commandBuilder.result()
  }

}
