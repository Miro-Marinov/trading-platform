package parity

class Order(val priceLevel: PriceLevel, val id: Long, var size: Long) {

  def reduceBy(quantity: Long): Unit = {
    size -= quantity
  }

  def resize(newSize: Long): Unit = {
    size = newSize
  }
}
