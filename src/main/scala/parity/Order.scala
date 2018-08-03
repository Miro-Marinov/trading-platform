package parity

class Order(val level: PriceLevel, val id: Long, var size: Long, var remainingQuantity: Long) {

  def this(level: PriceLevel, id: Long, size: Long) = {
    this(level, id, size, 0)
  }

  def reduce(quantity: Long): Unit = {
    remainingQuantity -= quantity
  }

  def resize(size: Long): Unit = {
    remainingQuantity = size
  }
}
