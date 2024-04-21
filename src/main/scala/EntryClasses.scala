package editer

abstract class Entry[T](val name: String, val data: Array[Byte]) {
  def getShort: Int = mask(data(0)) | mask(data(1)) << 8
  def getLong:  Long  = mask(data(0)) | mask(data(1)) << 8 | mask(data(2)) << 16 | mask(data(3)) << 24
  def getBytes: Array[Byte] = data
  def getString: String = data.map(_.toChar).mkString
  def getShorts: List[Int] = data.grouped(2).map { x => (mask(x(0)) | mask(x(1)) << 8) & 0x0000ffff }.toList
  def getLongs: List[Long] = data.grouped(4).map { x =>
    (mask(x(0)) | mask(x(1)) << 8 | mask(x(2)) << 16 | mask(x(3)) << 24).toLong & 0x00000000ffffffff
  }.toList
  def getRational: (Long, Long) = (
    mask(data(0)) | mask(data(1)) << 8 | mask(data(2)) << 16 | mask(data(3)) << 24,
    mask(data(4)) | mask(data(5)) << 8 | mask(data(6)) << 16 | mask(data(7)) << 24
  )
}

// object Entry {
//   def unapply(entry: Entry): Option[(String, Array[Byte])] = {
//     Some((entry.name, entry.data))
//   }
// }

class OneLongEntry(name: String, data: Array[Byte]) extends Entry[Long](name, data) {
  lazy val value: Long = getLong

  override def toString: String = {
    s"$name: $value"
  }
}

class OneShortEntry(name: String, data: Array[Byte]) extends Entry[Short](name, data) {
  lazy val value: Int = getShort

  override def toString: String = {
    s"$name: $value"
  }
}

class ByteArrayEntry(name: String, data: Array[Byte]) extends Entry[Array[Byte]](name, data) {
  val display_max = 16

  lazy val value: Array[Byte] = data

  override def toString: String = {
    var s = s"$name: ["
    if (value.length <= display_max) {
      for (i <- value.indices) {
        s += f"0x${value(i)}%x "
      }
    } else {
      for (i <- 0 to 4) {
        s += f"0x${value(i)}%x "
      }
      s += ". . . "
      for (i <- 0 to 4) {
        s += f"0x${value(value.length - 1 - i)}%x "
      }
    }
    s + "]"
  }
}

class StringEntry(name: String, data: Array[Byte]) extends Entry[String](name, data) {
  lazy val value: String = getString

  override def toString: String = {
    s"$name: $value"
  }
}

class RationalEntry(name: String, data: Array[Byte]) extends Entry[(Long, Long)](name, data) {
  lazy val value = getRational

  override def toString: String = {
    s"$name: ${value(0)}/${value(1)}"
  }
}

class MultiShortEntry(name: String, data: Array[Byte]) extends Entry[List[Short]](name, data) {
  lazy val value = getShorts

  override def toString: String = {
    s"$name: [${value.mkString(", ")}]"
  }
}

class MultiLongEntry(name: String, data: Array[Byte]) extends Entry[List[Long]](name, data) {
  val value = getLongs

  override def toString: String = {
    s"$name: [${value.mkString(", ")}]"
  }
}