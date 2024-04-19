package editer

abstract class Entry(val name: String, val data: Array[Byte]) {
  
}

object Entry {
  def unapply(entry: Entry): Option[(String, Array[Byte])] = {
    Some((entry.name, entry.data))
  }
}

class OneLongEntry(name: String, data: Array[Byte]) extends Entry(name, data) {
  val value: Long = mask(data(0)) | mask(data(1)) << 8 | mask(data(2)) << 16 | mask(data(3)) << 24

  override def toString: String = {
    s"$name: $value"
  }
}

class OneShortEntry(name: String, data: Array[Byte]) extends Entry(name, data) {
  val value: Int = mask(data(0)) | mask(data(1)) << 8

  override def toString: String = {
    s"$name: $value"
  }
}

class ByteArrayEntry(name: String, data: Array[Byte]) extends Entry(name, data) {
  val value: Array[Byte] = data

  override def toString: String = {
    var s = s"$name: "
    for (i <- value.indices) {
      s += f"${value(i)}%x "
    }
    s
  }
}

class StringEntry(name: String, data: Array[Byte]) extends Entry(name, data) {
  val value: String = data.map(_.toChar).mkString

  override def toString: String = {
    s"$name: $value"
  }
}

class RationalEntry(name: String, data: Array[Byte]) extends Entry(name, data) {
  val numerator: Long = mask(data(0)) | mask(data(1)) << 8 | mask(data(2)) << 16 | mask(data(3)) << 24
  val denominator: Long = mask(data(4)) | mask(data(5)) << 8 | mask(data(6)) << 16 | mask(data(7)) << 24

  override def toString: String = {
    s"$name: $numerator/$denominator"
  }
}

class MultiShortEntry(name: String, data: Array[Byte]) extends Entry(name, data) {
  val values: List[Int] = data.grouped(2).map { x =>
    (mask(x(0)) | mask(x(1)) << 8) & 0x0000ffff
  }.toList

  override def toString: String = {
    s"$name: ${values.mkString(", ")}"
  }
}

class MultiLongEntry(name: String, data: Array[Byte]) extends Entry(name, data) {
  val values: List[Long] = data.grouped(4).map { x =>
    (mask(x(0)) | mask(x(1)) << 8 | mask(x(2)) << 16 | mask(x(3)) << 24).toLong & 0x00000000ffffffff
  }.toList

  override def toString: String = {
    s"$name: ${values.mkString(", ")}"
  }
}