// source: http://lclevy.free.fr/cr2/#parsing

package editer

import java.io.{FileInputStream, BufferedInputStream}

package object CR2Reader {
  def readBinaryFile() = {
    val filename = "C:\\Users\\theoa\\Editing\\edit\\resources\\bird.CR2"
    val bis = BufferedInputStream(FileInputStream(filename))
    println("Reading file: " + filename)
    val headerLength = 16
    val bytes = new Array[Byte](16)
    val bytesRead = bis.read(bytes)
      if (bytesRead < headerLength) {
        println("Error reading header")
      }
    print("Header: ") 
    bytes.foreach(b => print(f"$b%02X "))
    println()
    // Iterator.continually(bis.read())
    //     .takeWhile(_ != -1)
    //     .foreach(b => b)  // do whatever you want with each byte
    bis.close()

    val header = new CR2Header(bytes)
    print(header)

  } 
}

class CR2Header(header: Array[Byte]) {

  val byteOrder: String = {
    val matcher = (mask(header(0)) | (mask(header(1)) << 8))
    matcher match {
      case 0x4d4d => "little_endian"
      case 0x4949 => "big_endian"
      case _ => throw new Exception("Unknown byte order")
    }
  }
  {
    val tiffMagic = (mask(header(2)) | (mask(header(3)) << 8))
    if (tiffMagic != 0x002a) {
      throw new Exception("Invalid TIFF magic number")
    }
  }

  val tiffOffset: Long = {
    mask(header(4)) | (mask(header(5)) << 8) | (mask(header(6)) << 16) | (mask(header(7)) << 24)
  }

  {
    val cr2Magic = (mask(header(8)) | (mask(header(9)) << 8))
    if (cr2Magic != 0x5243) { // Docs say this should be 0x4352 but seemingly its 0x5243
      throw new Exception("Invalid CR2 magic number")
    }
  
  }

  val cr2MajorVersion: Int = mask(header(10))
  val cr2MinorVersion: Int = mask(header(11))
  val rawIfdOffset: Long = mask(header(12)) | (mask(header(13)) << 8) | (mask(header(14)) << 16) | (mask(header(15)) << 24)

  private def mask(b: Byte): Int = b & 0xff

  override def toString: String = {
    s"Byte order: $byteOrder\n" +
    s"TIFF offset: $tiffOffset\n" +
    s"CR2 major version: $cr2MajorVersion\n" +
    s"CR2 minor version: $cr2MinorVersion\n" +
    f"RAW IFD offset: $rawIfdOffset%x\n"
  }

}