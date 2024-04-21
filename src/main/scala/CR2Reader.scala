// source: http://lclevy.free.fr/cr2/#parsing

package editer

import java.io.{FileInputStream, BufferedInputStream}

def mask(b: Byte): Int = b & 0xff

def getNBytes(filename: String, start: Long, n: Int): Array[Byte] = {
  val bis = BufferedInputStream(FileInputStream(filename))
  bis.skip(start)
  val buffer = new Array[Byte](n)
  val bytesRead = bis.read(buffer)
  if (bytesRead < n) {
    println(s"Error reading $n bytes at $start")
  }
  bis.close()
  buffer
}

package object CR2Reader {

  def readBinaryFile(filename: String, verbose: Boolean = false): CR2Image = {
    new CR2Image(filename, verbose)
  } 
}

class CR2Image(filename: String, verbose: Boolean = false) {
  val header = {
    val bis = BufferedInputStream(FileInputStream(filename))
    val headerLength = 16
    val headerBytes = new Array[Byte](16)
    val bytesRead = bis.read(headerBytes)
      if (bytesRead < headerLength) {
        println("Error reading header")
      }
    print("Header: ") 
    headerBytes.foreach(b => print(f"$b%02X "))
    println()
    bis.close()

    new CR2Header(headerBytes)
  }
  println(header)

  // Gather IDF entries
  val idfs = new Array[IFDReader](6)
  var next = header.tiffOffset
  var i = 0
  while (next != 0) {
    val tiffIDF = new IFDReader(next, filename, verbose)
    idfs(i) = tiffIDF
    println()
    println(s"IDF #$i")
    println(tiffIDF)
    next = tiffIDF.nextIFDOffset
    i += 1
  }
  val exifIDF = new IFDReader(idfs(0).exif, filename, verbose)
  println()
  println(s"EXIF IDF")
  println(exifIDF)
  val makernoteIDF = new IFDReader(exifIDF.makernote, filename, verbose)
  println()
  println(s"MAKERNOTE IDF")
  println(makernoteIDF)
  val gpsInfo
}

/*
  CR2 Header has form:
    2 bytes: Byte order
    2 bytes: TIFF magic number
    4 bytes: TIFF offset
    2 bytes: CR2 magic number
    1 byte: CR2 major version
    1 byte: CR2 minor version
    4 bytes: RAW IFD offset
  
*/
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

  override def toString: String = {
    s"-----------------------\n" +
    s"Byte order: $byteOrder\n" +
    f"TIFF offset: 0x$tiffOffset%x\n" +
    s"CR2 major version: $cr2MajorVersion\n" +
    s"CR2 minor version: $cr2MinorVersion\n" +
    f"RAW IFD offset: 0x$rawIfdOffset%x\n" + 
    s"-----------------------"
  }

}