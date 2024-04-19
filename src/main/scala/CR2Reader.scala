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

  def readBinaryFile(filename: String): CR2Image = {
    new CR2Image(filename)
  } 
}

class CR2Image(filename: String) {
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

  val tiffIDF = new IFDReader(header.tiffOffset, filename)
  println(tiffIDF)

  val rawIDF = new IFDReader(header.rawIfdOffset, filename)
  println(rawIDF)
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
    s"TIFF offset: $tiffOffset\n" +
    s"CR2 major version: $cr2MajorVersion\n" +
    s"CR2 minor version: $cr2MinorVersion\n" +
    f"RAW IFD offset: 0x$rawIfdOffset%x\n" + 
    s"-----------------------"
  }

}

class IFDReader(val startOffset: Long, val filename: String) {
  val numEntries = {
    val buffer = getNBytes(filename, startOffset, 2)
    (mask(buffer(0)) | (mask(buffer(1)) << 8))
  } 
  val entries = new Array[IDFEntry](numEntries)
  setupEntries()
  val nextIFDOffset = {
    val buffer = getNBytes(filename, startOffset + 2 + 12 * numEntries, 4)
    mask(buffer(0)) | (mask(buffer(1)) << 8) | (mask(buffer(2)) << 16) | (mask(buffer(3)) << 24)
  }
  
  private def setupEntries(): Unit = {
    val bis = BufferedInputStream(FileInputStream(filename))
    bis.skip(startOffset + 2)
    val buffer = new Array[Byte](12)
    for (i <- 0 until numEntries) {
      val bytesRead = bis.read(buffer)
      if (bytesRead < 12) {
        println(s"Error reading entry $i")
      } else {
        entries(i) = new IDFEntry(buffer)
      }
    }
  }

  override def toString: String = {
    s"-----------------------\n" +
    s"Number of entries: $numEntries\n" +
    f"Next IFD offset: 0x$nextIFDOffset%x\n" +
    s"-----------------------"
  }
}

/*
 IDF Entries have form:
  2 bytes: Tag
  2 bytes: Tag type
  4 bytes: Number of values
  4 bytes: Value offset

  Tag types:
    1 = unsigned char
    2 = string (with an ending zero)
    3 = unsigned short (2 bytes)
    4 = unsigned long (4 bytes)
    5 = unsigned rationnal (2 unsigned long)
    6 = signed char
    7 = byte sequence
    8 = signed short
    9 = signed long
    10 = signed rationnal (2 signed long)
    11 = float, 4 bytes, IEEE format
    12 = float, 8 bytes, IEEE format
*/
class IDFEntry(val entry: Array[Byte]) {
  val tag = mask(entry(0)) | (mask(entry(1)) << 8)
  val tagType = mask(entry(2)) | (mask(entry(3)) << 8)
  val numValues = mask(entry(4)) | (mask(entry(5)) << 8) | (mask(entry(6)) << 16) | (mask(entry(7)) << 24)
  val valueOffset = mask(entry(8)) | (mask(entry(9)) << 8) | (mask(entry(10)) << 16) | (mask(entry(11)) << 24)

  override def toString: String = {
    s"-----------------------\n" +
    f"Tag: 0x$tag%x\n" +
    s"Tag type: $tagType\n" +
    s"Number of values: $numValues\n" +
    f"Value offset: 0x$valueOffset%x\n" +
    s"-----------------------"
  }

  
  // Map tag types to byte lengths
  // 0 -> variable length
  val tagTypeMap: Map[Int, String] = Map(
    1 -> 1  // unsigned char
    2 -> 0  // string
    3 -> 2  // unsigned short
    4 -> 4  // unsigned long
    5 -> 8  // unsigned rationnal 
    6 -> 1  // signed char
    7 -> 0  // byte sequence
    8 -> 2  // signed short
    9 -> 4  // signed long
    10 -> 8 // signed rationnal 
    11 -> 4 // float, 4 bytes
    12 -> 8 // float, 8 bytes
  )

  // Map tag numbers to tag names and number of type length
  // 0 -> variable length
  val tagMap: Map[Int, (String, Int)] = Map(
    // TIFF tags
    0x0100 -> ("Image Width", 1),
    0x0101 -> ("Image Length", 1),
    0x0102 -> ("Bits Per Sample", 3),
    0x0103 -> ("Compression", 1),
    0x0106 -> ("Photometric Interpretation", 1),
    0x010f -> ("Make", 0),
    0x0110 -> ("Model", 0),
    0x0111 -> ("Strip Offsets", 1),
    0x0112 -> ("Orientation", 1),     // 1 -> (0,0) is top left
    0x0115 -> ("Samples Per Pixel", 1),
    0x0116 -> ("Rows Per Strip", 1),
    0x0117 -> ("Strip Byte Counts", 1),
    0x011a -> ("X Resolution", 1),
    0x011b -> ("Y Resolution", 1),
    0x011c -> ("Planar Configuration", 1),
    0x0128 -> ("Resolution Unit", 1), // 2 -> pixels per inche
    0x0131 -> ("Software", 0),
    0x0132 -> ("DateTime", 20),
    0x013b -> ("Artist", 0),
    0x8769 -> ("EXIF IFD", 1),
    0x8825 -> ("GPS IFD", 1),
    
    // EXIF tags
    0x829a -> ("Exposure Time", 1),
    0x829d -> ("F-Number", 1),
    0x8822 -> ("Exposure Program", 1),
    0x8827 -> ("ISO Speed Ratings", 1),
    0x927c -> ("Maker Note", 1),

    // Makernote tags
    0x0001 -> ("Camera Settings", 47),
    0x0002 -> ("Focal Length", 4),
    0x0006 -> ("Image Type", 15),
    0x0097 -> ("Dust Delete Data", 1024),
    0x00e0 -> ("Sensor Info", 17),
    0x4001 -> ("Colour Balance", 0),
    0x4013 -> ("AF Micro Adj", 5),
    0x4015 -> ("Vignette Correction", 0), // 116, but 66 for G11 and S90
    0x0083 -> ("Original Decision Data", 1) // seemingly

  ) 
  // Take a look at docs for tag 0x0083
  // Docs also mention Dust Delete Data - cba rn

  /*
   Sensor Info data:
    Offset 1: Sensor Width (in mm) 1 short 
    Offset 2: Sensor Height (in mm) 1 short
    Offset 5: Sensor Left Border (in mm) 1 short
    Offset 6: Sensor Top Border (in mm) 1 short
    Offset 7: Sensor Right Border (in mm) 1 short
    Offset 8: Sensor Bottom Border (in mm) 1 short
    Offset 9: Black Mask Left Border 1 short
    Offset a: Black Mask Top Border 1 short
    Offset b: Black Mask Right Border 1 short
    Offset c: Black Mask Bottom Border 1 short

   Colour Balance data:
    1227 (bytes?) for 450D,
    For EOS Cameras:
      Offset 0x003f -> RGGB level as shot - 4 shorts 

   Vignette Correction data:
    Offset 0x0: tag version - 1 short, 0x1000, but 0x1080 for G11 and S90
    Offset 0x2: tag length - 1 short (116 or 66)
    Offset 0x4: Correction Applied - 1 short (1=yes, 0=no)
    Offset 0x6: Auto Lighting Optimizer - 1 short (1=yes, 0=no) THIS IS WHAT COPILOT SAID, DOC DOESNT KNOW
    Offset 0xc: DPP Correction value - 1 short
    Offset 0x14: flags? - 1 short (reference doc)
    Offset 0x16: Image Width - 1 short (reference doc)
    Offset 0x18: Image Height - 1 short (reference doc)
    Offset 0x28: Lens data - 10 bytes. (reference doc for mapping)
    
    EF 70-200mm f4L IS USM = 38 02 9D 09 92 0C 0C 0E FF 0F
    EF 50mm f1.8 II        = LETS FIND OUT WOO
  */
  
}