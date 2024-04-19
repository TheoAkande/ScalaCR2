package editer

import java.io.{FileInputStream, BufferedInputStream}

class InvalidEntryException(message: String) extends Exception(message) {
  def this() = this("Invalid Entry Data")
}

class IFDReader(val startOffset: Long, val filename: String, verbose: Boolean = false) {

  // Find number of entries
  val numEntries = {
    val buffer = getNBytes(filename, startOffset, 2)
    (mask(buffer(0)) | (mask(buffer(1)) << 8))
  } 

  // data segment start
  val dataStart = startOffset + 6 + 12 * numEntries

  // Setup array of entries
  val entries = new Array[IDFEntry](numEntries)
  setupEntries()

  // Find next IFD offset
  val nextIFDOffset = {
    val buffer = getNBytes(filename, startOffset + 2 + 12 * numEntries, 4)
    mask(buffer(0)) | (mask(buffer(1)) << 8) | (mask(buffer(2)) << 16) | (mask(buffer(3)) << 24)
  }

  // Setup each entry
  private def setupEntries(): Unit = {
    val bis = BufferedInputStream(FileInputStream(filename))
    bis.skip(startOffset + 2)
    val buffer = new Array[Byte](12)
    for (i <- 0 until numEntries) {
      val bytesRead = bis.read(buffer)
      if (bytesRead < 12) {
        println(s"Error reading entry $i")
      } else {
        entries(i) = IDFEntry(buffer, filename, verbose)
      }
    }
  }

  override def toString: String = {
    s"-----------------------\n" +
    s"Number of entries: $numEntries\n" +
    f"Next IFD offset: 0x$nextIFDOffset%x\n" +
    "Entries:\n" +
    entries.filter(p => verbose || p.entry != null).mkString("\n") + "\n" +
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
case class IDFEntry(val idata: Array[Byte], filename: String, verbose: Boolean) {

  // Map tag types to byte lengths
  // 0 -> variable length
  val tagTypeMap: Map[Int, Int] = Map[Int, Int](
    1 -> 1,  // unsigned char
    2 -> 1,  // string
    3 -> 2,  // unsigned short
    4 -> 4,  // unsigned long
    5 -> 8,  // unsigned rationnal 
    6 -> 1,  // signed char
    7 -> 1,  // byte sequence
    8 -> 2,  // signed short
    9 -> 4,  // signed long
    10 -> 8, // signed rationnal 
    11 -> 4, // float, 4 bytes
    12 -> 8 // float, 8 bytes
  )

  // Map tag numbers to tag names and number of type length
  // 0 -> variable length
  val tagMap: Map[Int, (String, Int)] = Map[Int, (String, Int)](
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

  val tag = mask(idata(0)) | (mask(idata(1)) << 8)
  val tagType = mask(idata(2)) | (mask(idata(3)) << 8)
  val numValues = mask(idata(4)) | (mask(idata(5)) << 8) | (mask(idata(6)) << 16) | (mask(idata(7)) << 24)
  val valueOffset = mask(idata(8)) | (mask(idata(9)) << 8) | (mask(idata(10)) << 16) | (mask(idata(11)) << 24)
  val dataSize = numValues * tagTypeByteSize
  var imm = true
  val entry = try {
    if (dataSize <= 4 && dataSize > 0) {
      val darr = new Array[Byte](dataSize)
      for (i <- 0 until dataSize) {
        darr(i) = mask(idata(8 + i)).toByte
      }
      getEntry(darr)
    } else {
      imm = false
      getEntry(getNBytes(filename, valueOffset, dataSize))
    }
  } catch {
    case e: InvalidEntryException => {
      println(e)
      null
    }
  }

  private def getEntry(data: Array[Byte]): Entry = {

    // Create Entry object
    (tagMapEntry, tagType) match {
      case (None, _) => throw new InvalidEntryException(f"Unknown tag 0x${tag}%x")
      case (Some((name, 1)), 3) => new OneShortEntry(name, data)
      case (Some((name, length)), 2) => new StringEntry(name, data)
      case (Some((name, 1)), 4) => new OneLongEntry(name, data)
      case (Some((name, 1)), 5) => new RationalEntry(name, data)
      case (Some((name, _)), 7) => new ByteArrayEntry(name, data)
      case (Some((name, 1)), 8) => new OneShortEntry(name, data)
      case (Some((name, 1)), 9) => new OneLongEntry(name, data)
      case (Some((name, 1)), 10) => new RationalEntry(name, data)

      case (Some((name, _)), 3) => new MultiShortEntry(name, data)
      case (Some((name, _)), 4) => new MultiLongEntry(name, data)
      case (Some((name, _)), 8) => new MultiShortEntry(name, data)
      case (Some((name, _)), 9) => new MultiLongEntry(name, data)
      case (Some((name, smth)), selse) => {
        println(s"Name: $name, n: $smth, type: $selse") 
        throw new InvalidEntryException("Unknown tag type")
      }
      // TODO: cases 1, 6, 11, 12
    } 

  }

  override def toString: String = {
    if (verbose) {
      s"-----------------------\n" +
      f"Tag: 0x$tag%x\n" +
      s"Tag type: $tagType\n" +
      s"Number of values: $numValues\n" + 
      s"Data size: $dataSize\n" +
      (if (!imm) then f"Value offset: 0x$valueOffset%x\n" else "") +
      entry + "\n" +
      s"-----------------------"
    } else {
      s"$entry"
    }
  }

  def tagTypeByteSize: Int = {
    tagTypeMap.get(tagType) match {
      case Some(size) => size
      case None => throw new InvalidEntryException(f"Unknown tag type $tagType")
    }
  }

  def tagMapEntry: Option[(String, Int)] = {
    tagMap.get(tag)
  }

  def tagName: Option[String] = {
    tagMap.get(tag) match {
      case Some((name, _)) => Some(name) 
      case None => None
    }
  }
  def tagLength: Option[Int] = {
    tagMap.get(tag) match {
      case Some((_, length)) => Some(length)
      case None => None
    }
  }
}

object IDFEntry {
  def apply(entry: Array[Byte], filename: String, verbose: Boolean): IDFEntry = new IDFEntry(entry, filename, verbose)

  def unapply(entry: IDFEntry): Option[(Int, Long, Long)] = {
    Some((entry.tag, entry.dataSize, entry.valueOffset))
  }
}

