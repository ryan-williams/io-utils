package org.hammerlab.bytes

import caseapp.core.Error.MalformedValue
import caseapp.core.argparser._
import cats.Show
import cats.Show.show
import cats.implicits._

import scala.math._
import scala.util.Try

/**
 * Wrapper for representation of a number of bytes
 */
sealed abstract class Bytes(val value: Int, val scale: Scale)
  extends Product with Serializable {
  def bytes: Long = value * scale.value
  override def toString: String =
    s"$value$scale"
}

sealed abstract class Scale(
  val value: Long,
)(
  implicit
  name: sourcecode.Name
) {
  val max =
    if (value > Int.MaxValue)
      Long.MaxValue / value toInt
    else
      Int.MaxValue

  def apply(bytes: Int): Bytes

  override def toString: String = name.value

  def unapply(bytes: Bytes): Option[Int] = if (bytes.isInstanceOf[B]) Some(bytes.value) else None
}
object Scale {
  implicit def unwrap[B <: Bytes](scale: Scale): Long = scale.value
}

case class   B(override val value: Int) extends    Bytes(value,   B ) ; case object   B extends Scale( 1L                   ) { def apply(bytes: Int) =   B(bytes) }

case class  KB(override val value: Int) extends       SI(value,  KB ) ; case object  KB extends Scale( 1000L                ) { def apply(bytes: Int) =  KB(bytes) }
case class  MB(override val value: Int) extends       SI(value,  MB ) ; case object  MB extends Scale( 1000000L             ) { def apply(bytes: Int) =  MB(bytes) }
case class  GB(override val value: Int) extends       SI(value,  GB ) ; case object  GB extends Scale( 1000000000L          ) { def apply(bytes: Int) =  GB(bytes) }
case class  TB(override val value: Int) extends       SI(value,  TB ) ; case object  TB extends Scale( 1000000000000L       ) { def apply(bytes: Int) =  TB(bytes) }
case class  PB(override val value: Int) extends       SI(value,  PB ) ; case object  PB extends Scale( 1000000000000000L    ) { def apply(bytes: Int) =  PB(bytes) }
case class  EB(override val value: Int) extends       SI(value,  EB ) ; case object  EB extends Scale( 1000000000000000000L ) { def apply(bytes: Int) =  EB(bytes) }

case class KiB(override val value: Int) extends IEEE1541(value, KiB ) ; case object KiB extends Scale( 1L << 10             ) { def apply(bytes: Int) = KiB(bytes) }
case class MiB(override val value: Int) extends IEEE1541(value, MiB ) ; case object MiB extends Scale( 1L << 20             ) { def apply(bytes: Int) = MiB(bytes) }
case class GiB(override val value: Int) extends IEEE1541(value, GiB ) ; case object GiB extends Scale( 1L << 30             ) { def apply(bytes: Int) = GiB(bytes) }
case class TiB(override val value: Int) extends IEEE1541(value, TiB ) ; case object TiB extends Scale( 1L << 40             ) { def apply(bytes: Int) = TiB(bytes) }
case class PiB(override val value: Int) extends IEEE1541(value, PiB ) ; case object PiB extends Scale( 1L << 50             ) { def apply(bytes: Int) = PiB(bytes) }
case class EiB(override val value: Int) extends IEEE1541(value, EiB ) ; case object EiB extends Scale( 1L << 60             ) { def apply(bytes: Int) = EiB(bytes) }

sealed abstract class IEEE1541(override val value: Int, override val scale: Scale) extends Bytes(value, scale)
sealed abstract class       SI(override val value: Int, override val scale: Scale) extends Bytes(value, scale)

object Bytes {

  val bytesStrRegex = """^(\d+)((?:[KMGTPE]i?)?)B?$""".r

  def apply(bytesStr: String): Either[BadBytesString, Bytes] = {
    bytesStr.toUpperCase() match {
      case bytesStrRegex(numStr, suffix) ⇒
        Try {
          val num = numStr.toInt
          Option(suffix).fold { B(num): Bytes } {
            case "K"                    ⇒  KB(num)
            case "Ki"                   ⇒ KiB(num)
            case "M"                    ⇒  MB(num)
            case "Mi"                   ⇒ MiB(num)
            case "G"                    ⇒  GB(num)
            case "Gi"                   ⇒ GiB(num)
            case "Ti" if num <= TiB.max ⇒ TiB(num)
            case "T"  if num <=  TB.max ⇒  TB(num)
            case "Pi" if num <= PiB.max ⇒ PiB(num)
            case "P"  if num <=  PB.max ⇒  PB(num)
            case "Ei" if num <= EiB.max ⇒ EiB(num)
            case "E"  if num <=  EB.max ⇒  EB(num)
            case "Ti" | "T" | "Pi" | "P" | "Ei" | "E" ⇒ throw BytesOverflowException(bytesStr)
            case "" ⇒ B(num)
          }
        }
        .fold(
          _ ⇒ Left(BadBytesString(bytesStr)),
          Right(_)
        )
      case _ ⇒
        Left(BadBytesString(bytesStr))
    }
  }

  implicit def unwrapBytes(bytes: Bytes): Long = bytes.bytes

  object namespaces {
    import org.hammerlab.{ bytes ⇒ b }
    trait ieee1541 {
      type   B = b.  B
      type KiB = b.KiB
      type MiB = b.MiB
      type GiB = b.GiB
      type TiB = b.TiB
      type PiB = b.PiB
      type EiB = b.EiB

      val   B = b.  B
      val KiB = b.KiB
      val MiB = b.MiB
      val GiB = b.GiB
      val TiB = b.TiB
      val PiB = b.PiB
      val EiB = b.EiB
    }

    trait si {
      type  B = b. B
      type KB = b.KB
      type MB = b.MB
      type GB = b.GB
      type TB = b.TB
      type PB = b.PB
      type EB = b.EB

      val  B = b. B
      val KB = b.KB
      val MB = b.MB
      val GB = b.GB
      val TB = b.TB
      val PB = b.PB
      val EB = b.EB
    }

    trait binary {
      type  B = b.  B
      type KB = b.KiB
      type MB = b.MiB
      type GB = b.GiB
      type TB = b.TiB
      type PB = b.PiB
      type EB = b.EiB

      val  B = b.  B
      val KB = b.KiB
      val MB = b.MiB
      val GB = b.GiB
      val TB = b.TiB
      val PB = b.PiB
      val EB = b.EiB
    }
  }

  implicit val bytesParser =
    SimpleArgParser.from[Bytes]("bytes") {
      bytes ⇒
        Bytes(
          bytes
        )
        .leftMap {
          _ ⇒ MalformedValue("bytes", bytes)
        }
    }

  def unapply(bytes: Bytes): Option[Long] = Some(bytes.bytes)

  object format {
    implicit val showLongBytes: Show[ Long] = show(format(_))
    implicit val showBytes    : Show[Bytes] = show(format   )
  }

  def format(bytes: Bytes): String = format(bytes.bytes)
  def format(bytes: Bytes, includeB: Boolean): String = format(bytes.bytes, includeB)
  def format(bytes: Long, includeB: Boolean = false): String = {
    var bs = bytes
    var scale = 0
    var exact = true
    while (bs > (1 << 20)) {
      if (exact && bs % (1 << 10) != 0)
        exact = false
      bs /= (1 << 10)
      scale += 1
    }

    var b = bs.toDouble
    while (b >= (1 << 10)) {
      b /= (1 << 10)
      scale += 1
    }

    val digits = (round(b * 10) / 10).toInt.toString

    val numDigits = digits.length

    val suffix =
      (scale match {
        case 0 ⇒  ""
        case 1 ⇒ "K"
        case 2 ⇒ "M"
        case 3 ⇒ "G"
        case 4 ⇒ "T"
        case 5 ⇒ "P"
        case 6 ⇒ "E"
      }) +
      (if (includeB) "B" else "")

    val fmt =
      if (b < 99.95 && (!exact || floor(b) != ceil(b)))
        "%.1f"
      else
        "%.0f"

    val number = fmt.format(b)

    s"$number$suffix"
  }
}

case class BadBytesString        (str: String) extends IllegalArgumentException(str)
case class BytesOverflowException(str: String) extends IllegalArgumentException(str)
