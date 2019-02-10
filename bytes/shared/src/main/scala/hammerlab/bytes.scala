package hammerlab

import org.hammerlab.bytes.Bytes.namespaces
import org.hammerlab.bytes.syntax
import org.hammerlab.{ bytes ⇒ b }

/**
 * Import-friendly API, exposing interesting bits via:
 *
 * {{{
 * import hammerlab.bytes._
 * }}}
 */
object bytes
  extends syntax
     with namespaces.binary {

   trait syntax extends b.syntax
  object syntax extends   syntax

   trait binary extends Bytes.namespaces.binary
  object binary extends binary

  type Bytes = org.hammerlab.bytes.Bytes
  val  Bytes = org.hammerlab.bytes.Bytes

  val format = org.hammerlab.bytes.Bytes.format
}
