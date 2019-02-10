package org.hammerlab

package object bytes {
  /**
   * Enables syntax like `32.MB` or `32 MB` for constructing [[Bytes]]
   */
  implicit class BytesWrapper(val value: Int)
    extends AnyVal {
    def  B: Bytes = new  B(value)
    def KB: Bytes = new KiB(value)
    def MB: Bytes = new MiB(value)
    def GB: Bytes = new GiB(value)
    def TB: Bytes = new TiB(value)
    def PB: Bytes = new PiB(value)
    def EB: Bytes = new EiB(value)
  }
}
