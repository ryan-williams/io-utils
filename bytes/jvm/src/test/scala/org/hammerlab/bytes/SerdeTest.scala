package org.hammerlab.bytes

import java.io._

import org.hammerlab.Suite
import org.hammerlab.test.Cmp
import shapeless.Generic

class SerdeTest
  extends Suite {
  test("serde") {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(2.MB)
    oos.close()

    val bais = new ByteArrayInputStream(baos.toByteArray)
    val ois = new ObjectInputStream(bais)
    !![Generic[Scale]]
    !![Generic[  B]]
    !![Generic[ MB]]
    !![Generic[MiB]]
    !![Generic[IEEE1541]]
    !![Generic[SI]]
    !![Generic[Bytes]]
//    !![Cmp[MB]]
//    !![Cmp[MiB]]
//    !![Cmp[IEEE1541]]
//    !![Cmp[SI]]
//    !![Cmp[Bytes]]
    ==(ois.readObject().asInstanceOf[Bytes], 2.MB)
  }
}
