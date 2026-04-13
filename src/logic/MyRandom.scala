case class MyRandom(seed: Long) {
  def nextInt(n: Int): (Int, MyRandom) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val result = (nextSeed >>> 16).toInt % n
    ((if (result < 0) then -result else result), MyRandom(nextSeed))
  }
}
