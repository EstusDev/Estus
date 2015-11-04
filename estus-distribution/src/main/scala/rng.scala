package com.estus.distribution

object rng {

  private val N = 624
  private val M = 397
  private val MatrixA = 0x9908b0dfL
  private val UpperMask = 0x80000000L
  private val LowerMask = 0x7fffffffL
  private val mt = new Array[Long](N)
  private var mti = N + 1
  private val seed = System.currentTimeMillis

  mt(0) = seed
  for (i <- 1 until N) mt(i) = (1812433253L * (mt(i - 1) ^ (mt(i - 1) >>> 30)) + i) & 0xffffffffL

  // Sets Seed
  def setSeed(seed: Long) = {
    mt(0) = seed
    for (i <- 1 until N) mt(i) = (1812433253L * (mt(i - 1) ^ (mt(i - 1) >>> 30)) + i) & 0xffffffffL
  }

  // Generates the next random integer in the sequence
  def nextInt(): Int = {
    var y = 0L
    if(mti >= N) {
      val mag01 = Array(0L, MatrixA)
      var kk = 0
      while (kk < N - M) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)
        kk += 1
      }
      while (kk < N - 1) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + (M - N)) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)
        kk += 1
      }
      y = (mt(N - 1) & UpperMask) | (mt(0) & LowerMask)
      mt(N - 1) = mt(M - 1) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)
      mti = 0
    }
    y = mt(mti); mti += 1
    y ^= y >>> 11
    y ^= (y << 7) & 0x9d2c5680L
    y ^= (y << 15) & 0xefc60000L
    y ^= (y >>> 18)
    y.toInt
  }

  // Generates a random integer in the interval [0, limit)
  def nextInt(limit: Int): Int = {
    // Find shift distance
    val lim = limit.toLong & 0xffffffffL
    var n = -1; var bit = 1L << 32
    while (bit > lim) { n += 1; bit >>>= 1 }
    // Generate integer, take most significant bits; reject while outside interval
    var r = (nextInt().toLong & 0xffffffffL) >>> n
    while (r >= lim) { r = (nextInt().toLong & 0xffffffffL) >>> n }
    r.toInt
  }

  // Generates a random Double in the interval [0, 1)
  def nextDouble(): Double = {
    val a: Long = (nextInt().toLong & 0xffffffffL) >>> 5
    val b: Long = (nextInt().toLong & 0xffffffffL) >>> 6
    (a * 67108864.0 + b) / 9007199254740992.0
  }

}