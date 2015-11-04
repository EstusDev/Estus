package com.estus.statistics

import scala.collection.mutable.{Map => MutableMap}

object univariate {

  // Calculates Arithmetic Mean of a Vector
  // Single Pass | Time O(n) | Space O(c)
  def mean[T](vec: List[T])(implicit m: Numeric[T]): Double = {
    if(vec.isEmpty) throw new IllegalArgumentException
    var n: Long = 0L
    var sum = 0.0
    for(x <- vec) {
      n += 1
      sum += m.toDouble(x)
    }
    sum/n
  }



  // Calculates Variance of a Vector
  // Single Pass | Time O(n) | Space O(c)
  def variance[T](vec: List[T])(implicit m: Numeric[T]): Double = {
    if(vec.isEmpty) throw new IllegalArgumentException
    var n: Long = 0L
    var (mu, m2, delta) = (0.0, 0.0, 0.0)
    for(x <- vec) {
      n += 1
      delta = m.toDouble(x) - mu
      mu += delta/n
      m2 += delta*(m.toDouble(x) - mu)
    }
    m2/(n-1)
  }



  // Calculates Standard Deviation of a Vector
  // Single Pass | Time O(n) | Space O(c)
  def stddev[T](vec: List[T])(implicit m: Numeric[T]): Double = math.sqrt(variance(vec))



  // Calculates Skewness of a Vector
  // Single Pass | Time O(n) | Space O(c)
  def skewness[T](vec: List[T])(implicit m: Numeric[T]): Double = {
    if(vec.isEmpty) throw new IllegalArgumentException
    var (n, n1): (Long, Long) = (0L, 0L)
    var (mu, m2, m3, delta, delta_n, term1) = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    for(x <- vec) {
      n1 = n
      n += 1
      delta = m.toDouble(x) - mu
      delta_n = delta / n
      term1 = delta * delta_n * n1
      mu += delta_n
      m3 += term1 * delta_n * (n - 2) - 3 * delta_n * m2
      m2 += term1
    }
    math.sqrt(n)*m3/math.pow(m2,1.5)
  }



  // Calculates Kurtosis of a Vector
  // Single Pass | Time O(n) | Space O(c)
  def kurtosis[T](vec: List[T])(implicit m: Numeric[T]): Double = {
    if(vec.isEmpty) throw new IllegalArgumentException
    var (n, n1): (Long, Long) = (0L, 0L)
    var (mu, m2, m3, m4, delta, delta_n, delta_n2, term1) = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    for(x <- vec) {
      n1 = n
      n += 1
      delta = m.toDouble(x) - mu
      delta_n = delta / n
      delta_n2 = delta_n * delta_n
      term1 = delta * delta_n * n1
      mu += delta_n
      m4 += term1 * delta_n2 * (n*n - 3*n + 3) + 6 * delta_n2 * m2 - 4 * delta_n * m3
      m3 += term1 * delta_n * (n - 2) - 3 * delta_n * m2
      m2 += term1
    }
    (n*m4)/(m2*m2)-3
  }



  // Calculates Mode of a Vector
  // Use MutableMap Data Structure | Time O(3n) | Space O(2n)
  def mode[T](vec: List[T])(implicit m: Numeric[T]): List[T] = {
    if(vec.isEmpty) throw new IllegalArgumentException
    var n: Long = 0L
    val map = MutableMap.empty[T, Int]
    vec.foreach({t => map += t -> (map.getOrElse(t, 1) + 1); n += 1})
    val max = map.values.max
    map.filter(tup => tup._2 == max).map(tup => tup._1).toList
  }



  // Calculates Median of a Vector
  // Use Sorting Algorithm | Time O(n log n) | Space O(2n)
  def median[T](vec: List[T])(implicit m: Numeric[T]): Double = {
    if(vec.isEmpty) throw new IllegalArgumentException
    val n = vec.length
    val (lower, upper) = vec.sorted.splitAt(n/2)
    n % 2 match {
      case 0 => (m.toDouble(lower.last) + m.toDouble(upper.head))/2
      case 1 => m.toDouble(upper.head)
    }
  }



  // Calculates Quantile of a Vector
  // Use Sorting Algorithm | Time O(n log n) | Space O(2n)
  def quantile[T](vec: List[T], q: Double)(implicit m: Numeric[T]): T = {
    if(vec.isEmpty || q < 0.0 || q > 1.0) throw new IllegalArgumentException
    val n = vec.length
    vec.sorted.apply(if(math.ceil(q*n).toInt > n-1) n-1 else math.ceil(q*n).toInt)
  }
  // Use Sorting Algorithm | Time O(n log n) | Space O(2n)
  def quantile[T](vec: List[T], q: List[Double])(implicit m: Numeric[T]): List[T] = {
    if(vec.isEmpty || q.isEmpty || q.min < 0.0 || q.max > 1.0) throw new IllegalArgumentException
    val n = vec.length
    val vecsorted = vec.sorted
    q.map(t => if(math.ceil(t*n).toInt > n-1) vecsorted.apply(n-1) else vecsorted.apply(math.ceil(t*n).toInt))
  }



  // Calculate Range of a Vector
  // Single Pass | Time O(n) | Space O(c)
  def range[T](vec: List[T])(implicit m: Numeric[T]): List[T] = {
    if(vec.isEmpty) throw new IllegalArgumentException
    var (min, max) = (vec.head, vec.head)
    vec.foreach({t =>
      if(m.toDouble(t) < m.toDouble(min)) min = t
      if(m.toDouble(t) > m.toDouble(max)) max = t
    })
    List(min, max)
  }



  // Calculate Ranking of a Vector - "1224" ties logic
  // Use TreeMap (Red-Black Tree?) Data Structure | Time O(n log n) | Space O(3n)
  def rank[T](vec: List[T])(implicit m: Numeric[T]): List[Int] = {
    if(vec.isEmpty) throw new IllegalArgumentException
    var (n, i): (Int, Int) = (0, 0)
    // put everyting in a TreeMap
    val map = new java.util.TreeMap[T, List[Int]]()
    vec.foreach({tup =>
      if(map.containsKey(tup)) map.put(tup, map.get(tup).:+(n)) else map.put(tup, List(n))
      n += 1
    })
    // group each key in the map
    val order = new Array[Int](n)
    n = 0
    var itr = map.values().iterator()
    while(itr.hasNext){
      val tmp = itr.next
      tmp.foreach({tup => order(i) = n; i += 1})
      n += tmp.length
    }
    // extract the rankings
    val rankvec = new Array[Int](n)
    n = 0
    itr = map.values().iterator()
    while(itr.hasNext){
      val tmp = itr.next
      tmp.foreach({tup => rankvec(tup) = order(n); n += 1})
    }
    rankvec.toList
  }



  // Count Zeros, Double.NaN or x
  // Single Pass | Time O(n) | Space O(c)
  def countZeros[T](vec: List[T])(implicit m: Numeric[T]): Int = vec.count(_ == 0)
  def countNAs(vec: List[Double]): Int = vec.count(_.isNaN)
  def countX[T](vec: List[T], x: T)(implicit m: Numeric[T]): Int = vec.count(_ == x)



}

