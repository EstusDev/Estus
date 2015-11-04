package com.estus.distribution

import routine._

object norm {

  /** *
    *
    * Normal Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  def dnorm(q: Double, mean: Double, sd: Double, log_p: Boolean): Double = {
    (q, mean, sd) match {
      case (a, b, c) if (a+b+c).isNaN => Double.NaN
      case (a, b, c) if c.isInfinite => D__0(log_p)
      case (a, b, c) if a.isInfinite && a == b => Double.NaN
      case (a, b, c) if c < 0 => throw new IllegalArgumentException
      case (a, b, c) if c == 0 && a == b => Double.PositiveInfinity
      case (a, b, c) if c == 0 && a != b => D__0(log_p)
      case (a, b, c) if ((a-b)/c).isInfinite => D__0(log_p)
      case _ =>
        val x = math.abs((q - mean)/sd)
        (x, log_p) match {
          case (a, b) if a >= 2 * math.sqrt(DBL_MAX) => D__0(log_p)
          case (a, b) if b => -(M_LN_SQRT_2PI + 0.5 * x * x + math.log(sd))
          case (a, b) if a < 5 => M_1_SQRT_2PI * math.exp(-0.5 * x * x)/sd
          case (a, b) if a > math.sqrt(-2*M_LN2*(DBL_MIN_EXP + 1-DBL_MANT_DIG)) => 0.0
          case _ =>
            val x1 = ldexp(D_forceint(ldexp(x, 16)), -16)
            val x2 = x - x1
            M_1_SQRT_2PI / sd * (math.exp(-0.5 * x1 * x1) * math.exp((-0.5 * x2 - x1) * x2))
        }
    }
  }
  def dnorm(q: List[Double], mean: Double, sd: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    q.map(tup => dnorm(tup, mean, sd, log_p))
  }



  def pnorm(q: Double, mean: Double, sd: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (q, mean, sd, lower_tail, (q-mean)/sd) match {
      case (a, b, c, d, e) if a.isNaN || b.isNaN || c.isNaN || (a.isInfinite && a == b) => Double.NaN
      case (a, b, c, d, e) if c < 0 => throw new IllegalArgumentException
      case (a, b, c, d, e) if (c == 0 || e.isInfinite) && a < b => DT_0(lower_tail, log_p)
      case (a, b, c, d, e) if (c == 0 || e.isInfinite) && a >= b => DT_1(lower_tail, log_p)
      case (a, b, c, d, e) if d => pnorm_both(e, e, 0.0, !lower_tail, log_p).head
      case (a, b, c, d, e) if !d => pnorm_both(e, e, 0.0, !lower_tail, log_p).last
    }
  }
  private def pnorm_both(x: Double, cum: Double, ccum: Double, i_tail: Boolean, log_p: Boolean): List[Double] = {
    val a: List[Double] = List(
      2.2352520354606839287,
      161.02823106855587881,
      1067.6894854603709582,
      18154.981253343561249,
      0.065682337918207449113
    )
    val b: List[Double] = List(
      47.20258190468824187,
      976.09855173777669322,
      10260.932208618978205,
      45507.789335026729956
    )
    val c: List[Double] = List(
      0.39894151208813466764,
      8.8831497943883759412,
      93.506656132177855979,
      597.27027639480026226,
      2494.5375852903726711,
      6848.1904505362823326,
      11602.651437647350124,
      9842.7148383839780218,
      1.0765576773720192317e-8
    )
    val d: List[Double] = List(
      22.266688044328115691,
      235.38790178262499861,
      1519.377599407554805,
      6485.558298266760755,
      18615.571640885098091,
      34900.952721145977266,
      38912.003286093271411,
      19685.429676859990727
    )
    val p: List[Double] = List(
      0.21589853405795699,
      0.1274011611602473639,
      0.022235277870649807,
      0.001421619193227893466,
      2.9112874951168792e-5,
      0.02307344176494017303
    )
    val q: List[Double] = List(
      1.28426009614491121,
      0.468238212480865118,
      0.0659881378689285515,
      0.00378239633202758244,
      7.29751555083966205e-5
    )
    var (cum_ret, ccum_ret) = (cum, ccum)
    val (lower, upper) = (!i_tail, i_tail)
    var (xden, xnum, temp, del, eps, xsq, y, min) = (0.0, 0.0, 0.0, 0.0, 0.5*DBL_EPSILON, 0.0, math.abs(x), DBL_MIN)
    if(y.isNaN) {
      cum_ret = Double.NaN
      ccum_ret = Double.NaN
    } else if (y <= 0.67448975) {
      if(y > eps) {
        xsq = x * x
        xnum = a(4) * xsq
        xden = xsq
        for(i <- 0 to 2) {
          xnum = (xnum + a(i)) * xsq
          xden = (xden + b(i)) * xsq
        }
      } else {
        xnum = 0.0
        xden = 0.0
      }
      temp = x * (xnum + a(3)) / (xden + b(3))
      if(lower) cum_ret = 0.5 + temp
      if(upper) ccum_ret = 0.5 - temp
      if(log_p) {
        if(lower) cum_ret = math.log(cum_ret)
        if(upper) ccum_ret = math.log(ccum_ret)
      }
    } else if (y <= M_SQRT_32) {
      xnum = c(8) * y
      xden = y
      for(i <- 0 to 6) {
        xnum = (xnum + c(i)) * y
        xden = (xden + d(i)) * y
      }
      temp = (xnum + c(7)) / (xden + d(7))
      if(y >= 0) {
        xsq = math.floor(y*16)/16
      } else {
        xsq = math.ceil(y*16)/16
      }
      del = (y - xsq) * (y + xsq)
      if(log_p) {
        cum_ret = (-xsq * xsq * 0.5) + (-del * 0.5) + math.log(temp)
        if((lower && x > 0.0) || (upper && x <= 0.0))
          ccum_ret = math.log1p(-math.exp(-xsq * xsq * 0.5) * math.exp(-del * 0.5) * temp)
      } else {
        cum_ret = math.exp(-xsq * xsq * 0.5) * math.exp(-del * 0.5) * temp
        ccum_ret = 1.0 - cum_ret
      }
      if(x > 0.0) {
        temp = cum_ret
        if(lower) cum_ret = ccum_ret
        ccum_ret = temp
      }
    } else if ((log_p && y < 1e170) ||
      (lower && -37.5193 < x && x < 8.2924) ||
      (upper && -8.2924 < x && x < 37.5193)) {
      xsq = (1.0/x)*(1.0/x)
      xnum = p(5) * xsq
      xden = xsq
      for(i <- 0 to 3) {
        xnum = (xnum + p(i)) * xsq
        xden = (xden + q(i)) * xsq
      }
      temp = xsq * (xnum + p(4)) / (xden + q(4))
      temp = (M_1_SQRT_2PI - temp) / y
      if(y >= 0) {
        xsq = math.floor(y*16)/16
      } else {
        xsq = math.ceil(y*16)/16
      }
      del = (y - xsq) * (y + xsq)
      if(log_p) {
        cum_ret = (-xsq * xsq * 0.5) + (-del * 0.5) + math.log(temp)
        if((lower && x > 0.0) || (upper && x <= 0.0))
          ccum_ret = math.log1p(-math.exp(-xsq * xsq * 0.5) * math.exp(-del * 0.5) * temp)
      } else {
        cum_ret = math.exp(-xsq * xsq * 0.5) * math.exp(-del * 0.5) * temp
        ccum_ret = 1.0 - cum_ret
      }
      if(x > 0.0) {
        temp = cum_ret
        if(lower) cum_ret = ccum_ret
        ccum_ret = temp
      }
    } else {
      if(x > 0.0) {
        cum_ret = D__1(log_p)
        ccum_ret = D__0(log_p)
      } else {
        cum_ret = D__0(log_p)
        ccum_ret = D__1(log_p)
      }
    }
    if(log_p) {
      if(cum_ret > -min) cum_ret = -0.0
      if(ccum_ret > -min) ccum_ret = -0.0
    } else {
      if(cum_ret < min) cum_ret = 0.0
      if(ccum_ret < min) ccum_ret = 0.0
    }
    List(cum_ret, ccum_ret)
  }
  def pnorm(q: List[Double], mean: Double, sd: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    q.map(tup => pnorm(tup, mean, sd, lower_tail, log_p))
  }



  def qnorm(p: Double, mean: Double, sd: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    if(p.isNaN || mean.isNaN || sd.isNaN) throw new IllegalArgumentException
    var (p_tmp, q, r, val_tmp) = (0.0, 0.0, 0.0, 0.0)
    val Q_P01 = Q_P01_boundaries(p, Double.NegativeInfinity, Double.PositiveInfinity, lower_tail, log_p)
    if(!Q_P01.isNaN) return Q_P01
    if(sd < 0) throw new IllegalArgumentException
    if(sd == 0)	return mean
    p_tmp = DT_qIv(p, lower_tail, log_p)
    q = p_tmp - 0.5
    if(math.abs(q) <= .425) {
      r = 0.180625 - q * q
      val_tmp = q * (((((((r * 2509.0809287301226727 +
        33430.575583588128105) * r + 67265.770927008700853) * r +
        45921.953931549871457) * r + 13731.693765509461125) * r +
        1971.5909503065514427) * r + 133.14166789178437745) * r +
        3.387132872796366608) / (((((((r * 5226.495278852854561 +
        28729.085735721942674) * r + 39307.89580009271061) * r +
        21213.794301586595867) * r + 5394.1960214247511077) * r +
        687.1870074920579083) * r + 42.313330701600911252) * r + 1.0)
    } else {
      if (q > 0) r = DT_CIv(p, lower_tail, log_p) else r = p_tmp
      r = math.sqrt(-(if(log_p && ((lower_tail && q <= 0) || (!lower_tail && q > 0))) p else math.log(r)))
      if (r <= 5.0) {
        r += -1.6
        val_tmp = (((((((r * 7.7454501427834140764e-4 +
          .0227238449892691845833) * r + .24178072517745061177) *
          r + 1.27045825245236838258) * r +
          3.64784832476320460504) * r + 5.7694972214606914055) *
          r + 4.6303378461565452959) * r +
          1.42343711074968357734) / (((((((r *
          1.05075007164441684324e-9 + 5.475938084995344946e-4) *
          r + .0151986665636164571966) * r +
          .14810397642748007459) * r + .68976733498510000455) *
          r + 1.6763848301838038494) * r +
          2.05319162663775882187) * r + 1.0)
      } else {
        r += -5.0
        val_tmp = (((((((r * 2.01033439929228813265e-7 +
          2.71155556874348757815e-5) * r +
          .0012426609473880784386) * r + .026532189526576123093) *
          r + .29656057182850489123) * r +
          1.7848265399172913358) * r + 5.4637849111641143699) *
          r + 6.6579046435011037772) / (((((((r *
          2.04426310338993978564e-15 + 1.4215117583164458887e-7)*
          r + 1.8463183175100546818e-5) * r +
          7.868691311456132591e-4) * r + .0148753612908506148525)
          * r + .13692988092273580531) * r +
          .59983220655588793769) * r + 1.0)
      }
      if(q < 0.0) val_tmp = -val_tmp
    }
    mean + sd * val_tmp
  }
  def qnorm(p: List[Double], mean: Double, sd: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    p.map(tup => qnorm(tup, mean, sd, lower_tail, log_p))
  }



  def rnorm(n: Int, mean: Double, sd: Double): List[Double] = {
    if(n < 1 || mean.isNaN || sd.isInfinite || sd < 0.0) throw new IllegalArgumentException
    if(sd == 0.0 || mean.isInfinite) {
      List.fill(n)(mean)
    } else {
      qnorm(unif.runif(n, 0.0, 1.0), mean, sd, lower_tail = true, log_p = false)
    }
  }



}
