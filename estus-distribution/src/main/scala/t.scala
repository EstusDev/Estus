package com.estus.distribution

import routine._

import org.apache.commons.math3.distribution.{BetaDistribution, TDistribution}

object t {

  /** *
    *
    * Student-T and Skew Student-T Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dt_internal(q: Double, df: Double, log_p: Boolean, dist: TDistribution): Double = {
    (q, df) match {
      case (a, b) if a.isNaN || b.isNaN => Double.NaN
      case (a, b) if b <= 0.0 =>  throw new IllegalArgumentException
      case (a, b) if a.isInfinite => D__0(log_p)
      case (a, b) if b.isInfinite => norm.dnorm(q, 0.0, 1.0, log_p)
      case _ => if(!log_p) dist.density(q) else dist.logDensity(q)
    }
  }
  def dt(q: Double, df: Double, log_p: Boolean): Double = {
    dt_internal(q, df, log_p, new TDistribution(df))
  }
  def dt(q: List[Double], df: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty || df <= 0) throw new IllegalArgumentException
    val dist = new TDistribution(df)
    q.map(tup => dt_internal(tup, df, log_p, dist))
  }
  private def _dstd(q: Double, mean: Double, sd: Double, nu: Double, log_p: Boolean, dist: TDistribution): Double = {
    val s = math.sqrt(nu/(nu - 2))
    val z = (q - mean)/sd
    var density = dt_internal(z*s, nu, log_p = false, dist) * s / sd
    if(log_p) density = math.log(density)
    density
  }
  private def _dsstd(q: Double, nu: Double, xi: Double, dist: TDistribution): Double = {
    val m1 = 2 * math.sqrt(nu - 2)/(nu - 1)/routine.beta(0.5, 0.5*nu)
    val mu = m1 * (xi - 1/xi)
    val sigma = math.sqrt((1 - m1*m1) * (xi*xi + 1/(xi*xi)) + 2 * m1*m1 - 1)
    val z = q * sigma + mu
    val Xi = if(z > 0.0) xi else if (z == 0.0) 1 else 1/xi
    val g = 2/(xi + 1/xi)
    val density = g * _dstd(z/Xi, 0, 1, nu, log_p = false, dist)
    density * sigma
  }
  private def dsstd_internal(q: Double, mean: Double, sd: Double, nu: Double, xi: Double, log_p: Boolean, dist: TDistribution): Double = {
    val density = _dsstd((q - mean)/sd, nu, xi, dist)/sd
    if(log_p) math.log(density) else density
  }
  def dsstd(q: Double, mean: Double, sd: Double, nu: Double, xi: Double, log_p: Boolean): Double = {
    if(sd < 0.0 || nu <= 2.0 || xi == 0.0 || (mean+sd+nu+xi).isNaN) throw new IllegalArgumentException
    dsstd_internal(q, mean, sd, nu, xi, log_p, new TDistribution(nu))
  }
  def dsstd(q: List[Double], mean: Double, sd: Double, nu: Double, xi: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty || sd < 0.0 || nu <= 2.0 || xi == 0.0 || (mean+sd+nu+xi).isNaN) throw new IllegalArgumentException
    val dist = new TDistribution(nu)
    q.map(tup => dsstd_internal(tup, mean, sd, nu, xi, log_p, dist))
  }



  private def pt_internal(q: Double, df: Double, lower_tail: Boolean, log_p: Boolean, dist: BetaDistribution): Double = {
    (q, df) match {
      case (a, b) if a.isNaN || b.isNaN => Double.NaN
      case (a, b) if b <= 0.0 =>  throw new IllegalArgumentException
      case (a, b) if a.isInfinite => if(a < 0) DT_0(lower_tail, log_p) else DT_1(lower_tail, log_p)
      case (a, b) if b.isInfinite => norm.pnorm(q, 0.0, 1.0, lower_tail, log_p)
      case (a, b) if b > 4e5 =>
        val temp = 1.0/(4.0*df)
        norm.pnorm(q*(1.0 - temp)/math.sqrt(1.0 + 2.0*q*q*temp), 0.0, 1.0, lower_tail, log_p)
      case _ =>
        var temp = 0.0
        if(math.abs(q) > 1e30) {
          temp = -0.5 * df * (2 * math.log(math.abs(q)) - math.log(df))
          temp -= math.log(routine.beta(0.5*df, 0.5)) + math.log(0.5 * df)
          if(!log_p) temp = math.exp(temp)
        } else {
          temp = dist.cumulativeProbability (1.0 / (1 + (q / df) * q))
          if(log_p) temp = math.log (temp)
        }
        val lower_tail_use = if(q <= 0.0) !lower_tail else lower_tail
        if(log_p) {
          if(lower_tail_use) math.log1p(-0.5*math.exp(temp)) else temp - M_LN2
        } else {
          D_Cval(temp/2.0, lower_tail_use)
        }
    }
  }
  def pt(q: Double, df: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    pt_internal(q, df, lower_tail, log_p, new BetaDistribution(0.5*df, 0.5))
  }
  def pt(q: List[Double], df: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty || df <= 0) throw new IllegalArgumentException
    val dist = new BetaDistribution(0.5*df, 0.5)
    q.map(tup => pt_internal(tup, df, lower_tail, log_p, dist))
  }
  private def _pstd(q: Double, mean: Double, sd: Double, nu: Double, dist: BetaDistribution): Double = {
    val s = math.sqrt(nu/(nu - 2))
    val z = (q - mean)/sd
    pt_internal(z * s, df = nu, lower_tail = true, log_p = false, dist)
  }
  private def _psstd(q: Double, nu: Double, xi: Double, dist: BetaDistribution): Double = {
    val m1 = 2 * math.sqrt(nu - 2)/(nu - 1)/routine.beta(0.5, 0.5*nu)
    val mu = m1 * (xi - 1/xi)
    val sigma = math.sqrt((1 - m1*m1) * (xi*xi + 1/(xi*xi)) + 2 * m1*m1 - 1)
    val z = q * sigma + mu
    val Xi = math.pow(xi, sign(z))
    val g = 2/(xi + 1/xi)
    Heaviside(z) - sign(z) * g * Xi * _pstd(-math.abs(z)/Xi, 0, 1, nu = nu, dist)
  }
  private def psstd_internal(q: Double, mean: Double, sd: Double, nu: Double, xi: Double, dist: BetaDistribution): Double = {
    _psstd((q - mean)/sd, nu = nu, xi = xi, dist)
  }
  def psstd(q: Double, mean: Double, sd: Double, nu: Double, xi: Double): Double = {
    if(sd < 0.0 || nu <= 2.0 || xi == 0.0 || (mean+sd+nu+xi).isNaN) throw new IllegalArgumentException
    val dist = new BetaDistribution(0.5*nu, 0.5)
    psstd_internal((q - mean)/sd, mean, sd, nu, xi, dist)
  }
  def psstd(q: List[Double], mean: Double, sd: Double, nu: Double, xi: Double): List[Double] = {
    if(q.isEmpty || sd < 0.0 || nu <= 2.0 || xi == 0.0 || (mean+sd+nu+xi).isNaN) throw new IllegalArgumentException
    val dist = new BetaDistribution(0.5*nu, 0.5)
    q.map(tup => psstd_internal(tup, mean, sd, nu, xi, dist))
  }



  def qt_internal(p: Double, df: Double, lower_tail: Boolean, log_p: Boolean, betadist: BetaDistribution, tdist: TDistribution): Double = {
    if(p.isNaN || df.isNaN) return Double.NaN
    val Q_P01 = Q_P01_boundaries(p, Double.NegativeInfinity, Double.PositiveInfinity, lower_tail, log_p)
    if(!Q_P01.isNaN) return Q_P01
    val eps = 1.0e-12
    var neg = true
    var (a, b, c, d, p_tmp, p_cap, q, x, y) = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    if(df < 1) throw new IllegalArgumentException
    if(df > 1e20) norm.qnorm(p, 0.0, 1.0, lower_tail, log_p)
    p_tmp = D_qIv(p, log_p)
    if((lower_tail && p_tmp > 0.5) || (!lower_tail && p_tmp < 0.5)) {
      neg = false
      p_cap = 2 * D_Cval(p_tmp, lower_tail)
    } else {
      neg = true
      p_cap = 2 * D_Lval(p_tmp, lower_tail)
    }
    if(math.abs(df - 2) < eps) {
      if(p_cap > 0) {
        q = math.sqrt(2 / (p_cap * (2 - p_cap)) - 2)
      } else {
        if(log_p) q = M_SQRT2 * math.exp(-0.5 * D_Lval(p, lower_tail)) else q = Double.PositiveInfinity
      }
    } else if (df < 1 + eps) {
      if(p_cap > 0) {
        q = -math.tan((p_cap + 1) * M_PI_2)
      } else {
        if(log_p) q = M_1_PI * math.exp(-D_Lval(p, lower_tail))
        else q = Double.PositiveInfinity
      }
    } else {
      a = 1 / (df - 0.5)
      b = 48 / (a * a)
      c = ((20700 * a / b - 98) * a - 16) * a + 96.36
      d = ((94.5 / (b + c) - 3) / b + 1) * math.sqrt(a * M_PI_2) * df
      y = if(p_cap > 0 || !log_p) math.pow(d * p_cap, 2 / df) else math.exp(2 / df * (math.log(d) + M_LN2 + D_Lval(p, lower_tail)))
      if((df < 2.1 && p_cap > 0.5) || y > 0.05 + a) {
        x = if(p_cap > 0 || !log_p) norm.qnorm(0.5 * p_cap, 0.0, 1.0, lower_tail = true, log_p = false)
            else norm.qnorm(p,	0.0, 1.0, lower_tail,	log_p = true)
        y = x * x
        if (df < 5) c += 0.3 * (df - 4.5) * (x + 0.6)
        c = (((0.05 * d * x - 5) * x - 7) * x - 2) * x + b + c
        y = (((((0.4 * y + 6.3) * y + 36) * y + 94.5) / c - y - 3) / b + 1) * x
        y = math.expm1(a * y * y)
      } else {
        y = ((1 / (((df + 6) / (df * y) - 0.089 * d - 0.822) * (df + 2) * 3) + 0.5 / (df + 4)) * y - 1) *
            (df + 1) / (df + 2) + 1 / y
      }
      q = math.sqrt(df * y)
      x = (pt_internal(q, df, lower_tail = false, log_p = false, betadist) - p_cap/2) /
          dt_internal(q, df, log_p = false, tdist)
      q += x * (1.0 + x * q * (df + 1) / (2 * (q * q + df)))
    }
    if(neg) -q else q
  }
  def qt(p: Double, df: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    qt_internal(p, df, lower_tail, log_p, new BetaDistribution(df/2.0, 0.5), new TDistribution(df))
  }
  def qt(p: List[Double], df: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty || df <= 0) throw new IllegalArgumentException
    val betadist = new BetaDistribution(df/2.0, 0.5)
    val tdist = new TDistribution(df)
    p.map(tup => qt_internal(tup, df, lower_tail, log_p, betadist, tdist))
  }
  private def _qstd(p: Double, mean: Double, sd: Double, nu: Double, betadist: BetaDistribution, tdist: TDistribution): Double = {
    val s = math.sqrt(nu/(nu - 2))
    qt_internal(p, nu, lower_tail = true, log_p = false, betadist, tdist) * sd/s + mean
  }
  private def _qsstd(p: Double, nu: Double, xi: Double, betadist: BetaDistribution, tdist: TDistribution): Double = {
    val m1 = 2 * math.sqrt(nu - 2)/(nu - 1)/routine.beta(0.5, 0.5*nu)
    val mu = m1 * (xi - 1/xi)
    val sigma = math.sqrt((1 - m1*m1) * (xi*xi + 1/(xi*xi)) + 2 * m1*m1 - 1)
    val g = 2/(xi + 1/xi)
    val sig = sign(p - 0.5)
    val Xi = math.pow(xi, sig)
    val p_tmp = (Heaviside(p - 0.5, a = 0) - sig * p)/(g * Xi)
    (-sig * _qstd(p_tmp, 0, Xi, nu, betadist, tdist) - mu)/sigma
  }
  private def qsstd_internal(p: Double, mean: Double, sd: Double, nu: Double, xi: Double, betadist: BetaDistribution, tdist: TDistribution): Double = {
    _qsstd(p, nu, xi, betadist, tdist) * sd + mean
  }
  def qsstd(p: Double, mean: Double, sd: Double, nu: Double, xi: Double): Double = {
    if(sd < 0.0 || nu <= 2.0 || xi == 0.0 || (mean+sd+nu+xi).isNaN) throw new IllegalArgumentException
    val betadist = new BetaDistribution(0.5*nu, 0.5)
    val tdist = new TDistribution(nu)
    qsstd_internal(p, mean, sd, nu, xi, betadist, tdist)
  }
  def qsstd(p: List[Double], mean: Double, sd: Double, nu: Double, xi: Double): List[Double] = {
    if(p.isEmpty || sd < 0.0 || nu <= 2.0 || xi == 0.0 || (mean+sd+nu+xi).isNaN) throw new IllegalArgumentException
    val betadist = new BetaDistribution(0.5*nu, 0.5)
    val tdist = new TDistribution(nu)
    p.map(tup => qsstd_internal(tup, mean, sd, nu, xi, betadist, tdist))
  }



  def rt(n: Int, df: Double): List[Double] = {
    if(n < 1 || df.isNaN || df <= 0.0) throw new IllegalArgumentException
    if(df.isInfinite) {
      norm.rnorm(n, 0.0, 1.0)
    } else {
      qt(unif.runif(n, 0.0, 1.0), df, lower_tail = true, log_p = false)
    }
  }
  def rsstd(n: Int, mean: Double, sd: Double, nu: Double, xi: Double): List[Double] = {
    if(n < 1 || sd < 0.0 || nu <= 2.0 || xi == 0.0 || (mean+sd+nu+xi).isNaN) throw new IllegalArgumentException
    qsstd(unif.runif(n, 0.0, 1.0), mean, sd, nu, xi)
  }



}
