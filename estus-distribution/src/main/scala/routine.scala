package com.estus.distribution

import org.apache.commons.math3.special.Gamma

object routine {
  
  /* Radix of exponent representation */
  val FLT_RADIX = 2
  /* Number of base-FLT_RADIX digits in the significand of a float */
  val FLT_MANT_DIG = 24
  /* Number of decimal digits of precision in a float */
  val FLT_DIG = 6
  /* Addition rounds to 0: zero, 1: nearest, 2: +inf, 3: -inf, -1: unknown */
  val FLT_ROUNDS = 1
  /* Difference between 1.0 and the minimum float greater than 1.0 */
  val FLT_EPSILON = 1.1920929e-07F
  /* Minimum int x such that FLT_RADIX**(x-1) is a normalised float */
  val FLT_MIN_EXP = -125
  /* Minimum normalised float */
  val FLT_MIN = 1.17549435e-38F
  /* Minimum int x such that 10**x is a normalised float */
  val FLT_MIN_10_EXP = -37
  /* Maximum int x such that FLT_RADIX**(x-1) is a representable float */
  val FLT_MAX_EXP = 128
  /* Maximum float */
  val FLT_MAX = 3.40282347e+38
  /* Maximum int x such that 10**x is a representable float */
  val FLT_MAX_10_EXP = 38

  /* Number of base-FLT_RADIX digits in the significand of a double */
  val DBL_MANT_DIG = 53
  /* Number of decimal digits of precision in a double */
  val DBL_DIG = 15
  /* Difference between 1.0 and the minimum double greater than 1.0 */
  val DBL_EPSILON = 2.2204460492503131e-16
  /* Minimum int x such that FLT_RADIX**(x-1) is a normalised double */
  val DBL_MIN_EXP = -1021
  /* Minimum normalised double */
  val DBL_MIN = 2.2250738585072014e-308
  /* Minimum int x such that 10**x is a normalised double */
  val DBL_MIN_10_EXP = -307
  /* Maximum int x such that FLT_RADIX**(x-1) is a representable double */
  val DBL_MAX_EXP = 1024
  /* Maximum double */
  val DBL_MAX = 1.7976931348623157e+308
  /* Maximum int x such that 10**x is a representable double */
  val DBL_MAX_10_EXP = 308

  /* Number of base-FLT_RADIX digits in the significand of a long double */
  /* By default, we set the precision mode on x86 processors to double */
  val LDBL_MANT_DIG = DBL_MANT_DIG
  /* Number of decimal digits of precision in a long double */
  /* By default, we set the precision mode on x86 processors to double */
  val LDBL_DIG = DBL_DIG
  
  /* Difference between 1.0 and the minimum long double greater than 1.0 */
  val LDBL_EPSILON = DBL_EPSILON
  /* Minimum int x such that FLT_RADIX**(x-1) is a normalised long double */
  val LDBL_MIN_EXP = DBL_MIN_EXP
  /* Minimum normalised long double */
  val LDBL_MIN = DBL_MIN
  /* Minimum int x such that 10**x is a normalised long double */
  val LDBL_MIN_10_EXP = DBL_MIN_10_EXP
  /* Maximum int x such that FLT_RADIX**(x-1) is a representable long double */
  val LDBL_MAX_EXP = DBL_MAX_EXP
  /* Maximum long double */
  val LDBL_MAX = DBL_MAX
  /* Maximum int x such that 10**x is a representable long double */
  val LDBL_MAX_10_EXP = DBL_MAX_10_EXP

  val M_LN2 = 0.693147180559945309417232121458
  val M_LN_SQRT_2PI	= 0.918938533204672741780329736406
  val M_1_SQRT_2PI = 0.398942280401432677939946059934
  val M_SQRT_32	= 5.656854249492380195206754896838
  val M_LN_SQRT_PId2 = 0.225791352644727432363097614947
  val M_2PI	= 6.283185307179586476925286766559
  val M_SQRT2 = 1.414213562373095048801688724210
  val M_1_PI = 0.318309886183790671537767526745
  val M_PI_2 = 1.570796326794896619231321691640
  val M_PI = 3.141592653589793238462643383279502884197169399375


  def D__0   (log_p: Boolean): Double = if(log_p) Double.NegativeInfinity else 0.0
  def D__1   (log_p: Boolean): Double = if(log_p) 0.0 else 1.0
  def DT_0   (lower_tail: Boolean, log_p: Boolean): Double = if(lower_tail) D__0(log_p) else D__1(log_p)
  def DT_1   (lower_tail: Boolean, log_p: Boolean): Double = if(lower_tail) D__1(log_p) else D__0(log_p)
  def D_Lval (p: Double, lower_tail: Boolean): Double = if(lower_tail) p else 0.5 - p + 0.5
  def D_Cval (p: Double, lower_tail: Boolean): Double = if(lower_tail) 0.5 - p + 0.5 else p
  def D_val  (q: Double, log_p: Boolean): Double = if(log_p) math.log(q) else q
  def D_qIv  (p: Double, log_p: Boolean): Double = if(log_p) math.exp(p) else p
  def D_exp  (q: Double, log_p: Boolean): Double = if(log_p) q else math.exp(q)
  def D_log  (p: Double, log_p: Boolean): Double = if(log_p) p else math.log(p)
  def D_Clog (p: Double, log_p: Boolean): Double = if(log_p) math.log1p(-p) else 0.5 - p + 0.5
  def D_LExp (q: Double, log_p: Boolean): Double = if(log_p) Log1_Exp(q) else math.log1p(-q)
  def DT_val (q: Double, lower_tail: Boolean, log_p: Boolean): Double = if(lower_tail) D_val(q, log_p) else D_Clog(q, log_p)
  def DT_Cval(q: Double, lower_tail: Boolean, log_p: Boolean): Double = if(lower_tail) D_Clog(q, log_p) else D_val(q, log_p)
  def DT_qIv (p: Double, lower_tail: Boolean, log_p: Boolean): Double = {if(log_p) {if(lower_tail) math.exp(p) else -math.expm1(p)} else {D_Lval(p, lower_tail)}}
  def DT_CIv (p: Double, lower_tail: Boolean, log_p: Boolean): Double = {if(log_p) {if(lower_tail) -math.expm1(p) else math.exp(p)} else {D_Cval(p, lower_tail)}}
  def DT_exp (q: Double, lower_tail: Boolean, log_p: Boolean): Double = D_exp(D_Lval(q, lower_tail), log_p)
  def DT_Cexp(q: Double, lower_tail: Boolean, log_p: Boolean): Double = D_exp(D_Cval(q, lower_tail), log_p)
  def DT_log (p: Double, lower_tail: Boolean, log_p: Boolean): Double = if(lower_tail) D_log(p, log_p) else D_LExp(p, log_p)
  def DT_Clog(p: Double, lower_tail: Boolean, log_p: Boolean): Double = if(lower_tail) D_LExp(p, log_p) else D_log(p, log_p)
  def DT_Log (p: Double, lower_tail: Boolean): Double = if(lower_tail) p else  Log1_Exp(p)
  def Q_P01_check(p: Double, log_p: Boolean) = if((log_p && p > 0) || (!log_p && (p < 0 || p > 1))) throw new IllegalArgumentException
  def Q_P01_boundaries(p: Double, LEFT: Double, RIGHT: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    if(log_p) {
      if(p > 0) return Double.NaN
      if(p == 0) {if(lower_tail) return RIGHT else return LEFT}
      if(p.isNegInfinity) {if(lower_tail) return LEFT else return RIGHT}
    } else {
      if(p < 0 || p > 1) return Double.NaN
      if(p == 0) {if(lower_tail) return LEFT else return RIGHT}
      if(p == 1) {if(lower_tail) return RIGHT else return LEFT}
    }
    Double.NaN
  }
  def P_bounds_01(q: Double, min: Double, max: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    q match {
      case x if x <= min => return DT_0(lower_tail, log_p)
      case x if x >= max => return DT_1(lower_tail, log_p)
    }
    Double.NaN
  }
  def P_bounds_Inf_01(q: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    if(q.isInfinite) {
      q match {
        case x if x > 0 => return DT_1(lower_tail, log_p)
        case _ => return DT_0(lower_tail, log_p)
      }
    }
    Double.NaN
  }





  def D_fexp(f: Double, x: Double, log_p: Boolean): Double = if(log_p) -0.5*math.log(f)+x else math.exp(x)/math.sqrt(f)
  def D_forceint(x: Double): Double = math.floor(x + 0.5)
  def D_nonint(x: Double): Boolean = math.abs(x - math.floor(x+0.5)) > 1e-7
  def D_negInonint(x: Double): Boolean = x < 0.0 || D_nonint(x)
  def Log1_Exp(x: Double): Double = if(x > -M_LN2) math.log(-math.expm1(x)) else math.log1p(-math.exp(x))
  def ldexp(x: Double, n: Int): Double = x * math.pow(2, n)



  def beta(a: Double, b: Double): Double = Gamma.gamma(a)*Gamma.gamma(b)/Gamma.gamma(a+b)
  def sign(a: Double): Int = if(a < 0.0) {-1} else if (a == 0.0) {0} else {1}
  def Heaviside(x: Double, a: Double = 0): Double = (sign(x - a) + 1)/2
  def tanpi(x: Double): Double = {
    if(x.isNaN) Double.NaN
    if(x.isInfinite) throw new IllegalArgumentException
    var xx = x % 1.0
    if(xx <= -0.5) xx += 1 else if (x > 0.5) xx -= 1
    if(xx == 0.0) {
      0.0
    } else {
      if(xx == 0.5) Double.NaN else math.tan(M_PI * xx)
    }
  }
  def log1pexp(x: Double): Double = {
    if(x.isNaN) Double.NaN
    if (x > 0) x + math.log1p(math.exp(-x)) else math.log1p(math.exp(x))
  }
}
