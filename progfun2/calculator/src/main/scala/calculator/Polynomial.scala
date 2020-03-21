package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      // delta = b^2 - 4ac
      math.pow(b.apply(), 2) - 4 * a.apply() * c.apply()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta.apply() < 0)
        Set(Double.NaN)
      else
        Set(
          (-b.apply() - math.sqrt(delta.apply())) / 2 * a.apply(),
          (-b.apply() + math.sqrt(delta.apply())) / 2 * a.apply()
        )
    }
  }
}
