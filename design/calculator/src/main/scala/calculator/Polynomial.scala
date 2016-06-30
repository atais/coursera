package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)
    if (delta() > 0) {
      Signal(Set(
        (-b() + math.sqrt(delta())) / (2 * a()),
        (-b() - math.sqrt(delta())) / (2 * a())
      ))
    } else if (delta() == 0) {
      Signal(Set(-b() / (2 * a())))
    } else {
      Signal(Set.empty)
    }
  }
}
