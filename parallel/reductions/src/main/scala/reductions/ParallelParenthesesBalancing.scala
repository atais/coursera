package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.util.Try

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def balance(chars: Array[Char], counter: Int): Boolean = {
      if (chars.isEmpty) {
        counter == 0
      } else if (chars.head == '(') {
        balance(chars.tail, counter + 1)
      } else if (chars.head == ')') {
        if (counter == 0) throw new RuntimeException("unbalanced")
        else balance(chars.tail, counter - 1)
      } else {
        balance(chars.tail, counter)
      }
    }
    Try(balance(chars, 0)).getOrElse(false)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, sum: Int, closing: Int): (Int, Int) = {
      if (idx == until) (sum, closing)
      else if (chars(idx) == '(') traverse(idx + 1, until, sum + 1, closing)
      else if (chars(idx) == ')') {
        if (sum == 0) traverse(idx + 1, until, sum, closing + 1)
        else traverse(idx + 1, until, sum - 1, closing)
      }
      else traverse(idx + 1, until, sum, closing)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from > threshold) {
        val ((s1, c1), (s2, c2)) = parallel(reduce(from, (from + until) / 2), reduce((from + until) / 2, until))
        if (from == 0 && s1 - c2 < 0) throw new RuntimeException("unbalanced")
        (s1 - c2 + s2, c1)
      } else {
        traverse(from, until, 0, 0)
      }
    }
    Try(reduce(0, chars.length) ==(0, 0)).getOrElse(false)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
