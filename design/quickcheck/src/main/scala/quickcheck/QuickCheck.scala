package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.util.Try

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min 1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min 2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == math.min(a, b)
  }

  property("max 2") = forAll { (a: Int, b: Int) =>
    val h = deleteMin(insert(a, insert(b, empty)))
    findMin(h) == math.max(a, b)
  }

  property("empty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("empty") = forAll { (a: Int) =>
    val h = Try(deleteMin(deleteMin(insert(a, empty))))
    h.isFailure
  }

  property("meld") = forAll { (a: Int, b: Int) =>
    val h = meld(insert(a, empty), insert(b, empty))
    findMin(h) == math.min(a, b)
  }

  property("meld with one") = forAll { (a: Int) =>
    val h = meld(insert(a, empty), insert(a, empty))
    findMin(h) == a
  }

  property("meld with one 2") = forAll { (a: Int) =>
    val h = deleteMin(meld(insert(a, empty), insert(a, empty)))
    findMin(h) == a
  }

  property("meld with empty 2") = forAll { (a: Int) =>
    val h = deleteMin(meld(insert(a, empty), empty))
    isEmpty(h)
  }

  property("meld with empty") = forAll { (a: Int) =>
    val h = meld(insert(a, empty), empty)
    findMin(h) == a
  }

  property("meld 2") = forAll { (a: H, b: H) =>
    val h = meld(a, b)
    findMin(h) == math.min(findMin(a), findMin(b))
  }

  property("meld 3") = forAll { (a: H, b: H, c: Int) =>
    val h = meld(insert(c, a), insert(c, b))
    findMin(h) == math.min(c, math.min(findMin(a), findMin(b)))
  }

  property("insert big") = forAll { (a: List[A]) =>
    val h = a.foldLeft(empty)((acc, v) => insert(v, acc))

    if (a.isEmpty) {
      isEmpty(h)
    } else {
      findMin(h) == a.sorted.head
    }
  }

  property("gen1") = forAll { (h: H) =>
    def getMin(h: H, list: List[A]): List[A] = {
      if (isEmpty(h)) list
      else getMin(deleteMin(h), list :+ findMin(h))
    }
    val v = getMin(h, List.empty)
    v == v.sorted
  }

  property("gen1") = forAll { (start: List[A]) =>
    val h = start.foldLeft(empty)((acc, v) => insert(v, acc))

    def getMin(h: H, list: List[A]): List[A] = {
      if (isEmpty(h)) list
      else getMin(deleteMin(h), list :+ findMin(h))
    }
    val v = getMin(h, List.empty)
    v == start.sorted
  }


}
