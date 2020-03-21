package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      e <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(e, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(h: H): List[A] = {
    if (isEmpty(h))
      Nil
    else
      findMin(h) :: toList(deleteMin(h))
  }
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: Int, b: Int) =>
    val h = meld(insert(a, empty), insert(b, empty))
    val m = if (a < b) a else b
    findMin(h) == m
  }

  property("gen3") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("gen4") = forAll { (h: H) =>
    val res = toList(h)
    res == res.sorted
  }

  property("gen4") = forAll { (h1: H, h2: H) =>
    val l = toList(meld(h1, h2))
    val l1 = toList(h1)
    val l2 = toList(h2)
    l == (l1 ++ l2).sorted
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    if (!isEmpty(h)) {
      val m = findMin(h)
      if (isEmpty(h1)) {
        m == findMin(h2)
      }
      else if (isEmpty(h2)) {
        m == findMin(h1)
      }
      else {
        m == findMin(h1) || m == findMin(h2)
      }
    }
    else true
  }
}
