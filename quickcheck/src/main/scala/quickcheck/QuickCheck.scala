package quickcheck

import java.util.NoSuchElementException

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.{Failure, Success, Try}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: A, b: A) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == {
      if (a < b) a else b
    }
  }

  property("gen3") = forAll { (a: A) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("gen4") = forAll { xs:List[A] =>
    def findAndDel(h: H): List[A] = {
      if(isEmpty(h)) Nil
      else findMin(h)::findAndDel(deleteMin(h))
    }
    def insertInHeap(h: H,xs:List[A]):H = xs match {
      case Nil => empty
      case head::tail => insert(head,insertInHeap(h,tail))
    }
    val heap = insertInHeap(empty,xs)
    val ys = findAndDel(heap)
    xs.sorted == ys
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    val a = Try(findMin(h1))
    val b = Try(findMin(h2))
    val c = Try(findMin(meld(h1, h2)))
    val min = (a,b) match {
      case (Success(v1),Success(v2)) => if(v1<v2) v1 else v2
      case (Success(v1),Failure(_)) =>  v1
      case (Failure(_),Success(v2)) =>  v2
      case (Failure(_),Failure(_)) => Nil
    }
    val bool = c match{
      case Success(v) => v==min
      case Failure(e1) => e1.getMessage == "min of empty heap"
    }
    bool
  }

}
