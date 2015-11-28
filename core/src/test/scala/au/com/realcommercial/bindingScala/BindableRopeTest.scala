package au.com.realcommercial.bindingScala

import au.com.realcommercial.bindingScala.BindableRope.{Single, FixedTree, FixedLeaf, Growable}
import utest._


object BindableRopeTest extends TestSuite {
  def tests = TestSuite {

    'TestSingle {
      val rope = new Single(5)
      assert(rope.toSeq == Seq(5))
      rope.toSeq(0) += 100
      assert(rope.toSeq == Seq(105))
    }

    'TestFixTree {
      val rope =  new FixedTree[Int](Array(new Single(5)))
      assert(rope.toSeq == Seq(5))
      rope.toSeq(0) += 100
      assert(rope.toSeq == Seq(105))
    }

    'TestFixLeaf {
      val rope =  new FixedLeaf[Int](Array(5))
      assert(rope.toSeq == Seq(5))
      rope.toSeq(0) += 100
      assert(rope.toSeq == Seq(105))
    }

    'TestRope {
      val rope = new FixedTree[Int](Array(new Single(0), new FixedLeaf(Array(1, 2, 3)), new FixedLeaf(Array(4, 5, 6))))
      assert(rope.toSeq == Seq(0, 1, 2, 3, 4, 5, 6))
      rope.toSeq(3) *= 10
      println(rope.toSeq)
      assert(rope.toSeq == Seq(0, 1, 2, 30, 4, 5, 6))
    }
  }

}
