package au.com.realcommercial.bindingScala

import com.thoughtworks.each.Monadic._
import au.com.realcommercial.bindingScala.BindableRope._
import utest._

import scala.collection.mutable.ArrayBuffer


object BindableRopeTest extends TestSuite {
  def tests = TestSuite {

    'TestSingle {
      val rope = new Single(5)
      assert(rope.flatten == Seq(5))
      rope.flatten(0) += 100
      assert(rope.flatten == Seq(105))
    }

    'TestArrayTree {
      val rope = new ArrayTree[Int](Array(new Single(5)))
      assert(rope.flatten == Seq(5))
    }

    'TestArrayLeaf {
      val rope = new ArrayLeaf[Int](Array(5))
      assert(rope.flatten == Seq(5))
      rope(0) += 100
      assert(rope.flatten == Seq(105))
    }

    'TestNestedArrayTree {
      val rope = new ArrayTree[Int](Array(new Single(0), new ArrayLeaf(Array(1, 2, 3)), new ArrayLeaf(Array(4, 5, 6))))
      assert(rope.flatten == Seq(0, 1, 2, 3, 4, 5, 6))
    }

    'TestBufferLeaf {
      val rope = new BufferLeaf[Int](ArrayBuffer(5))
      assert(rope.flatten == Seq(5))
      rope(0) += 100
      assert(rope.flatten == Seq(105))
      rope += 200
      assert(rope.flatten == Seq(105, 200))
      300 +=: rope
      assert(rope.flatten == Seq(300, 105, 200))
    }


    'TestUpdate {
      val leaf0 = new BufferLeaf(ArrayBuffer(0, 1, 2, 3))
      val leaf1 = new BufferLeaf(ArrayBuffer(7, 8))
      val rope = new ArrayTree(Array[BindableRope[Int]](leaf0, new Single(4), new ArrayLeaf(Array(5, 6)), leaf1))
      assert(rope.flatten == Seq(0, 1, 2, 3, 4, 5, 6, 7, 8))
      val ropeChangeSet = new ArrayBuffer[(Int, Int)]
      rope.subscribe(new Subscriber[Int] {
        override def insert(parent: BindableRope[Int], index: Int, newChildren: Int*): Unit = ???

        override def update(parent: BindableRope[Int], index: Int, newChild: Int): Unit = {
          ropeChangeSet += (index -> newChild)
        }

        override def remove(parent: BindableRope[Int], index: Int): Unit = ???

        override def splice(source: BindableRope[Int], index: Int, numberOfOldChildren: Int, newChildren: Int*): Unit = ???
      })
      val leaf0ChangeSet = new ArrayBuffer[(Int, Int)]
      leaf0.subscribe(new Subscriber[Int] {
        override def insert(parent: BindableRope[Int], index: Int, newChildren: Int*): Unit = ???

        override def update(parent: BindableRope[Int], index: Int, newChild: Int): Unit = {
          leaf0ChangeSet += (index -> newChild)
        }

        override def remove(parent: BindableRope[Int], index: Int): Unit = ???

        override def splice(source: BindableRope[Int], index: Int, numberOfOldChildren: Int, newChildren: Int*): Unit = ???
      })
      leaf0(1) *= 100
      assert(rope.flatten == Seq(0, 100, 2, 3, 4, 5, 6, 7, 8))
      assert(leaf0ChangeSet == Seq(1 -> 100))
      assert(ropeChangeSet == Seq(1 -> 100))
    }

    'TestUpdateTree {
      val leaf0 = new BufferLeaf(ArrayBuffer(0, 1, 2, 3))
      val leaf1 = new BufferLeaf(ArrayBuffer(7, 8))
      val rope = new ArrayTree(Array[BindableRope[Int]](leaf0, new Single(4), new ArrayLeaf(Array(5, 6)), leaf1))
      assert(rope.flatten == Seq(0, 1, 2, 3, 4, 5, 6, 7, 8))
      rope(1) = new ArrayLeaf(Array(400, 401))
      assert(rope.flatten == Seq(0, 1, 2, 3, 400, 401, 5, 6, 7, 8))
    }
  }

}
