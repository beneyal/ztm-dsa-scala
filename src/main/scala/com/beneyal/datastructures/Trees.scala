package com.beneyal.datastructures

object Trees {
  sealed trait BinarySearchTree {
    def insert(x: Int): BinarySearchTree
    def lookup(x: Int): Boolean
    def remove(x: Int): BinarySearchTree
    def findMin: Option[BinarySearchTree.Node]
  }

  object BinarySearchTree {

    case object Empty extends BinarySearchTree {
      override def insert(x: Int): BinarySearchTree = Node(x, Empty, Empty)

      override def lookup(x: Int): Boolean = false

      override def remove(x: Int): BinarySearchTree = throw new NoSuchElementException("Removing from empty BST.")

      override def findMin: Option[Node] = None
    }

    final case class Node(value: Int, left: BinarySearchTree, right: BinarySearchTree) extends BinarySearchTree {
      override def insert(x: Int): BinarySearchTree =
        if (x <= value) Node(value, left.insert(x), right)
        else Node(value, left, right.insert(x))

      override def lookup(x: Int): Boolean =
        if (x == value) true
        else if (x <= value) left.lookup(x)
        else right.lookup(x)

      override def remove(x: Int): BinarySearchTree = {
        if (x == value) {
          (left, right) match {
            case (Empty, Empty)                      => Empty
            case (Empty, rightChild @ Node(_, _, _)) => rightChild
            case (leftChild @ Node(_, _, _), Empty)  => leftChild
            case (leftChild @ Node(_, _, _), rightChild @ Node(v2, l2, r2)) =>
              val Some(Node(v, _, _)) = rightChild.findMin
              Node(v, leftChild, Node(v2, l2.remove(v), r2))
          }
        } else if (x <= value) {
          Node(value, left.remove(x), right)
        } else {
          Node(value, left, right.remove(x))
        }
      }

      override def findMin: Option[Node] =
        left match {
          case Empty            => Some(this)
          case Node(_, left, _) => left.findMin
        }
    }
  }

  val exampleTree: BinarySearchTree = BinarySearchTree.Empty
    .insert(9)
    .insert(4)
    .insert(6)
    .insert(20)
    .insert(170)
    .insert(15)
    .insert(1)
    .remove(170)

  def main(args: Array[String]): Unit = {
    pprint.pprintln(exampleTree)
  }
}
