package com.beneyal.datastructures

import scala.annotation.tailrec

object LinkedLists {
  sealed trait LinkedList {
    def append(x: Int): LinkedList
    def prepend(x: Int): LinkedList
    def insert(index: Int, x: Int): LinkedList
    def remove(index: Int): LinkedList
    def reverse: LinkedList
    def isEmpty: Boolean
  }

  object LinkedList {
    case object Empty extends LinkedList {
      override def append(x: Int): LinkedList  = Node(x, Empty)
      override def prepend(x: Int): LinkedList = Node(x, Empty)
      override def insert(index: Int, x: Int): LinkedList =
        throw new IllegalArgumentException(s"Index $index doesn't exist")
      override def remove(index: Int): LinkedList = throw new NoSuchElementException("Removing from empty list")
      override def reverse: LinkedList            = Empty
      override def isEmpty: Boolean               = true
      override def toString: String               = "[]"
    }

    final case class Node(value: Int, tail: LinkedList) extends LinkedList {
      override def append(x: Int): LinkedList =
        if (tail.isEmpty) Node(value, Node(x, tail))
        else Node(value, tail.append(x))

      override def prepend(x: Int): LinkedList = Node(x, this)

      override def insert(index: Int, x: Int): LinkedList =
        if (index == 0) prepend(x)
        else Node(value, tail.insert(index - 1, x))

      override def remove(index: Int): LinkedList =
        if (index == 0) tail
        else Node(value, tail.remove(index - 1))

      override def reverse: LinkedList =
        if (tail.isEmpty) this
        else tail.reverse.append(value)

      override def isEmpty: Boolean = false

      override def toString: String = {
        @tailrec
        def loop(list: LinkedList, parts: Vector[String]): String =
          list match {
            case Empty             => parts.mkString("[", ", ", "]")
            case Node(value, tail) => loop(tail, parts :+ value.toString)
          }

        loop(this, Vector.empty)
      }
    }

    def apply(xs: Int*): LinkedList =
      if (xs.isEmpty) Empty
      else Node(xs.head, apply(xs.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val list = LinkedList(1, 2, 3, 4)
    println(s"list = $list")
    println(s"list.append(5) = ${list.append(5)}")
    println(s"list.prepend(0) = ${list.prepend(0)}")
    println(s"list.insert(3, 42) = ${list.insert(3, 42)}")
    println(s"list.remove(3) = ${list.remove(3)}")
    println(s"list.reverse = ${list.reverse}")
  }
}
