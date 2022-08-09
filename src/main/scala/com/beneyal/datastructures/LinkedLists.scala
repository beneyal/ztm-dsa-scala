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

      override def reverse: LinkedList = {
        @tailrec
        def loop(list: LinkedList, result: LinkedList): LinkedList =
          list match {
            case Empty             => result
            case Node(value, tail) => loop(tail, result.prepend(value))
          }

        loop(this, LinkedList.empty)
      }

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

    def empty: LinkedList = Empty
  }

  sealed trait DoublyLinkedList {
    def append(x: Int): DoublyLinkedList
    def prepend(x: Int): DoublyLinkedList
    def insert(index: Int, x: Int): DoublyLinkedList
    def remove(index: Int): DoublyLinkedList
    def reverse: DoublyLinkedList
    def isEmpty: Boolean

    def updateNext(newNext: => DoublyLinkedList): DoublyLinkedList
    def updatePrev(newPrev: => DoublyLinkedList): DoublyLinkedList
  }

  object DoublyLinkedList {
    case object Empty extends DoublyLinkedList {
      override def append(x: Int): DoublyLinkedList = new Node(x, Empty, Empty)

      override def prepend(x: Int): DoublyLinkedList = new Node(x, Empty, Empty)

      override def insert(index: Int, x: Int): DoublyLinkedList =
        throw new IllegalArgumentException(s"Index $index doesn't exist")

      override def remove(index: Int): DoublyLinkedList =
        throw new NoSuchElementException("Cannot remove from an empty list")

      override def reverse: DoublyLinkedList = Empty

      override def isEmpty: Boolean = true

      override def toString: String = "[]"

      override def updateNext(newNext: => DoublyLinkedList): DoublyLinkedList = this

      override def updatePrev(newPrev: => DoublyLinkedList): DoublyLinkedList = this
    }

    final class Node(val value: Int, p: => DoublyLinkedList, n: => DoublyLinkedList) extends DoublyLinkedList {
      lazy val prev: DoublyLinkedList = p
      lazy val next: DoublyLinkedList = n

      override def append(x: Int): DoublyLinkedList = {
        lazy val result: DoublyLinkedList = new Node(value, prev.updateNext(result), next.append(x).updatePrev(result))
        result
      }

      override def prepend(x: Int): DoublyLinkedList = {
        lazy val result: DoublyLinkedList = new Node(x, Empty, this.updatePrev(result))
        result
      }

      override def insert(index: Int, x: Int): DoublyLinkedList =
        if (index == 0) prepend(x)
        else new Node(value, prev, next.insert(index - 1, x))

      override def remove(index: Int): DoublyLinkedList =
        if (index == 0) next
        else new Node(value, prev, next.remove(index - 1))

      override def reverse: DoublyLinkedList = {
        @tailrec
        def loop(list: DoublyLinkedList, result: DoublyLinkedList): DoublyLinkedList =
          list match {
            case Empty      => result
            case node: Node => loop(node.next, result.prepend(node.value))
          }

        loop(this, DoublyLinkedList.empty)
      }

      override def isEmpty: Boolean = false

      override def toString: String = {
        @tailrec
        def loop(list: DoublyLinkedList, parts: Vector[String]): String =
          list match {
            case Empty      => parts.mkString("[", ", ", "]")
            case node: Node => loop(node.next, parts :+ node.value.toString)
          }

        loop(this, Vector.empty)
      }

      override def updateNext(newNext: => DoublyLinkedList): DoublyLinkedList = {
        lazy val result: DoublyLinkedList = new Node(value, prev.updateNext(result), newNext)
        result
      }

      override def updatePrev(newPrev: => DoublyLinkedList): DoublyLinkedList = {
        lazy val result: DoublyLinkedList = new Node(value, newPrev, next.updatePrev(result))
        result
      }
    }

    def apply(xs: Int*): DoublyLinkedList =
      xs.foldLeft(empty)(_.append(_))

    def empty: DoublyLinkedList = Empty
  }

  def main(args: Array[String]): Unit = {
    val list = LinkedList(1, 2, 3, 4)
    println(s"list = $list")
    println(s"list.append(5) = ${list.append(5)}")
    println(s"list.prepend(0) = ${list.prepend(0)}")
    println(s"list.insert(3, 42) = ${list.insert(3, 42)}")
    println(s"list.remove(3) = ${list.remove(3)}")
    println(s"list.reverse = ${list.reverse}")

    println()

    val dllist = DoublyLinkedList(1, 2, 3, 4)
    println(s"dllist = $dllist")
    println(s"dllist.append(5) = ${dllist.append(5)}")
    println(s"dllist.prepend(0) = ${dllist.prepend(0)}")
    println(s"dllist.insert(3, 42) = ${dllist.insert(3, 42)}")
    println(s"dllist.remove(3) = ${dllist.remove(3)}")
    println(s"dllist.reverse = ${dllist.reverse}")
  }
}
