package com.beneyal.datastructures

import scala.annotation.tailrec

object StacksAndQueues {
  trait Stack[A] {
    def peek: Option[A]
    def push(x: A): Stack[A]
    def pop: (Option[A], Stack[A])
    def isEmpty: Boolean
  }

  final case class LinkedListStack[A] private (private val xs: List[A]) extends Stack[A] {
    override def peek: Option[A]      = xs.headOption
    override def push(x: A): Stack[A] = LinkedListStack(x :: xs)
    override def pop: (Option[A], Stack[A]) = xs match {
      case first :: rest => (Some(first), LinkedListStack(rest))
      case Nil           => (None, this)
    }
    override def isEmpty: Boolean = xs.isEmpty
  }

  object LinkedListStack {
    def apply[A](xs: A*): Stack[A] = LinkedListStack(xs.toList)
    def empty[A]: Stack[A]         = LinkedListStack(Nil)
  }

  trait Queue[A] {
    def peek: Option[A]
    def enqueue(x: A): Queue[A]
    def dequeue: (Option[A], Queue[A])
    def isEmpty: Boolean
  }

  final case class ArrayQueue[A] private (private val xs: Vector[A]) extends Queue[A] {
    override def peek: Option[A]         = xs.headOption
    override def enqueue(x: A): Queue[A] = ArrayQueue(xs :+ x)
    override def dequeue: (Option[A], Queue[A]) = xs match {
      case first +: rest => (Some(first), ArrayQueue(rest))
      case _             => (None, this)
    }
    override def isEmpty: Boolean = xs.isEmpty
  }

  object ArrayQueue {
    def apply[A](xs: A*): Queue[A] = ArrayQueue(xs.toVector)
    def empty[A]: Queue[A]         = ArrayQueue(Vector.empty)
  }

  final case class StackQueue[A] private (private val s1: Stack[A], private val s2: Stack[A]) extends Queue[A] {
    override def peek: Option[A] = {
      val newQueue = if (s2.isEmpty) switchedStacks else this
      newQueue.s2.peek
    }

    override def enqueue(x: A): Queue[A] = StackQueue(s1.push(x), s2)

    override def dequeue: (Option[A], Queue[A]) = {
      val newQueue      = if (s2.isEmpty) switchedStacks else this
      val (x, newStack) = newQueue.s2.pop
      (x, newQueue.copy(s2 = newStack))
    }

    private def switchedStacks: StackQueue[A] = {
      @tailrec
      def loop(s1: Stack[A], s2: Stack[A]): StackQueue[A] =
        if (s1.isEmpty) {
          StackQueue(s1, s2)
        } else {
          val (Some(x), newStack) = s1.pop
          loop(newStack, s2.push(x))
        }

      loop(s1, s2)
    }

    override def isEmpty: Boolean = s1.isEmpty && s2.isEmpty
  }

  object StackQueue {
    def empty[A]: StackQueue[A] = StackQueue(LinkedListStack.empty, LinkedListStack.empty)
  }

  def main(args: Array[String]): Unit = {
    val q       = StackQueue.empty[Int]
    val (x, q2) = q.enqueue(1).enqueue(2).dequeue
    println(x)
    println(q2.peek)
    val (y, _) = q2.enqueue(3).enqueue(4).dequeue
    println(y)
  }
}
