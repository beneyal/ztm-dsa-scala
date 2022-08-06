package com.beneyal.algorithms

import com.beneyal.datastructures.Trees.{exampleTree, BinarySearchTree}

import scala.annotation.tailrec

object Searching {
  def linearSearch(arr: Array[Int], key: Int): Option[Int] = {
    @tailrec
    def loop(i: Int): Option[Int] =
      if (i == arr.length) None
      else if (arr(i) == key) Some(key)
      else loop(i + 1)

    loop(0)
  }

  def binarySearch(arr: Array[Int], key: Int): Option[Int] = {
    @tailrec
    def loop(arr: Array[Int]): Option[Int] =
      if (arr.isEmpty) {
        None
      } else {
        val mid = arr(arr.length / 2)
        if (key == mid) {
          Some(key)
        } else if (key < mid) {
          loop(arr.take(arr.length / 2))
        } else {
          loop(arr.drop(arr.length / 2 + 1))
        }
      }

    loop(arr)
  }

  def bfs(tree: BinarySearchTree): Vector[Int] = {
    @tailrec
    def loop(queue: Vector[BinarySearchTree], result: Vector[Int]): Vector[Int] =
      queue match {
        case Vector() => result
        case first +: rest =>
          first match {
            case BinarySearchTree.Empty                    => loop(rest, result)
            case BinarySearchTree.Node(value, left, right) => loop(rest :+ left :+ right, result :+ value)
          }
      }

    loop(Vector(tree), Vector.empty)
  }

  def inOrder(tree: BinarySearchTree): Vector[Int] = {
    tree match {
      case BinarySearchTree.Empty                    => Vector.empty[Int]
      case BinarySearchTree.Node(value, left, right) => inOrder(left) ++ Vector(value) ++ inOrder(right)
    }
  }

  def main(args: Array[String]): Unit = {
    val numbers = Array(99, 44, 6, 2, 1, 5, 63, 87, 283, 4, 0).sorted
    println(linearSearch(numbers, 99))
    println(binarySearch(numbers, 99))
    println(bfs(exampleTree).mkString("[", ", ", "]"))
    println(inOrder(exampleTree).mkString("[", ", ", "]"))
  }
}
