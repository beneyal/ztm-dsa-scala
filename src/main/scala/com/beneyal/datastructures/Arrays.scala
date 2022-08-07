package com.beneyal.datastructures

import scala.annotation.tailrec

object Arrays {
  def reverseString(str: String): String = {
    @tailrec
    def loop(i: Int, result: Vector[Char]): String =
      if (i < str.length) {
        loop(i + 1, result :+ str(i))
      } else {
        result.mkString
      }

    loop(0, Vector.empty)
  }

  def mergeSortedArrays(a: Vector[Int], b: Vector[Int]): Vector[Int] = {
    @tailrec
    def loop(i: Int, j: Int, result: Vector[Int]): Vector[Int] =
      if (i < a.length && j < b.length) {
        if (a(i) < b(j)) loop(i + 1, j, result :+ a(i))
        else loop(i, j + 1, result :+ b(j))
      } else if (i < a.length) {
        loop(i + 1, j, result :+ a(i))
      } else if (j < b.length) {
        loop(i, j + 1, result :+ b(j))
      } else {
        result
      }

    loop(0, 0, Vector.empty)
  }

  def main(args: Array[String]): Unit = {
    println(mergeSortedArrays(Vector(0, 3, 4, 31), Vector(4, 6, 30)))
  }
}
