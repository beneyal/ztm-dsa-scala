package com.beneyal.algorithms

import com.beneyal.datastructures.Arrays.mergeSortedArrays

import scala.annotation.tailrec

object Sorting {
  def bubbleSort(arr: Vector[Int]): Vector[Int] = {
    @tailrec
    def getMaxAndRest(arr: Vector[Int], i: Int, max: Int, rest: Vector[Int]): (Int, Vector[Int]) =
      if (i == arr.length) (max, rest)
      else if (arr(i) > max) getMaxAndRest(arr, i + 1, arr(i), rest :+ max)
      else getMaxAndRest(arr, i + 1, max, rest :+ arr(i))

    @tailrec
    def loop(arr: Vector[Int], result: Vector[Int]): Vector[Int] =
      if (arr.isEmpty) {
        result
      } else {
        val (max, rest) = getMaxAndRest(arr, 1, arr(0), Vector.empty)
        loop(rest, max +: result)
      }

    loop(arr, Vector.empty)
  }

  def selectionSort(arr: Vector[Int]): Vector[Int] = {
    @tailrec
    def findMinIndex(arr: Vector[Int], i: Int, min: Int, minIndex: Int): Int =
      if (i == arr.length) minIndex
      else if (arr(i) < min) findMinIndex(arr, i + 1, arr(i), i)
      else findMinIndex(arr, i + 1, min, minIndex)

    @tailrec
    def loop(arr: Vector[Int], result: Vector[Int]): Vector[Int] =
      if (arr.isEmpty) {
        result
      } else {
        val minIndex = findMinIndex(arr, 1, arr(0), 0)
        loop(arr.patch(minIndex, Nil, 1), result :+ arr(minIndex))
      }

    loop(arr, Vector.empty)
  }

  def insertionSort(arr: Vector[Int]): Vector[Int] = {
    @tailrec
    def insert(elem: Int, sorted: Vector[Int], i: Int, result: Vector[Int]): Vector[Int] =
      if (i == sorted.length) {
        result :+ elem
      } else if (elem < sorted(i)) {
        (result :+ elem) ++ sorted.drop(i)
      } else {
        insert(elem, sorted, i + 1, result :+ sorted(i))
      }

    @tailrec
    def loop(arr: Vector[Int], i: Int, sorted: Vector[Int]): Vector[Int] =
      if (i == arr.length) {
        sorted
      } else if (i == 0) {
        loop(arr, 1, Vector(arr(0)))
      } else {
        loop(arr, i + 1, insert(arr(i), sorted, 0, Vector.empty))
      }

    loop(arr, 0, Vector.empty)
  }

  def mergeSort(arr: Vector[Int]): Vector[Int] = {
    def recur(arr: Vector[Int]): Vector[Int] =
      if (arr.length <= 1) {
        arr
      } else {
        val (left, right) = arr.splitAt(arr.length / 2)
        mergeSortedArrays(recur(left), recur(right))
      }

    recur(arr)
  }

  def quickSort(arr: Vector[Int]): Vector[Int] = {
    if (arr.length <= 1) {
      arr
    } else {
      val pivot  = arr(0)
      val left   = arr.filter(_ < pivot)
      val middle = arr.filter(_ == pivot)
      val right  = arr.filter(_ > pivot)
      quickSort(left) ++ middle ++ quickSort(right)
    }
  }

  def main(args: Array[String]): Unit = {
    val numbers = Vector(99, 44, 6, 2, 1, 5, 63, 87, 283, 4, 0)
    println(bubbleSort(numbers).mkString("[", ", ", "]"))
    println(selectionSort(numbers).mkString("[", ", ", "]"))
    println(insertionSort(numbers).mkString("[", ", ", "]"))
    println(mergeSort(numbers).mkString("[", ", ", "]"))
    println(quickSort(numbers).mkString("[", ", ", "]"))
  }
}
