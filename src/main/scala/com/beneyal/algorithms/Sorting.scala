package com.beneyal.algorithms

import scala.collection.mutable

object Sorting {
  def bubbleSort(arr: Array[Int]): Array[Int] = {
    val result = arr.clone()
    for (_ <- result.indices) {
      for (j <- 0 until result.length - 1) {
        if (result(j) > result(j + 1)) {
          val temp = result(j)
          result(j) = result(j + 1)
          result(j + 1) = temp
        }
      }
    }
    result
  }

  def selectionSort(arr: Array[Int]): Array[Int] = {
    val result = arr.clone()
    for (i <- result.indices) {
      var minIndex = i
      for (j <- i until result.length) {
        if (result(j) < result(minIndex)) {
          minIndex = j
        }
      }
      val temp = result(i)
      result(i) = result(minIndex)
      result(minIndex) = temp
    }
    result
  }

  def insertionSort(arr: Array[Int]): Array[Int] = {
    val result = mutable.ArrayBuffer.from(arr)
    for (i <- result.indices) {
      if (result(i) < result(0)) {
        val x = result(i)
        result.remove(i)
        result.insert(0, x)
      } else {
        for (j <- 1 until i) {
          if (result(i) > result(j - 1) && result(i) < result(j)) {
            val x = result(i)
            result.remove(i)
            result.insert(j, x)
          }
        }
      }
    }
    result.toArray
  }

  def mergeSort(arr: Array[Int]): Array[Int] = {
    def merge(left: Array[Int], right: Array[Int]): Array[Int] = {
      val result    = Array.fill(left.length + right.length)(0)
      var (i, j, k) = (0, 0, 0)
      while (i < left.length && j < right.length) {
        if (left(i) < right(j)) {
          result(k) = left(i)
          i += 1
        } else {
          result(k) = right(j)
          j += 1
        }
        k += 1
      }
      while (i < left.length) {
        result(k) = left(i)
        i += 1
        k += 1
      }
      while (j < right.length) {
        result(k) = right(j)
        j += 1
        k += 1
      }
      result
    }

    def recur(arr: Array[Int]): Array[Int] =
      if (arr.length <= 1) {
        arr
      } else {
        val (left, right) = arr.splitAt(arr.length / 2)
        merge(recur(left), recur(right))
      }

    recur(arr)
  }

  def quickSort(arr: Array[Int]): Array[Int] = {
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
    val numbers = Array(99, 44, 6, 2, 1, 5, 63, 87, 283, 4, 0)
    println(bubbleSort(numbers).mkString("Array(", ", ", ")"))
    println(selectionSort(numbers).mkString("Array(", ", ", ")"))
    println(insertionSort(numbers).mkString("Array(", ", ", ")"))
    println(mergeSort(numbers).mkString("Array(", ", ", ")"))
    println(quickSort(numbers).mkString("Array(", ", ", ")"))
  }
}
