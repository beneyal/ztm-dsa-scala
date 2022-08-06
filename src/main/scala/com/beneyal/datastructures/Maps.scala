package com.beneyal.datastructures

import scala.annotation.tailrec

object Maps {
  def firstRecurringCharacter[A](xs: Vector[A]): Option[A] = {
    @tailrec
    def loop(xs: Vector[A], seen: Set[A]): Option[A] =
      xs match {
        case first +: _ if seen(first) => Some(first)
        case first +: rest             => loop(rest, seen + first)
        case _                         => None
      }

    loop(xs, Set.empty)
  }
}
