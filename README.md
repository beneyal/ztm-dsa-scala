# Master the Coding Interview: Data Structures + Algorithms - Scala Edition

## Introduction

This repository is a collection of solutions to exercises from the course [Master the Coding Interview: Data Structures + Algorithms](https://zerotomastery.io/courses/learn-data-structures-and-algorithms/).

Solutions are written in Scala 2.13.8.

As Scala is a hybrid object-oriented and functional programming language, I have implemented all solutions as functionally as possible. This means that (for the most part) there won't be in-place mutations, no `null`s, and there will be lots of recursion.

If you see the `@tailrec` annotation above a function definition, this means it is recursive, but will essentially compile to an iterative loop (so no extra stack space is consumed).

## Requirements

- Java Development Kit version 8 or later
- [sbt](https://www.scala-sbt.org/)

## Playing Around

Clone the repository, `cd` into it, run `sbt run` and select the module to run.

## Coding

If you want to change the code, I recommend either [JetBrains IntelliJ IDEA](https://www.jetbrains.com/idea/) with the Scala plugin, or [Visual Studio Code](https://code.visualstudio.com/) with the [Metals](https://scalameta.org/metals/) plugin.

## Contributing

This repository is mostly a one-and-done, but if you want to improve or add solutions, feel free to open an issue or a pull request and I will review them when I can.
