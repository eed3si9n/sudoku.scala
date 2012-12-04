package com.eed3si9n.sudoku

import java.io.File

object Main extends App {
  args.headOption map { arg => Solver.solve(Reader.read(new File("data/2.sdk")))
  } getOrElse println("usage: > run data/1.sdk")
}
