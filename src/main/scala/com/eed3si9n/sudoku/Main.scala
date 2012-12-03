package com.eed3si9n.sudoku

import java.io.File

object Main extends App {
  Solver.solve(Reader.read(new File("data/2.sdk")))
}
