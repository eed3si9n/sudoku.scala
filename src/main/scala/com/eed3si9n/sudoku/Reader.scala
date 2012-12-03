package com.eed3si9n.sudoku

import java.io.File

object Reader {
  import scalaz._
  import Scalaz._

  def read(path: String): Vector[Cell] = read(new File(path))
  def read(file: File): Vector[Cell] = {
    val source = scala.io.Source.fromFile(file, "UTF-8")
    val lines = Vector(source.getLines.toSeq filterNot { x => x.isEmpty || (x startsWith "#") }: _*)
    lines.zipWithIndex flatMap { case (line, idx) =>
      val cs = Vector(line.toSeq: _*)
      (1 |-> lines.size) map { x =>
        Cell((x, idx + 1), cs(x - 1).toString.parseInt.toOption)
      }
    }
  }
}

case class Cell(pos: (Int, Int), value: Option[Int])
