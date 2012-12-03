package com.eed3si9n.sudoku

import java.io.File

object Reader {
  import scalaz._
  import Scalaz._

  def read(path: String): Game = read(new File(path))
  def read(file: File): Game = {
    val source = scala.io.Source.fromFile(file, "UTF-8")
    val lines = Vector(source.getLines.toSeq filterNot { x => x.isEmpty || (x startsWith "#") }: _*)
    val cells = lines.zipWithIndex flatMap { case (line, idx) =>
      val cs = Vector(line.toSeq: _*)
      (1 |-> lines.size) map { x =>
        Cell((x, idx + 1), cs(x - 1).toString.parseInt.toOption)
      }
    }
    Game(cells)
  }
}

case class Game(cells: Vector[Cell]) {
  import scalaz._
  import Scalaz._

  val n: Int = math.pow(cells.size, 0.5).round.toInt
  val sqrtn: Int = math.pow(n, 0.5).round.toInt
  val allValues = Vector((1 |-> n): _*)
  def apply(pos: (Int, Int)) = (cells.find {_.pos == pos}).get
  override def toString: String = {
    (allValues flatMap { y =>
      allValues flatMap { x =>
        val cell = apply((x, y))
        (cell.value map { x =>
        cell.value.toString
        } getOrElse {cell.cs.toString}) +
        (if (x == n) "\n"
        else " ")
      }
    }).mkString
  }
}

case class Cell(pos: (Int, Int),
  value: Option[Int],
  cs: Vector[Int] = Vector())
