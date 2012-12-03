package com.eed3si9n.sudoku

import scalaz._
import Scalaz._
import typelevel._

object Solver {
  def solve(game: Game) {
    def doLoop(g: Game) {
      println(g.toString)
      if (g.isSolved) println("solved")
      else {
        val g2 = runOnce(g)
        if (g == g2) sys.error("solver is stuck")
        else doLoop(g2)
      }
    }
    doLoop(game)
  }
  def runOnce(game: Game): Game = {
    val (nonEmptyCells, emptyCells) = game.cells partition {_.value.isDefined}
    def homogenize[M[_], B, T <: HList](xs: HCons[M[B], T]): List[M[B]] =
      xs.fold[Id, List[M[B]], Homogenize[M[B]]](new Homogenize) 
    def foldCells(xs: Vector[Cell], game: Game): Vector[Cell] = {
      def f[T <: TCList](cell: Cell) = Solver.cellMachine(cell.pos, game.sqrtn)
      val hnil = AppFunc.HNil[Cell, List[Unit]]
      val css = if (xs.isEmpty) Nil
        else if (xs.size === 1) homogenize((f(xs(0)) :: hnil) traverse game.cells) map {_ exec game.allValues}
        else if (xs.size === 2) homogenize((f(xs(1)) :: f(xs(0)) :: hnil) traverse game.cells) map {_ exec game.allValues}
        else if (xs.size === 3) homogenize((f(xs(2)) :: f(xs(1)) :: f(xs(0)) :: hnil) traverse game.cells) map {_ exec game.allValues}
        else if (xs.size === 4) homogenize((f(xs(3)) :: f(xs(2)) :: f(xs(1)) :: f(xs(0)) :: hnil) traverse game.cells) map {_ exec game.allValues}
        else sys.error("invalid")
      (xs zip css.reverse) map { case (cell, cs) =>
        cell.copy(cs = cs)
      }
    }
    val cellsWithCs = Vector((emptyCells grouped 4).toSeq: _*) flatMap { g => foldCells(g, game) }
    val cellsWithCs2: Vector[Cell] = cellsWithCs map { x =>
      val css = homogenize(evalMachine(x.pos, game.sqrtn) traverse cellsWithCs) map {_ exec x.cs}
      x.copy(cs = (css find {_.size == 1}) | x.cs)
    }
    val solveCells = cellsWithCs2 map { cell =>
      if (cell.cs.size == 1) cell.copy(value = cell.cs(0).some, cs = Vector())
      else cell
    }
    game.copy(cells = nonEmptyCells ++ solveCells)
  }
  def cellMachine(pos: (Int, Int), n: Int) =
    sequence(horizontalMachine(pos) :: verticalMachine(pos) :: groupMachine(pos, n) :: AppFunc.HNil)    
  def horizontalMachine(pos: (Int, Int)) =
    buildMachine { cell: Cell => pos._2 == cell.pos._2 && cell.value.isDefined }
  def verticalMachine(pos: (Int, Int)) =
    buildMachine { cell: Cell => pos._1 == cell.pos._1 && cell.value.isDefined }
  def groupMachine(pos: (Int, Int), n: Int) =
    buildMachine { cell: Cell =>
      ((pos._1 - 1) / n == (cell.pos._1 - 1) / n) &&
      ((pos._2 - 1) / n == (cell.pos._2 - 1) / n) &&
      cell.value.isDefined
    }
  def buildMachine(predicate: Cell => Boolean) = AppFuncU { cell: Cell =>
    for {
      xs <- get[Vector[Int]]
      _  <- put(if (predicate(cell)) xs filter {_ != cell.value.get} 
                else xs)
    } yield ()
  }
  def evalMachine(pos: (Int, Int), n: Int) =
    horizEvalMachine(pos) :: vertEvalMachine(pos) :: groupEvalMachine(pos, n) :: AppFunc.HNil
  def horizEvalMachine(pos: (Int, Int)) =
    buildEvalMachine { cell: Cell => (pos != cell.pos) && (pos._2 == cell.pos._2) }
  def vertEvalMachine(pos: (Int, Int)) =
    buildEvalMachine { cell: Cell => (pos != cell.pos) && (pos._1 == cell.pos._1) }
  def groupEvalMachine(pos: (Int, Int), n: Int) =
    buildEvalMachine { cell: Cell =>
      (pos != cell.pos) &&
      ((pos._1 - 1) / n == (cell.pos._1 - 1) / n) &&
      ((pos._2 - 1) / n == (cell.pos._2 - 1) / n)
    }
  def buildEvalMachine(predicate: Cell => Boolean) = AppFuncU { cell: Cell =>
    for {
      xs <- get[Vector[Int]]
      _  <- put(if (predicate(cell)) xs diff cell.cs
                else xs)
    } yield ()
  }
  def sequence[M[_]: Applicative, T <: TCList, B](g: HListFunc[TCCons[M, T], Applicative, Cell, B]) =
    new Func[M, Applicative, Cell, List[B]] {
      def runA(c: Cell): M[List[B]] = {
        val xs = g.runA(c)
        val list: List[M[B]] = xs.fold[Id, List[M[B]], Homogenize[M[B]]](new Homogenize)
        list.sequence
      }
      def F = Applicative[M]
      def TC = g.TC
    }
  def homogenize[M[_]: Applicative, T <: TCList, B](g: HListFunc[TCCons[M, T], Applicative, Cell, B]) =
    new Func[({type λ[α] = List[M[α]]})#λ, Applicative, Cell, B] {  
      def runA(c: Cell): List[M[B]] = {
        val xs = g.runA(c)
        xs.fold[Id, List[M[B]], Homogenize[M[B]]](new Homogenize)
      }
      def F = (Applicative[List] <<: Applicative[M] <<: TC.idCompose).instance
      def TC = g.TC
    }
  class Homogenize[T] extends HFold[Id, List[T]] {
    type Init = List[T]
    def init = Nil
    type Apply[E, A <: List[T]] = List[T]
    def apply[E, A <: List[T]](elem: E, acc: A) =
      (elem match {
        case x: T => x
      }) :: acc
  }
}
