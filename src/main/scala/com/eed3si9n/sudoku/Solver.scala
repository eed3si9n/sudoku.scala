package com.eed3si9n.sudoku

import scalaz._
import Scalaz._
import typelevel._

object Solver {
  def solve(game: Game) {


  }
  def runOnce(game: Game): Game = {
    val (nonEmptyCells, emptyCells) = game.cells partition {_.value.isDefined}
    val solveCells = emptyCells map { cell =>
      val candidates = (cellMachine(cell.pos, game.sqrtn) traverse game.cells) exec game.allValues
      if (candidates.size == 1) cell.copy(value = candidates(0).some, cs = Vector())
      else cell.copy(value = none, cs = candidates)
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
      val updated = if (predicate(cell)) xs filter {_ != cell.value.get} 
                    else xs
      _  <- put(updated)
    } yield updated
  }
  def sequence[M[_]: Applicative, T <: TCList](g: HListFunc[TCCons[M, T], Applicative, Cell, Vector[Int]]) =
    new Func[M, Applicative, Cell, List[Vector[Int]]] {
      def runA(c: Cell): M[List[Vector[Int]]] = {
        val xs = g.runA(c)
        val list: List[M[Vector[Int]]] = xs.fold[Id, List[M[Vector[Int]]], Homogenize[M[Vector[Int]]]](new Homogenize)
        list.sequence
      }
      def F = Applicative[M]
      def TC = g.TC
    }  
  def homogenize[M[_]: Applicative, T <: TCList](g: HListFunc[TCCons[M, T], Applicative, Cell, Vector[Int]]) =
    new Func[({type λ[α] = List[M[α]]})#λ, Applicative, Cell, Vector[Int]] {  
      def runA(c: Cell): List[M[Vector[Int]]] = {
        val xs = g.runA(c)
        xs.fold[Id, List[M[Vector[Int]]], Homogenize[M[Vector[Int]]]](new Homogenize)
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
