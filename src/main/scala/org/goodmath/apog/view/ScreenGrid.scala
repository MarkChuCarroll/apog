package org.goodmath.apog.view

/**
  * Created by markcc on 12/23/16.
  */
trait ScreenGrid {
  def numLines: Int
  def numColumns: Int
  def setCharacter(row: Int, col: Int, c: Char, style: Int)
  def inspect: String
}

object ScreenGrid {
  val NORMAL_TEXT = 0
  val CONTROL = 1
  val EMPTY = 10
}

class SimpleScreenGrid(rows: Int, columns: Int) extends ScreenGrid {
  val grid: Array[Array[(Char, Int)]] = Array.ofDim[(Char, Int)](rows, columns)
  for {
    row <- 0 until numLines
    col <- 0 until numColumns
  } {
    grid(row)(col) = (' ', ScreenGrid.EMPTY)
  }

  override def numLines: Int = rows

  override def numColumns: Int = columns

  override def setCharacter(row: Int, col: Int, c: Char, style: Int): Unit = {
    if (row < numLines && col < numColumns) {
      grid(row)(col) = (c, style)
    }
  }

  override def inspect: String = {
    val s = new StringBuilder()
    grid.foreach {(row: Array[(Char, Int)]) =>
      s.append('[')
      row.foreach { case (c, st) =>
        if (st ==  ScreenGrid.NORMAL_TEXT) {
          s.append(" " + c)
        } else if (st == ScreenGrid.CONTROL) {
          s.append("^" + c)
        } else if (st == ScreenGrid.EMPTY) {
          s.append("__")
        }
      }
      s.append("]\n")
    }
    s.toString
  }
}