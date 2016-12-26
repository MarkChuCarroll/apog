package org.goodmath.apog.view

import org.goodmath.apog.rope.RopeBuffer

/**
  * Created by markcc on 12/24/16.
  */
object ScreenGridController {
  val NORMAL_TEXT = 0
  val CONTROL = 1
  val EMPTY = 10
}

class ScreenGridController(val grid: ScreenGrid, val contents: RopeBuffer) {
  def render(firstRow: Int, firstColumn: Int): Unit = {
      for {
        row <- 0 until grid.numLines
        column <- 0 until grid.numColumns
      } {
        contents.charAtCoords(firstRow + row, firstColumn + column) match {
          case Some('\n') => grid.setCharacter(row, column, 'n', ScreenGridController.CONTROL)
          case Some(c) => grid.setCharacter(row, column, c, ScreenGridController.NORMAL_TEXT)
          case None => grid.setCharacter(row, column, ' ', ScreenGridController.EMPTY)
        }
      }
  }

}
