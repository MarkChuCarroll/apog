package org.goodmath.apog.rope

import scala.collection.mutable.{MutableList, HashMap, Map}
import scala.util.{Try, Success, Failure}


class BufferException(val buf: RopeBuffer) extends Exception
case class BadPosition(b: RopeBuffer, pos: Int) extends BufferException(b)
case class BadCoordinates(b: RopeBuffer, line: Int, column: Int) extends BufferException(b)
case class BadRange(b: RopeBuffer, start: Int, end: Int) extends BufferException(b)
case class InvalidMark(b: RopeBuffer, mark: String) extends BufferException(b)


class RopeBuffer(initialContents: Rope) {
  var contents: Rope = initialContents
  var cursorPosition: Int = 0

  // Marks are named pointers at a position in a buffer.
  // As edits occur, the marks should move to match the edit actions.
  // If the target of a mark is removed, the mark should be moved to the first
  // character before the original mark point that's still part of the document.
  val marks: Map[String, Int] = new HashMap[String, Int]

  /**
    * Create a marked location in a buffer. If the mark already exists, then it will be updated
    * to point at the new location.
    * @param name the name of the mark.
    * @param pos the position to mark.
    */
  def setMarkAt(name: String, pos: Int): Unit = {
    marks(name) = pos
  }

  /**
    * Create a marked location at the current cursor point.
    * If the mark already exists, then it will updated to point at the new location.
    * @param name the name of the mark.
    */
  def setMark(name: String): Unit = {
    marks(name) = cursorPosition
  }

  /**
    * Retrieve the position stored in a mark.
    * @param name the name of the mark.
    * @return the current position of the mark, or None.
    */
  def getMark(name: String): Option[Int] = marks.get(name)

  /**
    * Remove a mark from a buffer.
    * @param name the name of the mark to remove.
    */
  def removeMark(name: String): Unit = marks.remove(name)

  /**
    * Move the cursor to a position.
    * @param pos the position to move the cursor to.
    * @return a Try value indicating whether or not the cursor was moved successfully.
    */
  def moveCursorTo(pos: Int): Try[Unit] = {
    if (pos < 0 || pos > contents.length) {
      Failure(BadPosition(this, pos))
    } else {
      cursorPosition = pos
      Success(NullUndo(this))
    }
  }

  /**
    * Move the cursor to the position of a named mark.
    * @param name the name of the mark.
    * @return a success object wrapping the new cursor position, or a failure object wrapping
    *         the error.
    */
  def moveCursorToMark(name: String): Try[Int] = {
    getMark(name) match {
      case Some(pos) =>
        moveCursorTo(pos)
        Success(pos)
      case None =>
        Failure(InvalidMark(this, name))
    }
  }

  /**
    * Move the cursor forward one character.
    * @return a Try value.
    */
  def stepCursorForwards(): Try[Unit] = {
    if (cursorPosition + 1  > contents.length) {
      Failure(BadPosition(this, cursorPosition+1))
    } else {
      cursorPosition += 1
      Success()
    }
  }

  /**
    * Move the cursor back by one position.
    * @return a Try value.
    */
  def stepCursorBackwards(): Try[Unit] = {
    if (cursorPosition - 1 < 0) {
      Failure(BadPosition(this, cursorPosition - 1))
    } else {
      cursorPosition -= 1
      Success()
    }
  }

  /**
    * Insert a string into the buffer at a position.
    * @param s the string to insert.
    * @param pos the position to insert at.
    * @return A Try value wrapping the updated buffer contents.
    */
  def insertAt(s: String, pos: Int): Try[Rope] = {
    // Update marks: anything that comes after the point of insert needs to get shifted.
    for {
      m <- marks.keys
    } {
      val markPos = marks.getOrElse(m, -1)
      if (markPos >= pos) {
        marks(m) = marks(m) + s.length
      }
    }
    contents.insertAt(Rope.create(s), pos) match {
      case Some(newRope) =>
        contents = newRope
        Success(newRope)
      case None =>
        Failure(BadPosition(this, pos))
    }
  }

  /**
    * Delete a range of text from the buffer.
    * @param start the start of the range to delete.
    * @param end the end of the range to delete.
    * @return a Try value wrapping the deleted text.
    */
  def deleteRange(start: Int, end: Int): Try[Rope] = {
    contents.deleteRange(start, end) match {
      case Some((cut, newRope)) =>
        contents = newRope
        for {
          m <- marks.keys
        } {
          val markPos = marks.getOrElse(m, -1)
          // If the mark comes after the start of the delete:
          if (markPos >= start) {
            if (markPos <= end) {
              // if it's in the deleted region, then move it to right before he first removed char.
              marks(m) = start
            } else {
              // otherwise, shift it backwards by the cut size.
              marks(m) = marks(m) - (end - start)
            }
          }
        }
        Success(cut)
      case None => Failure(BadRange(this, start, end))
    }
  }

  /**
    * Copy a range of text from the buffer.
    * @param start the start of the region to copy.
    * @param end the end of the region to copy.
    * @return a Try value wrapping the copied text.
    */
  def copyRange(start: Int, end: Int): Try[Rope] = {
    contents.getRange(start, end) match {
      case Some(r) => Success(r)
      case None => Failure(BadRange(this, start, end))
    }
  }

  /**
    * Insert a string at the current cursor position.
    * @param s the string to insert.
    * @return a Try value wrapping the updated buffer contents.
    */
  def insert(s: String):  Try[Rope] = {
    insertAt(s, cursorPosition).map { result =>
      cursorPosition += s.length
      result
    }
  }

  /**
    * Delete a range of text starting at the cursor.
    * @param len the length of the region to delete.
    * @return a Try value wrapping the deleted text.
    */
  def delete(len: Int): Try[Rope] = {
    deleteRange(cursorPosition, cursorPosition + len)
  }

  /**
    * Copy a range of text starting at the cursor point.
    * @param len the length of the range to copy.
    * @return a Try value wrapping the copied text.
    */
  def copy(len: Int): Try[Rope] = {
    copyRange(cursorPosition, cursorPosition + len)
  }

  /**
    * Get the character at a row/column position.
    * @param row the row
    * @param column the column
    * @return the character, or None.
    */
  def charAtCoords(row: Int, column: Int): Option[Char]  = contents.charAtCoord(row, column)

  /**
    * Move cursor to a (line, column) coordinate.
    * @param line the line number
    * @param column the column
    * @return A success object wrapping the position of the cursor, or a Failure.
    */
  def moveCursorToCoords(line: Int, column: Int): Try[Int] = {
    contents.positionOfCoord(line, column) match {
      case Some(pos) =>
        moveCursorTo(pos)
        Success(pos)
      case None => Failure(BadCoordinates(this, line, column))
    }
  }
}
