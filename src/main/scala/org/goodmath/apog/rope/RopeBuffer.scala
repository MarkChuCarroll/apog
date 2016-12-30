package org.goodmath.apog.rope

import scala.collection.immutable.Stack
import scala.util.{Failure, Success, Try}


class BufferException(val buf: RopeBuffer) extends Exception
case class BadPosition(b: RopeBuffer, pos: Int) extends BufferException(b)
case class BadCoordinates(b: RopeBuffer, line: Int, column: Int) extends BufferException(b)
case class BadRange(b: RopeBuffer, start: Int, end: Int) extends BufferException(b)
case class InvalidMark(b: RopeBuffer, mark: String) extends BufferException(b)
case class BufferValueTypeError(b: RopeBuffer, expected: String, actual: String) extends BufferException(b)

trait BufferOperationValue {
  def valueType: String
  def stringValue: Option[String]
  def intValue: Option[Int]
  def markValue: Option[(String, Int)]
}
case class IntegerValue(intVal: Int) extends BufferOperationValue {
  override def valueType: String = "Integer"

  override def stringValue: Option[String] = Some(intVal.toString)

  override def intValue: Option[Int] = Some(intVal)

  override def markValue: Option[(String, Int)] = None
}
case class StringValue(strVal: String) extends BufferOperationValue {
  override def valueType: String = "String"

  override def stringValue: Option[String] = Some(strVal)

  override def intValue: Option[Int] = {
    try {
      Some(strVal.toInt)
    } catch {
      case _: Exception => None
    }
  }

  override def markValue: Option[(String, Int)] = None
}
case class MarkValue(name: String, pos: Int) extends BufferOperationValue {
  override def valueType: String = "Mark"

  override def stringValue: Option[String] = Some(s"$name=$pos")

  override def intValue: Option[Int] = Some(pos)

  override def markValue: Option[(String, Int)] = Some((name, pos))
}


/**
  * A buffer using a rope to store its contents.
  * Most rope operations return a try value, with success wrapping an undo
  * record that undoes the action, and  failure wrapping an exception describing the reason
  * for failure.
  *
  * Q: How do we handle editor commands that produce a value? Eg, cut, copy, getPosition?
  *
  * @param initialContents the initial contents of the buffer.
  */
class RopeBuffer(initialContents: Rope) {
  var contents: Rope = initialContents
  var cursorPosition: Int = 0
  private var undoStack: List[UndoRecord] = List()
  private var modified: Boolean = false

  def isModified: Boolean = modified

  def setUnmodified(): Unit = {
    modified = false
  }

  def pushUndo(u: UndoRecord): Unit = {
    undoStack = u :: undoStack
  }

  def popUndo(): UndoRecord = {
    if (undoStack.isEmpty) {
      NullUndo(this)
    } else {
      val r = undoStack.head
      undoStack = undoStack.tail
      r
    }
  }

  def undo(): Unit = {
    val u = popUndo()
    u.execute()
  }

  // Marks are named pointers at a position in a buffer.
  // As edits occur, the marks should move to match the edit actions.
  // If the target of a mark is removed, the mark should be moved to the first
  // character before the original mark point that's still part of the document.
  val marks: scala.collection.mutable.Map[String, Int] = new scala.collection.mutable.HashMap[String, Int]

  /**
    * Create a marked location in a buffer. If the mark already exists, then it will be updated
    * to point at the new location.
    * @param name the name of the mark.
    * @param pos the position to mark.
    */
  def setMarkAt(name: String, pos: Int): Try[(BufferOperationValue, UndoRecord)] = {
    marks(name) = pos
    Success((MarkValue(name, pos), NullUndo(this)))
  }

  /**
    * Create a marked location at the current cursor point.
    * If the mark already exists, then it will updated to point at the new location.
    * @param name the name of the mark.
    */
  def setMark(name: String): Try[(BufferOperationValue, UndoRecord)] = {
    setMarkAt(name, cursorPosition)
  }

  /**
    * Retrieve the position stored in a mark.
    * @param name the name of the mark.
    * @return the current position of the mark, or None.
    */
  def getMark(name: String): Try[(BufferOperationValue, UndoRecord)] = {
    marks.get(name) match {
      case Some(pos) => Success(MarkValue(name, pos), NullUndo(this))
      case None => Failure(InvalidMark(this, name))
    }
  }
  /**
    * Remove a mark from a buffer.
    * @param name the name of the mark to remove.
    */
  def removeMark(name: String): Try[(BufferOperationValue, UndoRecord)] = {
    val pos = marks.getOrElse(name, 0)
    marks.remove(name)
    Success((MarkValue(name, pos), NullUndo(this)))
  }

  /**
    * Move the cursor to a position.
    */
  def moveCursorTo(pos: Int): Try[(BufferOperationValue, UndoRecord)] = {
    val currentPos = cursorPosition
    if (pos < 0 || pos > contents.length) {
      Failure(BadPosition(this, pos))
    } else {
      cursorPosition = pos
      Success(IntegerValue(pos), UndoCursorMove(this, currentPos))
    }
  }

  /**
    * Move the cursor to the position of a named mark.
    * @param name the name of the mark.
    */
  def moveCursorToMark(name: String): Try[(BufferOperationValue, UndoRecord)] = {
    getMark(name).flatMap {
      case (MarkValue(_, pos), _) => moveCursorTo(pos)
      case (v, _) => Failure(BufferValueTypeError(this, "Mark", v.valueType))
    }
  }

  /**
    * Move the cursor forward one character.
    */
  def stepCursorForwards(): Try[(BufferOperationValue, UndoRecord)] = {
    if (cursorPosition + 1  > contents.length) {
      Failure(BadPosition(this, cursorPosition+1))
    } else {
      cursorPosition += 1
      Success((IntegerValue(cursorPosition), UndoStepCursorForwards(this)))
    }
  }

  /**
    * Move the cursor back by one position.
    */
  def stepCursorBackwards(): Try[(BufferOperationValue, UndoRecord)] = {
    if (cursorPosition - 1 < 0) {
      Failure(BadPosition(this, cursorPosition - 1))
    } else {
      cursorPosition -= 1
      Success((IntegerValue(cursorPosition), UndoStepCursorBackwards(this)))
    }
  }

  /**
    * Insert a string into the buffer at a position.
    * @param s the string to insert.
    * @param pos the position to insert at.
    */
  def insertAt(s: String, pos: Int): Try[(BufferOperationValue, UndoRecord)] = {
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
        modified = true
        Success(StringValue(s), UndoInsert(this, pos, s.length))
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
  def deleteRange(start: Int, end: Int): Try[(BufferOperationValue, UndoRecord)] = {
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
        modified = true
        Success((StringValue(cut.toString), UndoDelete(this, start, cut.toString)))
      case None => Failure(BadRange(this, start, end))
    }
  }

  /**
    * Copy a range of text from the buffer.
    * @param start the start of the region to copy.
    * @param end the end of the region to copy.
    * @return a Try value wrapping the copied text.
    */
  def copyRange(start: Int, end: Int): Try[(BufferOperationValue, UndoRecord)] = {
    contents.getRange(start, end) match {
      case Some(r) => Success((StringValue(r.toString), NullUndo(this)))
      case None => Failure(BadRange(this, start, end))
    }
  }

  /**
    * Insert a string at the current cursor position.
    * @param s the string to insert.
    * @return a Try value wrapping the updated buffer contents.
    */
  def insert(s: String):  Try[(BufferOperationValue, UndoRecord)] = {
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
  def delete(len: Int): Try[(BufferOperationValue, UndoRecord)] = {
    deleteRange(cursorPosition, cursorPosition + len)
  }

  /**
    * Copy a range of text starting at the cursor point.
    * @param len the length of the range to copy.
    * @return a Try value wrapping the copied text.
    */
  def copy(len: Int): Try[(BufferOperationValue, UndoRecord)] = {
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
  def moveCursorToCoords(line: Int, column: Int): Try[(BufferOperationValue, UndoRecord)] = {
    val orig = cursorPosition
    contents.positionOfCoord(line, column) match {
      case Some(pos) =>
        moveCursorTo(pos)
        Success((IntegerValue(pos), UndoCursorMove(this, orig)))
      case None => Failure(BadCoordinates(this, line, column))
    }
  }
}
