package org.goodmath.apog.rope

import scala.util.{Try, Success, Failure}

class BufferException(val buf: RopeBuffer) extends Exception

case class BadPosition(b: RopeBuffer, pos: Int) extends BufferException(b)

trait Context {
  def buffer: RopeBuffer
  // the cursor can be either a single point, or a range.
  def selection: Either[Int, (Int, Int)]
  def vars: Map[String, String]
}

trait BufferOperation[ResultType] {
  def name: String
  def execute(context: Context, str: String, num: Int): Try[ResultType]
}

class RopeBuffer(initialContents: Rope) {
  var contents: Rope = initialContents
  var cursorPosition: Int = 0

  def moveCursorTo(pos: Int): Try[Unit] = {
    if (pos < 0 || pos > contents.length) {
      Failure(BadPosition(this, pos))
    } else {
      cursorPosition = pos
      Success()
    }
  }

  def stepCursorForwards(): Try[Unit] = {
    if (cursorPosition + 1  > contents.length) {
      Failure(BadPosition(this, cursorPosition+1))
    } else {
      cursorPosition += 1
      Success()
    }
  }

  def stepCursorBackwards(): Try[Unit] = {
    if (cursorPosition - 1 < 0) {
      Failure(BadPosition(this, cursorPosition - 1))
    } else {
      cursorPosition -= 1
      Success()
    }
  }

  def insertAt(s: String, pos: Int): Try[Rope] = {
    contents.insertAt(Rope.create(s), pos) match {
      case Some(newRope) =>
        contents = newRope
        Success(newRope)
      case None =>
        Failure(BadPosition(this, pos))
    }
  }
  def charAtCoords(row: Int, column: Int): Option[Char]  = contents.charAtCoord(row, column)

}
