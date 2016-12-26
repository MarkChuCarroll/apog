package org.goodmath.apog.rope

import scala.collection.mutable.ArrayBuffer

/**
  * A rope is a representation for a sequence of characters for a text editor.
  * It's a binary tree of text, with the actual text in the leaves.
  */
abstract class Rope {
  /**
    * The small rope size: if you concatenate two strings whose combined
    * length is less than SMALL_ROPE, then the strings will be copied and
    * merged into a single leaf node, instead of creating an internal node.
    */
  val SMALL_ROPE = 20

  /**
    * Concatenate this rope with another.
    * @param r the rope to add.
    * @return the new rope.
    */
  def concat(r: Rope): Rope = {
    if (length + r.length < SMALL_ROPE) {
      new LeafNode(toString + r.toString)
    } else if (depth <= 4*r.depth && r.depth <= 4*depth) {
      // If the new rope would reasonably balanced, then go ahead and created
      // an internal node.
      new InternalNode(this, r)
    } else {
      // If the new rope would be badly out of balance, then start from scratch with
      // a balanced tree.
      Rope.create(this.toString + r.toString)
    }
  }

  /**
    * Split a rope at a position. The original rope will be unmodified.
    * @param pos the position of the split
    * @return an optional pair of the before split and after split ropes; None if the
    *         position comes after the end of the rope.
    */
  def split(pos: Int): Option[(Rope, Rope)]

  /**
    * Get the character at a position.
    * @param pos the position to insert at.
    * @return the character, or None if the position is after the end of the rope.
    */
  def charAt(pos: Int): Option[Char]

  /**
    * Insert a chunk of text into a rope.
    * @param r a rope containing the new text.
    * @param pos the position of the insert.
    * @return a new rope with the text inserted, or None if the position was invalid.
    */
  def insertAt(r: Rope, pos: Int): Option[Rope] = {
    split(pos).map { case (before, after) =>
      before.concat(r).concat(after)
    }
  }

  /**
    * Insert a character into the string. If it's at the end of a leaf, then the insert
    * should happen in place.
    * @param c the character to insert.
    * @param pos the position to insert it at.
    * @return An option wrapping the updated rope. This could be either a new rope (if the update couldn't
    *         be done in place), or the same rope (if it could happen in-place). If the position is invalid,
    *         then return None.
    */
  def insertChar(c: Char, pos: Int): Option[Rope]

  /**
    * Remove a chunk of text from a rope.
    * @param start the start of the region to remove.
    * @param end the end of the region to remove.
    * @return a pair containing (cut text, rope with the cut text removed),
    *         or None if either position was invalid.
    */
  def deleteRange(start: Int, end: Int): Option[(Rope, Rope)] = {
    if (end <= start) {
      None
    } else {
      split(start).flatMap {
        case (before, rest) =>
          rest.split(end - start).map {
            case (cut, after) => (cut, before.concat(after))
          }
      }
    }
  }
  /**
    * Copy a range of text from a rope.
    * @param start the start of the region to copy.
    * @param end the end of the region to copy.
    * @return the copied text, or None if either position was invalid.
    */
  def getRange(start: Int, end: Int): Option[Rope]

  /**
    * Get the number of characters in a rope.
    */
  def length: Int

  /**
    * Get the number of newlines in a rope.
    */
  def numberOfNewlines: Int

  /**
    * Get the tree-depth of a rope.
    */
  def depth: Int

  /**
    * Get the position of the first character of a line.
    * @param line the line number
    * @return the position, or None if the position is invalid.
    */
  def positionOfLine(line: Int): Option[Int]

  def positionOfCoord(line: Int, col: Int): Option[Int] = {
    positionOfLine(line).flatMap { startOfLine =>
       lengthOfLine(line).flatMap { lineLength =>
         if (col < lineLength)  {
           Some(startOfLine + col)
         } else {
           None
         }
       }
    }
  }

  def charAtCoord(line: Int, column: Int): Option[Char] = {
    positionOfCoord(line, column).flatMap { pos =>
      charAt(pos)
    }
  }

  /**
    * Get the length of a line, including the newline.
    * @param line the line number
    * @return the length of the line, or None if the line number is invalid.
    */
  def lengthOfLine(line: Int): Option[Int] = {
    // TODO: this doesn't work for the last line. And it includes the newline. Which? Maybe?
    positionOfLine(line).flatMap(first =>
      positionOfLine(line + 1).map(second => second - first)
    )
  }

  /**
    * Get the (0-based) line number containing a position.
    * @param pos the position.
    * @return the line number containing the position, or None.
    */
  def lineOfPosition(pos: Int): Option[Int]

  /**
    * Generate a string describing the rope for testing/debugging.
    */
  def inspect: String
}

object Rope {
  /**
    * Big rope is the maximum size of a rope leaf node before the node will be split.
    */
  val BIG_ROPE = 100

  /**
    * Create a rope.
    * @param s a string containing the text to put in the rope.
    * @return a new rope.
    */
  def create(s: String): Rope = {
    if (s.length > BIG_ROPE) {
      val mid = s.length / 2
      Rope.create(s.substring(0, mid)).concat(Rope.create(s.substring(mid)))
    }
    new LeafNode(s)
  }
}

class InternalNode(val left: Rope, val right: Rope) extends Rope {
  override def length: Int = left.length + right.length

  override def numberOfNewlines: Int = left.numberOfNewlines + right.numberOfNewlines

  override def depth: Int = math.max(left.depth, right.depth) + 1

  def charAt(pos: Int): Option[Char] = {
    if (pos < left.length) {
      left.charAt(pos)
    } else {
      right.charAt(pos - left.length)
    }
  }

  override def split(pos: Int): Option[(Rope, Rope)] = {
    if (pos < left.length) {
      left.split(pos).map { case (before, after) =>
        (before, after.concat(right))
      }
    } else {
      right.split(pos - left.length).map { case (before, after) =>
        (left.concat(before), after)
      }
    }
  }

  def getRange(start: Int, end: Int): Option[Rope] = {
    split(start).flatMap { case (_, remainder) =>
      remainder.split(end - start).map { case (cut, _) => cut }
    }
  }

  override def inspect: String = {
    "(I: " + left.inspect + ", " + right.inspect + ")"
  }

  override def toString: String = left.toString + right.toString

  override def positionOfLine(line: Int): Option[Int] = {
    if (line == 0) {
      Some(0)
    } else if (line <= left.numberOfNewlines) {
      left.positionOfLine(line)
    } else {
      right.positionOfLine(line - left.numberOfNewlines).map(pos => pos + left.length)
    }
  }

  override def lineOfPosition(pos: Int): Option[Int] = {
    if (pos < left.length) {
      left.lineOfPosition(pos)
    } else {
      right.lineOfPosition(pos - left.length).map(l => left.numberOfNewlines + l)
    }
  }

  def insertChar(c: Char, pos: Int): Option[Rope] = {
    if (pos <= left.length) {
      left.insertChar(c, pos).map { newLeft =>
        if (newLeft == left) {
          // If we were able to insert in-place, then we just return this.
          this
        } else {
          // If we couldn't insert in-place, then we need to update.
          newLeft.concat(right)
        }
      }
    } else {
      right.insertChar(c, pos - left.length).map { newRight =>
        if (newRight == right) {
          this
        } else {
          left.concat(newRight)
        }
      }
    }
  }
}

class LeafNode(val contentStr: String) extends Rope {
  val contents = new ArrayBuffer[Char](contentStr.length * 2)
  contentStr.foreach { c => contents.append(c) }

  override def length: Int = contents.length()

  override def numberOfNewlines: Int = contents.count(c => c == '\n')

  override val depth: Int = 1

  override def charAt(pos: Int): Option[Char] = {
    if (pos < length) {
      Some(contents.charAt(pos))
    } else {
      None
    }

  }

  def split(pos: Int): Option[(Rope, Rope)] = {
    if (pos > length) {
      None
    } else {
      Some((Rope.create(contents.subSequence(0, pos).toString),
            Rope.create(contents.subSequence(pos, length).toString)))
    }
  }

  def getRange(start: Int, end: Int): Option[Rope] = {
    if (start < 0 || start >= end || end >= length) {
      None
    } else {
      Some(Rope.create(contents.subSequence(start, end).toString))
    }
  }

  override def inspect: String = "(L: '" + contents.mkString + "')"

  override def toString: String = contents.mkString

  override def positionOfLine(line: Int): Option[Int] = {
    if (line == 0) {
      Some(0)
    } else if (line > numberOfNewlines) {
      None
    } else {
      var nls = 0
      contents.indices.foreach { i =>
        if (contents.charAt(i) == '\n') {
          nls += 1
        }
        if (nls == line) {
          return Some(i + 1)
        }
      }
      None
    }
  }

  override def lineOfPosition(pos: Int): Option[Int] = {
    if (pos > length) { None }
    else {
      split(pos).map { case (before, _) => before.numberOfNewlines }
    }
  }

  override def insertChar(c: Char, pos: Int): Option[Rope] = {
    if (pos > length) {
      None
    } else if (pos == length) {
      contents.append(c)
      Some(this)
    } else {
      split(pos).flatMap { case (before, after) =>
        before.insertChar(c, pos).map { n => n.concat(after)}
      }
    }
  }
}

