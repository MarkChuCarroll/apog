package org.goodmath.apog

/** A rope is a representation for a sequence of characters for a text editor.
  * It's a binary tree of text, with the actual text in the leaves.
  */
abstract class Rope {
  /** The small rope size: if you concatenate two strings whose combined
    * length is less than SMALL_ROPE, then the strings will be copied and
    * merged into a single leaf node, instead of creating an internal node.
    */
  val SMALL_ROPE = 20

  /**
    * Concatenate this rope with another.
    * @param r
    * @return the new rope.
    */
  def concat(r: Rope): Rope = {
    if (length + r.length < SMALL_ROPE) {
      new LeafNode(toString + r.toString)
    } else {
      new InternalNode(this, r)
    }
  }

  /**
    * Split a rope at a position. The original rope will be unmodified.
    * @param pos
    * @return an optional pair of the before split and after split ropes; None if the
    *         position comes after the end of the rope.
    */
  def split(pos: Int): Option[(Rope, Rope)]

  /**
    * Get the character at a position.
    * @param pos
    * @return the character, or None if the position is after the end of the rope.
    */
  def char_at(pos: Int): Option[Char]

  /**
    * Insert a chunk of text into a rope.
    * @param r a rope containing the new text.
    * @param pos the position of the insert.
    * @return a new rope with the text inserted, or None if the position was invalid.
    */
  def insert_at(r: Rope, pos: Int): Option[Rope] = {
    split(pos).map { case (before, after) =>
      before.concat(r).concat(after)
    }
  }

  /**
    * Remove a chunk of text from a rope.
    * @param start the start of the region to remove.
    * @param end the end of the region to remove.
    * @return a pair containing (cut text, rope with the cut text removed),
    *         or None if either position was invalid.
    */
  def delete_range(start: Int, end: Int): Option[(Rope, Rope)] = {
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
  def get_range(start: Int, end: Int): Option[Rope]

  /** Get the number of characters in a rope.
    */
  def length: Int

  /**
    * Get the number of newlines in a rope.
    */
  def number_of_newlines: Int

  /**
    * Get the tree-depth of a rope.
    */
  def depth: Int

  /**
    * Get the position of the first character of a line.
    * @param line the line number
    * @return the position, or None if the position is invalid.
    */
  def position_of_line(line: Int): Option[Int]

  /**
    * Get the length of a line, including the newline.
    * @param line the line number
    * @return the length of the line, or None if the line number is invalid.
    */
  def length_of_line(line: Int): Option[Int] = {
    position_of_line(line).flatMap(first =>
      position_of_line(line + 1).map(second => second - first)
    )
  }

  /**
    * Get the (0-based) line number containing a position.
    * @param pos the position.
    * @return the line number containing the position, or None.
    */
  def line_for_position(pos: Int): Option[Int]

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
  override def length = left.length + right.length
  override def number_of_newlines = left.number_of_newlines + right.number_of_newlines
  override def depth = math.max(left.depth, right.depth) + 1

  def char_at(pos: Int): Option[Char] = {
    if (pos < left.length) {
      left.char_at(pos)
    } else {
      right.char_at(pos - left.length)
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

  def get_range(start: Int, end: Int): Option[Rope] = {
    split(start).flatMap { case (before, remainder) =>
      remainder.split(end - start).map { case (cut, after) => cut }
    }
  }

  override def inspect: String = {
    "(I: " + left.inspect + ", " + right.inspect + ")"
  }

  override def toString: String = left.toString + right.toString

  override def position_of_line(line: Int): Option[Int] = {
    if (line == 0) {
      Some(0)
    } else if (line <= left.number_of_newlines) {
      left.position_of_line(line)
    } else {
      right.position_of_line(line - left.number_of_newlines).map(pos => pos + left.length)
    }
  }

  override def line_for_position(pos: Int): Option[Int] = {
    if (pos < left.length) {
      left.line_for_position(pos)
    } else {
      right.line_for_position(pos - left.length).map(l => left.number_of_newlines + l)
    }
  }
}

class LeafNode(val contents: String) extends Rope {
  override def length = contents.length()
  override def number_of_newlines = contents.count(c => c == '\n')
  override val depth = 1
  override def char_at(pos: Int): Option[Char] = {
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
      Some((Rope.create(contents.substring(0, pos)),
            Rope.create(contents.substring(pos))))
    }
  }

  def get_range(start: Int, end: Int): Option[Rope] = {
    if (start < 0 || start >= end || end >= length) {
      None
    } else {
      Some(Rope.create(contents.substring(start, end)))
    }
  }

  override def inspect: String = "(L: '" + contents + "')"

  override def toString: String = contents

  override def position_of_line(line: Int): Option[Int] = {
    if (line > number_of_newlines) {
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

  override def line_for_position(pos: Int): Option[Int] = {
    if (pos > length) { None }
    else {
      split(pos).map { case (before, _) => before.number_of_newlines }
    }
  }
}

