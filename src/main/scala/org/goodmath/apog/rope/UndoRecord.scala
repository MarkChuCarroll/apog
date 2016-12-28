package org.goodmath.apog.rope

import scala.collection.mutable.MutableList

trait UndoRecord {
  def buffer: RopeBuffer
  def execute(): Unit

  /**
    * Return a flag indicating whether or not this undo operation actually does anything.
    */
  def isNoop: Boolean

  /**
    * Buffer operations will combine multiple undo operations into a single
    * unit. This operation takes the current undo operation, and combines it
    * with the undo operation following it.
    * @param next the next undo record
    * @return an undo operation combining this undoable operation with another. The result may
    *         be either an entirely new undo record, or it may be (a modified version of)
    *         one of the input records. For example, an empty undo combined with any
    *         other undo will just return the other undo.
    */
  def concat(next: UndoRecord): UndoRecord
}

case class NullUndo(override val buffer: RopeBuffer) extends UndoRecord {
  override def execute(): Unit = ()

  override def concat(next: UndoRecord): UndoRecord = next

  /**
    * Return a flag indicating whether or not this undo operation actually does anything.
    */
  override def isNoop: Boolean = true
}


class CompoundUndo(val buf: RopeBuffer) extends UndoRecord {
  val steps: MutableList[UndoRecord] = new MutableList[UndoRecord]()

  override def concat(next: UndoRecord): UndoRecord = {
    next match {
      case cu: CompoundUndo =>
        cu.steps.map { step => steps += step }
      case _ =>
        steps += next
    }
    this
  }

  override def buffer: RopeBuffer = buf

  override def execute(): Unit = {
    steps.foreach { step => step.execute() }
  }

  /**
    * Return a flag indicating whether or not this undo operation actually does anything.
    */
  override def isNoop: Boolean = false
}
