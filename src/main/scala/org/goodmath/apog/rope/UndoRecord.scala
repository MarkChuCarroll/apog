package org.goodmath.apog.rope

import scala.collection.mutable.MutableList

abstract class UndoRecord(val buffer: RopeBuffer) {
  def execute(): Unit

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
  def concat(next: UndoRecord): UndoRecord = {
    val result = CompoundUndo(buffer)
    result.addUndo(this)
    result.addUndo(next)
    result
  }
}

case class NullUndo(b: RopeBuffer) extends UndoRecord(b) {
  override def execute(): Unit = ()

  override def concat(next: UndoRecord): UndoRecord = next
}


case class CompoundUndo(b: RopeBuffer) extends UndoRecord(b) {
  val steps: scala.collection.mutable.MutableList[UndoRecord] =
    new scala.collection.mutable.MutableList[UndoRecord]()

  def addUndo(u: UndoRecord): Unit = {
    steps += u
  }

  override def concat(next: UndoRecord): UndoRecord = {
    next match {
      case cu: CompoundUndo =>
        cu.steps.map { step => steps += step }
      case _ =>
        addUndo(next)
    }
    this
  }

  override def execute(): Unit = {
    steps.foreach { step => step.execute() }
  }

}

case class UndoCursorMove(b: RopeBuffer, origPos: Int) extends UndoRecord(b) {
  override def execute(): Unit = {
    buffer.moveCursorTo(origPos)
  }
}

case class UndoStepCursorForwards(b: RopeBuffer) extends UndoRecord(b) {
  override def execute(): Unit = {
    b.stepCursorBackwards()
  }
}

case class UndoStepCursorBackwards(b: RopeBuffer) extends UndoRecord(b) {
  override def execute(): Unit = {
    b.stepCursorForwards()
  }

}

case class UndoInsert(b: RopeBuffer, pos: Int, length: Int) extends UndoRecord(b) {
  override def execute(): Unit = {
    b.moveCursorTo(pos)
    b.delete(length)
  }

}

case class UndoDelete(b: RopeBuffer, pos: Int, deletedText: String) extends UndoRecord(b) {
  override def execute(): Unit = {
    b.moveCursorTo(pos)
    b.insert(deletedText)
  }
}