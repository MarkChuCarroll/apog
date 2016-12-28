package org.goodmath.apog.rope

import scala.collection.mutable.Map
import scala.util.Try

/**
  * An edit operation takes a buffer, and a set of variable bindings.
  * The buffer itself defines its current cursor point/selection. It can make
  * any changes it wants to the buffer, the cursor point, and the selection.
  * It can also modify the variable bindings.
  *
  * The edit operations should batch the undo records for its substeps
  * into a single undo operation, and return the sequence of things needed to
  * undo any buffer modifications in an undo record.
  *
  */
trait BufferOperation[ResultType] {
  def name: String
  def execute(buffer: RopeBuffer, bindings: Map[String, String]): Try[UndoRecord]
}
