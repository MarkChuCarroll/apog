package org.goodmath.apog.language

/**
  * An editor operation is a primitive executable for the apog editor.
  * It takes an editor state, and generates a new editor state. Along the way,
  * it can modify the contents of the active buffer, change the active buffer,
  * create or destroy buffers.
  *
  * Every edit operation returns a Try. If it performs any edit actions on the contents
  * of a buffer, it should insert undo records into the buffer's undo history.
  *
  * An operation can either succeed, producing a value; or it can fail. A given operation
  * invocation can produce success values multiple times; once it fails, it must continue to
  * fail.
  */
class Operation {

}
