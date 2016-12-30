package org.goodmath.apog.rope

import org.scalatest._

class RopeBufferSuite extends FunSuite with BeforeAndAfter {

  var rb: RopeBuffer = _

  before {
    val r = Rope.create("1111111111\n")
      .concat(Rope.create("2222222222\n"))
      .concat(Rope.create("3333333333\n"))
      .concat(Rope.create("4444444444\n"))
      .concat(Rope.create("5555555555\n"))
      .concat(Rope.create("6666666666\n"))
      .concat(Rope.create("7777777777\n"))
    rb = new RopeBuffer(r)
  }


  test("A rope buffer can set marks") {
    rb.setMarkAt("one", 8)
    rb.setMarkAt("two", 28)
    rb.setMarkAt("three", 56)
    assert(8 == rb.getMark("one").get._1.intValue.get)
    assert(28 == rb.getMark("two").get._1.intValue.get)
    assert(56 == rb.getMark("three").get._1.intValue.get)
  }

  test("Rope buffer marks should move with inserts") {
    rb.setMarkAt("one", 8)
    rb.setMarkAt("two", 28)
    rb.setMarkAt("three", 56)
    rb.insertAt("some text", 17)
    assert(8 == rb.getMark("one").get._1.intValue.get)
    assert(37 == rb.getMark("two").get._1.intValue.get)
    assert(65 == rb.getMark("three").get._1.intValue.get)
    assert("1111111111\n222222some text2222\n3333333333\n" +
      "4444444444\n5555555555\n6666666666\n7777777777\n" == rb.contents.toString)

  }

  test("Rope buffer marks should move for deletes") {
    rb.setMarkAt("one", 8)
    rb.setMarkAt("two", 28)
    rb.setMarkAt("three", 56)
    rb.deleteRange(26, 32)
    assert(8 == rb.getMark("one").get._1.intValue.get)
    assert(26 == rb.getMark("two").get._1.intValue.get)
    assert(50 == rb.getMark("three").get._1.intValue.get)
    assert("1111111111\n2222222222\n3333\n4444444444\n" +
      "5555555555\n6666666666\n7777777777\n" == rb.contents.toString)
  }

  test("We can move the cursor around and do edits at the cursor point") {
    rb.moveCursorTo(3)
    assert(3 == rb.cursorPosition)
    rb.insert("BOO!")
    assert(7 == rb.cursorPosition)
    rb.moveCursorTo(27)
    rb.insert("YIKES!")
    assert(33 == rb.cursorPosition)
    rb.stepCursorBackwards()
    rb.stepCursorBackwards()
    rb.stepCursorBackwards()
    assert(30 == rb.cursorPosition)
    rb.moveCursorTo(1)
    assert(1 == rb.cursorPosition)
    rb.stepCursorBackwards()
    rb.stepCursorBackwards()
    rb.stepCursorBackwards()
    assert(0 == rb.cursorPosition)
  }

  test("We can use undo") {
    val before = rb.contents.toString
    rb.moveCursorTo(27)
    val firstOp = rb.delete(13)
    assert(firstOp.isSuccess)
    val firstString = rb.contents.toString
    firstOp.foreach{ case (_, undo) => rb.pushUndo(undo) }
    rb.moveCursorTo(14)
    val secondOp = rb.insert("hello there")
    assert(secondOp.isSuccess)
    val secondString = rb.contents.toString
    secondOp.foreach{ case (_, undo) => rb.pushUndo(undo) }
    // Both the first and second op should have modified the buffer contents.
    assert(before != firstString)
    assert(firstString != secondString)

    rb.undo()
    assert(firstString == rb.contents.toString)
    rb.undo()
    assert(before == rb.contents.toString)
  }
}
