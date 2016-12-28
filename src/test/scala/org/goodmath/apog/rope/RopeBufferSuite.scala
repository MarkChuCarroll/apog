package org.goodmath.apog.rope

import org.scalatest._

class RopeBufferSuite extends  FunSuite {

  def createRectNumberRope: Rope =
    Rope.create("1111111111\n")
      .concat(Rope.create("2222222222\n"))
      .concat(Rope.create("3333333333\n"))
      .concat(Rope.create("4444444444\n"))
      .concat(Rope.create("5555555555\n"))
      .concat(Rope.create("6666666666\n"))
      .concat(Rope.create("7777777777\n"))


  test("A rope buffer can set marks") {
    val r = createRectNumberRope
    val rb = new RopeBuffer(r)
    rb.setMarkAt("one", 8)
    rb.setMarkAt("two", 28)
    rb.setMarkAt("three", 56)
    assert(8 == rb.getMark("one").get)
    assert(28 == rb.getMark("two").get)
    assert(56 == rb.getMark("three").get)
  }

  test("Rope buffer marks should move with inserts") {
    val r = createRectNumberRope
    val rb = new RopeBuffer(r)
    rb.setMarkAt("one", 8)
    rb.setMarkAt("two", 28)
    rb.setMarkAt("three", 56)
    rb.insertAt("some text", 17)
    assert(8 == rb.getMark("one").get)
    assert(37 == rb.getMark("two").get)
    assert(65 == rb.getMark("three").get)
    assert("1111111111\n222222some text2222\n3333333333\n" +
      "4444444444\n5555555555\n6666666666\n7777777777\n" == rb.contents.toString)

  }

  test("Rope buffer marks should move for deletes") {
    val r = createRectNumberRope
    val rb = new RopeBuffer(r)
    rb.setMarkAt("one", 8)
    rb.setMarkAt("two", 28)
    rb.setMarkAt("three", 56)
    rb.deleteRange(26, 32)
    assert(8 == rb.getMark("one").get)
    assert(26 == rb.getMark("two").get)
    assert(50 == rb.getMark("three").get)
    assert("1111111111\n2222222222\n3333\n4444444444\n" +
      "5555555555\n6666666666\n7777777777\n" == rb.contents.toString)
  }

  test("We can move the cursor around and do edits at the cursor point") {
    val r = createRectNumberRope
    val rb = new RopeBuffer(r)
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
}
