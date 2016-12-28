package org.goodmath.apog.view

import org.goodmath.apog.rope.{Rope, RopeBuffer}
import org.scalatest._

class ScreenGridSuite extends FunSuite {

  test("empty grid") {
    val s = new SimpleScreenGrid(4, 10)
    assert((
       "[____________________]\n" +
       "[____________________]\n" +
       "[____________________]\n" +
       "[____________________]\n") == s.inspect)
  }

  test("non-empty grid at (0, 0)") {
    val s = new SimpleScreenGrid(4, 10)
    val r = Rope.create("1111111111111\n")
      .concat(Rope.create("2222222222222\n"))
      .concat(Rope.create("3333333333333\n"))
      .concat(Rope.create("4444444444444\n"))
      .concat(Rope.create("5555555555555\n"))
      .concat(Rope.create("6666666666666\n"))
      .concat(Rope.create("7777777777777\n"))
    val b = new RopeBuffer(r)
    val controller = new ScreenGridController(s, b)
    controller.render(0, 0)
    assert((
      "[ 1 1 1 1 1 1 1 1 1 1]\n" +
        "[ 2 2 2 2 2 2 2 2 2 2]\n" +
        "[ 3 3 3 3 3 3 3 3 3 3]\n" +
        "[ 4 4 4 4 4 4 4 4 4 4]\n") == s.inspect)
  }

  test("non-empty grid at other coords") {
    val s = new SimpleScreenGrid(4, 10)
    val r = Rope.create("A bunch of text\n")
      .concat(Rope.create("arranged in lines of more than 10 characters\n"))
      .concat(Rope.create("or less\n"))
      .concat(Rope.create("so that\n"))
      .concat(Rope.create("we can see what happens\n"))
      .concat(Rope.create("when we render it at different positions\n"))
    val b = new RopeBuffer(r)
    val controller = new ScreenGridController(s, b)
    controller.render(0, 0)
    assert((
       "[ A   b u n c h   o f]\n" +
       "[ a r r a n g e d   i]\n" +
       "[ o r   l e s s^n____]\n" +
       "[ s o   t h a t^n____]\n") == s.inspect)

    controller.render(0, 5)
    assert((
      "[ c h   o f   t e x t]\n" +
        "[ g e d   i n   l i n]\n" +
        "[ s s^n______________]\n" +
        "[ a t^n______________]\n") == s.inspect)

    controller.render(2, 4)
    assert((
      "[ e s s^n____________]\n" +
        "[ h a t^n____________]\n" +
        "[ a n   s e e   w h a]\n" +
        "[   w e   r e n d e r]\n") == s.inspect)

    controller.render(4, 2)
    assert((
      "[   c a n   s e e   w]\n" +
        "[ e n   w e   r e n d]\n" +
        "[____________________]\n" +
        "[____________________]\n") == s.inspect)
  }


}
