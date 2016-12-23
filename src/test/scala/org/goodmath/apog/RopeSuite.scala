package org.goodmath.apog

import org.scalatest._

class RopeSuite extends FunSuite {
  test("short ropes should merge on concat") {
    val r = Rope.create("hello ")
    val s = Rope.create("world")
    val concat = r.concat(s)
    assert(concat.inspect == "(L: 'hello world')")
    assert(concat.toString == "hello world")
  }


  test("longer ropes should concatenate as trees") {
        val r = Rope.create("123456789011223344556677889900")
        val s = Rope.create("abcdefghijklmnopqrstuvwxyz")
        val t = r.concat(s)
        assert("(I: (L: '123456789011223344556677889900'), (L: 'abcdefghijklmnopqrstuvwxyz'))" ==
          t.inspect)
  }

  def buildTestString(c: Char, lineLength: Int, size: Int): String = {
    val sb = new StringBuilder()
    (0 until size).foreach { i =>
      if (i != 0 && i %lineLength == 0) {
        sb.append('\n')
      }
      sb.append(c)
    }
    sb.append('\n')
    sb.toString()
  }

  test("chars should be retrievable by position") {
    val a = Rope.create(buildTestString('a', 23, 50))
    val b = Rope.create(buildTestString('b', 17, 50))
    val c = Rope.create(buildTestString('c', 19, 50))
    val d = Rope.create(buildTestString('d', 13, 50))
    val e = Rope.create(buildTestString('e', 29, 50))
    val f = Rope.create(buildTestString('f', 31, 50))
    val g = Rope.create(buildTestString('g', 37, 50))
    val all = a.concat(b).concat(c).concat(d).concat(e).concat(f).concat(g)
    assert('a' == all.char_at(0).get)
    assert('a' == all.char_at(1).get)
    assert('a' ==  all.char_at(8).get)
    assert('a' ==  all.char_at(8).get)
    assert('a' ==  all.char_at(10).get)
    assert('a' ==  all.char_at(12).get)
    assert('a' ==  all.char_at(16).get)
    assert('a' ==  all.char_at(19).get)
    assert('a' ==  all.char_at(20).get)
    assert('\n' == all.char_at(23).get)
    assert('a' == all.char_at(26).get)
    assert('a' == all.char_at(31).get)
    assert('\n' == all.char_at(47).get)
    assert('b' ==  all.char_at(55).get)
    assert('b' ==  all.char_at(56).get)
    assert('\n' ==  all.char_at(70).get)
    assert('b' ==  all.char_at(91).get)
    assert('b' ==  all.char_at(100).get)
    assert('c' ==  all.char_at(110).get)
    assert('c' ==  all.char_at(130).get)
    assert('c' ==  all.char_at(140).get)
    assert('c' ==  all.char_at(150).get)
    assert('\n' ==  all.char_at(158).get)
    assert('d' ==  all.char_at(160).get)
    assert('d' ==  all.char_at(170).get)
    assert('d' ==  all.char_at(180).get)
    assert('d' ==  all.char_at(190).get)
    assert('\n' ==  all.char_at(200).get)
    assert('d' ==  all.char_at(210).get)
    assert('e' ==  all.char_at(220).get)
    assert('e' ==  all.char_at(230).get)
    assert('e' ==  all.char_at(240).get)
    assert('e' ==  all.char_at(250).get)
    assert('e' ==  all.char_at(260).get)
    assert('f' ==  all.char_at(270).get)
    assert('f' ==  all.char_at(280).get)
    assert('f' ==  all.char_at(290).get)
    assert('f' ==  all.char_at(300).get)
    assert('f' ==  all.char_at(310).get)
    assert('g' ==  all.char_at(320).get)
    assert('g' ==  all.char_at(330).get)
    assert('g' ==  all.char_at(340).get)
    assert('g' ==  all.char_at(350).get)
    assert('g' ==  all.char_at(360).get)
    assert(all.char_at(1000).isEmpty)
  }

    test("a rope should be splittable into subropes") {
      val a = Rope.create("1111111111")
      val b = Rope.create("2222222222")
      val c = Rope.create("3333333333")
      val d = Rope.create("4444444444")
      val e = Rope.create("5555555555")
      val f = Rope.create("6666666666")
      val g = Rope.create("7777777777")
      val all = a.concat(b).concat(c).concat(d).concat(e).concat(f).concat(g)

      val sp = all.split(21)
      assert(sp.isDefined)
      sp.foreach { case (before, after) =>
        assert("111111111122222222223" == before.toString)
        assert("3333333334444444444555555555566666666667777777777" == after.toString)
        assert("1111111111222222222233333333334444444444555555555566666666667777777777" == all.toString)

        assert("(I: (I: (L: '1111111111'), (L: '2222222222')), (L: '3'))" == before.inspect)
        assert("(I: (I: (I: (L: '3333333334444444444'), (L: '5555555555')), (L: '6666666666')), (L: '7777777777'))" ==
          after.inspect)
        assert(all.toString == before.concat(after).toString)
      }
    }

  test("You should be able to insert into the middle of a rope") {
    val a = Rope.create("1111111111")
    val b = Rope.create("2222222222")
    val c = Rope.create("3333333333")
    val d = Rope.create("4444444444")
    val e = Rope.create("5555555555")
    val f = Rope.create("6666666666")
    val g = Rope.create("7777777777")
    val all = a.concat(b).concat(c).concat(d).concat(e).concat(f).concat(g)
    val res = all.insert_at(Rope.create("abc"), 13)
    assert(res.isDefined)
    res.foreach { newr =>
      assert("1111111111222abc222222233333333334444444444555555555566666666667777777777" == newr.toString)
      assert("1111111111222222222233333333334444444444555555555566666666667777777777" == all.toString)
    }
  }

  test("Multiple splits shouldn't interfere with each other") {
    val a = Rope.create("1111111111")
    val b = Rope.create("222222222222222")
    val c = Rope.create("33333333")
    val d = Rope.create("4444444444444")
    val e = Rope.create("5555555555555555555")
    val f = Rope.create("666666")
    val g = Rope.create("7777777777777777")
    val all = a.concat(b).concat(c).concat(d).concat(e).concat(f).concat(g)
    val firstSplit = all.split(23)
    assert(firstSplit.isDefined)
    firstSplit.foreach { case (begin, end) =>
      assert("11111111112222222222222" == begin.toString)
      assert("2233333333444444444444455555555555555555556666667777777777777777" == end.toString)
    }
    val secondSplit = all.split(41)
    assert(secondSplit.isDefined)
    secondSplit.foreach { case (begin, end) =>
      assert("11111111112222222222222223333333344444444" == begin.toString)
      assert("4444455555555555555555556666667777777777777777" == end.toString)
    }
    val thirdSplit = all.split(52)
    assert(thirdSplit.isDefined)
    thirdSplit.map { case (begin, end) =>
      assert("1111111111222222222222222333333334444444444444555555" == begin.toString)
      assert("55555555555556666667777777777777777" == end.toString)

    }
    assert("111111111122222222222222233333333444444444444455555555555555555556666667777777777777777" ==
      all.toString)
  }

  test("you should be able to cut a section out of a rope") {
    val a = Rope.create("1111111111")
    val b = Rope.create("2222222222")
    val c = Rope.create("3333333333")
    val d = Rope.create("4444444444")
    val e = Rope.create("5555555555")
    val f = Rope.create("6666666666")
    val g = Rope.create("7777777777")
    val all = a.concat(b).concat(c).concat(d).concat(e).concat(f).concat(g)
    val res = all.delete_range(22, 48)
    assert(res.isDefined)
    res.foreach { case (cut, rest) =>
      assert("33333333444444444455555555" == cut.toString)
      assert("11111111112222222222335566666666667777777777" == rest.toString)
    }
  }

  test("You should be able to copy sections of a rope") {
    val a = Rope.create("1111111111")
    val b = Rope.create("2222222222")
    val c = Rope.create("3333333333")
    val d = Rope.create("4444444444")
    val e = Rope.create("5555555555")
    val f = Rope.create("6666666666")
    val g = Rope.create("7777777777")
    val all = a.concat(b).concat(c).concat(d).concat(e).concat(f).concat(g)
    val copy = all.get_range(22, 48)
    assert(copy.isDefined)
    copy.foreach { r =>
      assert(r.toString == "33333333444444444455555555")
    }
  }

  test("You should be able to find newlines") {
    val a = Rope.create(buildTestString('a', 10, 50))
    val b = Rope.create(buildTestString('b', 10, 50))
    val c = Rope.create(buildTestString('c', 20, 100))
    val d = Rope.create(buildTestString('d', 20, 100))
    val e = Rope.create(buildTestString('e', 30, 90))
    val f = Rope.create(buildTestString('f', 30, 90))
    val g = Rope.create(buildTestString('g', 30, 100))
    val all = a.concat(b).concat(c).concat(d).concat(e).concat(f).concat(g)
    assert(30 == all.number_of_newlines)
    val lines = (0 until 30).map(i => all.position_of_line(i).get)
    val expected = Array(0, 11, 22, 33, 44, 55, 66, 77, 88, 99, 110, 131, 152, 173, 194, 215, 236, 257, 278,
      299, 320, 351, 382, 413, 444, 475, 506, 537, 568, 599)
    for (i <- 0 until 30) {
      assert(expected(i) == lines(i))
    }
  }

  test("You should be able to figure out how long lines are") {
    val a = Rope.create("1111111111\n") // 0-11
    val b = Rope.create("222222222222222\n") // 11-27
    val c = Rope.create("33333333\n") // 27-36
    val d = Rope.create("4444444444444\n") // 36-49
    val e = Rope.create("5555555555555555555\n") // 49-69
    val f = Rope.create("666666\n") // 69-76
    val g = Rope.create("7777777777777777\n") // 76-
    val all = a.concat(b).concat(c).concat(d).concat(e).concat(f).concat(g)
    assert(10 == all.length_of_line(0).get)
    assert(15 ==  all.length_of_line(1).get)
    assert(8 ==  all.length_of_line(2).get)
    assert(13 == all.length_of_line(3).get)
    assert(19 ==  all.length_of_line(4).get)
    assert(6 == all.length_of_line(5).get)
    assert(16 == all.length_of_line(6).get)
    assert(all.length_of_line(7).isEmpty)
    assert(all.length_of_line(8).isEmpty)
  }

  test("you should be able to figure out what line you're on") {
    val a = Rope.create("1111111111\n")  // 0-11
    val b = Rope.create("222222222222222\n") // 11-27
    val c = Rope.create("33333333\n") // 27-36
    val d = Rope.create("4444444444444\n") // 36-49
    val e = Rope.create("5555555555555555555\n") // 49-69
    val f = Rope.create("666666\n") // 69-76
    val g = Rope.create("7777777777777777\n") // 76-
    val all = a.concat(b).concat(c).concat(d).concat(e).concat(f).concat(g)
    assert(1 == all.line_for_position(0).get)
    assert(1 == all.line_for_position(9).get)
    assert(1 == all.line_for_position(10).get)
    assert(2 == all.line_for_position(11).get)
    assert(2 == all.line_for_position(21).get)
    assert(3 == all.line_for_position(29).get)
    assert(4 == all.line_for_position(44).get)
    assert(5 == all.line_for_position(54).get)
    assert(6 == all.line_for_position(70).get)
    assert(7 == all.line_for_position(80).get)
    assert(7 == all.line_for_position(92).get)
    assert(all.line_for_position(99).isEmpty)
  }

}