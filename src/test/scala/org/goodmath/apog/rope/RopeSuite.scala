package org.goodmath.apog.rope

import org.scalatest._

class RopeSuite extends FunSuite {
  def createJaggedLetterRope: Rope =
    Rope.create(buildTestString('a', 23, 50))
      .concat(Rope.create(buildTestString('b', 17, 50)))
      .concat(Rope.create(buildTestString('c', 19, 50)))
      .concat(Rope.create(buildTestString('d', 13, 50)))
      .concat(Rope.create(buildTestString('e', 29, 50)))
      .concat(Rope.create(buildTestString('f', 31, 50)))
      .concat(Rope.create(buildTestString('g', 37, 50)))

  def createJaggedNumberRope: Rope =
    Rope.create("1111111111\n")
      .concat(Rope.create("222222222222222\n"))
      .concat(Rope.create("33333333\n"))
      .concat(Rope.create("4444444444444\n"))
      .concat(Rope.create("5555555555555555555\n"))
      .concat(Rope.create("666666\n"))
      .concat(Rope.create("7777777777777777\n"))

  def createRectNumberRope: Rope =
    Rope.create("1111111111")
      .concat(Rope.create("2222222222"))
      .concat(Rope.create("3333333333"))
      .concat(Rope.create("4444444444"))
      .concat(Rope.create("5555555555"))
      .concat(Rope.create("6666666666"))
      .concat(Rope.create("7777777777"))

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
    val all = createJaggedLetterRope
    assert('a' == all.charAt(0).get)
    assert('a' == all.charAt(1).get)
    assert('a' ==  all.charAt(8).get)
    assert('a' ==  all.charAt(8).get)
    assert('a' ==  all.charAt(10).get)
    assert('a' ==  all.charAt(12).get)
    assert('a' ==  all.charAt(16).get)
    assert('a' ==  all.charAt(19).get)
    assert('a' ==  all.charAt(20).get)
    assert('\n' == all.charAt(23).get)
    assert('a' == all.charAt(26).get)
    assert('a' == all.charAt(31).get)
    assert('\n' == all.charAt(47).get)
    assert('b' ==  all.charAt(55).get)
    assert('b' ==  all.charAt(56).get)
    assert('\n' ==  all.charAt(70).get)
    assert('b' ==  all.charAt(91).get)
    assert('b' ==  all.charAt(100).get)
    assert('c' ==  all.charAt(110).get)
    assert('c' ==  all.charAt(130).get)
    assert('c' ==  all.charAt(140).get)
    assert('c' ==  all.charAt(150).get)
    assert('\n' ==  all.charAt(158).get)
    assert('d' ==  all.charAt(160).get)
    assert('d' ==  all.charAt(170).get)
    assert('d' ==  all.charAt(180).get)
    assert('d' ==  all.charAt(190).get)
    assert('\n' ==  all.charAt(200).get)
    assert('d' ==  all.charAt(210).get)
    assert('e' ==  all.charAt(220).get)
    assert('e' ==  all.charAt(230).get)
    assert('e' ==  all.charAt(240).get)
    assert('e' ==  all.charAt(250).get)
    assert('e' ==  all.charAt(260).get)
    assert('f' ==  all.charAt(270).get)
    assert('f' ==  all.charAt(280).get)
    assert('f' ==  all.charAt(290).get)
    assert('f' ==  all.charAt(300).get)
    assert('f' ==  all.charAt(310).get)
    assert('g' ==  all.charAt(320).get)
    assert('g' ==  all.charAt(330).get)
    assert('g' ==  all.charAt(340).get)
    assert('g' ==  all.charAt(350).get)
    assert('g' ==  all.charAt(360).get)
    assert(all.charAt(1000).isEmpty)
  }

    test("a rope should be splittable into subropes") {
      val all = createRectNumberRope

      val sp = all.split(21)
      assert(sp.isDefined)
      sp.foreach { case (before, after) =>
        assert("111111111122222222223" == before.toString)
        assert("3333333334444444444555555555566666666667777777777" == after.toString)
        assert("1111111111222222222233333333334444444444555555555566666666667777777777" ==
          all.toString)
        assert("(L: '111111111122222222223')" == before.inspect)
        assert("(I: (L: '333333333444444444455555555556666666666'), (L: '7777777777'))" ==
          after.inspect)
        assert(all.toString == before.concat(after).toString)
      }
    }

  test("You should be able to insert into the middle of a rope") {
    val all = createRectNumberRope
    val res = all.insertAt(Rope.create("abc"), 13)
    assert(res.isDefined)
    res.foreach { newr =>
      assert("1111111111222abc222222233333333334444444444555555555566666666667777777777" ==
        newr.toString)
      assert("1111111111222222222233333333334444444444555555555566666666667777777777" ==
        all.toString)
    }
  }

  test("Multiple splits shouldn't interfere with each other") {
    val all = createJaggedNumberRope
    val firstSplit = all.split(23)
    assert(firstSplit.isDefined)
    firstSplit.foreach { case (begin, end) =>
      assert("1111111111\n222222222222" == begin.toString)
      assert("222\n33333333\n4444444444444\n5555555555555555555\n666666\n7777777777777777\n" ==
        end.toString)
    }
    val secondSplit = all.split(41)
    assert(secondSplit.isDefined)
    secondSplit.foreach { case (begin, end) =>
      assert("1111111111\n222222222222222\n33333333\n44444" == begin.toString)
      assert("44444444\n5555555555555555555\n666666\n7777777777777777\n" == end.toString)
    }
    val thirdSplit = all.split(52)
    assert(thirdSplit.isDefined)
    thirdSplit.map { case (begin, end) =>
      assert("1111111111\n222222222222222\n33333333\n4444444444444\n55" == begin.toString)
      assert("55555555555555555\n666666\n7777777777777777\n" == end.toString)

    }
    assert(("1111111111\n222222222222222\n33333333\n4444444444444\n" +
            "5555555555555555555\n666666\n7777777777777777\n") ==
      all.toString)
  }

  test("You should be able to split the result of a split") {
    val all = createJaggedNumberRope
    val firstSplit = all.split(23)
    assert(firstSplit.isDefined)
    firstSplit.foreach { case (begin, end) =>
      assert("1111111111\n222222222222" == begin.toString)
      assert("222\n33333333\n4444444444444\n5555555555555555555\n666666\n7777777777777777\n" ==
        end.toString)
    }
    val (fPre, fPost) = firstSplit.get
    val secondSplit = fPre.split(12)
    assert(secondSplit.isDefined)
    val (sPre, sPost) = secondSplit.get
    assert("1111111111\n2" == sPre.toString)
    assert("22222222222" == sPost.toString)
    val thirdSplit = fPost.split(19)
    assert(thirdSplit.isDefined)
    val (tPre, tPost) = thirdSplit.get
    assert("222\n33333333\n444444" == tPre.toString)
    assert("4444444\n5555555555555555555\n666666\n7777777777777777\n" == tPost.toString)
    assert(("1111111111\n222222222222222\n33333333\n4444444444444\n" +
      "5555555555555555555\n666666\n7777777777777777\n") ==
      all.toString)
  }

  test("you should be able to cut a section out of a rope") {
    val all = createRectNumberRope
    val res = all.deleteRange(22, 48)
    assert(res.isDefined)
    res.foreach { case (cut, rest) =>
      assert("33333333444444444455555555" == cut.toString)
      assert("11111111112222222222335566666666667777777777" == rest.toString)
    }
  }

  test("You should be able to copy sections of a rope") {
    val all = createRectNumberRope
    val copy = all.getRange(22, 48)
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
    assert(30 == all.numberOfNewlines)
    val lines = (0 until 30).map(i => all.positionOfLine(i).get)
    val expected = Array(0, 11, 22, 33, 44, 55, 66, 77, 88, 99, 110, 131, 152, 173, 194,
      215, 236, 257, 278, 299, 320, 351, 382, 413, 444, 475, 506, 537, 568, 599)
    for (i <- 0 until 30) {
      assert(expected(i) == lines(i))
    }
  }

  test("You should be able to figure out how long lines are") {
    val all = createJaggedNumberRope
    assert(11 == all.lengthOfLine(0).get)
    assert(16 ==  all.lengthOfLine(1).get)
    assert(9 ==  all.lengthOfLine(2).get)
    assert(14 == all.lengthOfLine(3).get)
    assert(20 ==  all.lengthOfLine(4).get)
    assert(7 == all.lengthOfLine(5).get)
    assert(17 == all.lengthOfLine(6).get)
    assert(all.lengthOfLine(7).isEmpty)
    assert(all.lengthOfLine(8).isEmpty)
  }

  test("you should be able to figure out what line you're on") {
    val all = createJaggedNumberRope
    assert(0 == all.lineOfPosition(0).get)
    assert(0 == all.lineOfPosition(9).get)
    assert(0 == all.lineOfPosition(10).get)
    assert(1 == all.lineOfPosition(11).get)
    assert(1 == all.lineOfPosition(21).get)
    assert(2 == all.lineOfPosition(29).get)
    assert(3 == all.lineOfPosition(44).get)
    assert(4 == all.lineOfPosition(54).get)
    assert(5 == all.lineOfPosition(70).get)
    assert(6 == all.lineOfPosition(80).get)
    assert(6 == all.lineOfPosition(92).get)
    assert(all.lineOfPosition(99).isEmpty)
  }

  test("you should be able to do character inserts in-place sometimes") {
    val orig = Rope.create("this is a string of test data")
    val one = orig.insertChar('!', 11)
    assert(one.isDefined)
    assert("(I: (L: 'this is a s!'), (L: 'tring of test data'))" == one.get.inspect)
    val two = one.get.insertChar('@', 12)
    assert(two.isDefined)
    assert("(I: (L: 'this is a s!@'), (L: 'tring of test data'))" == two.get.inspect)
    // Do another insert - this should work in-place.
    val three = one.get.insertChar('#', 13)
    assert(three.isDefined)
    assert("(I: (L: 'this is a s!@#'), (L: 'tring of test data'))" == three.get.inspect)
    assert(two.get == three.get)
    // This one shouldn't work in-place.
    val four = three.get.insertChar('$', 11)
    assert(four.isDefined)
    assert("(I: (L: 'this is a s$!@#'), (L: 'tring of test data'))" == four.get.inspect)
    assert(four != three)
  }
}