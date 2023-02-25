/*
 * Copyright 2023 Mark C. Chu-Carroll
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.goodmath.apogee.rope

import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*

class RopeTest {

    @Test
    fun testGetLength() {
        val r = Rope.create("hello world")
        assertEquals(11, r.length)
        val s = Rope.create("hello world\nHow are you\nI am fine\nthis is boring\n")
        assertEquals(49, s.length)
    }

    @Test
    fun getNewlines() {
        val r = Rope.create("hello world")
        assertEquals(0, r.newlines)
        val s = Rope.create("hello world\nHow are you\nI am fine\nthis is boring\n")
        assertEquals(4, s.newlines)
    }

    @Test
    fun testConcat() {
        val a = Rope.create("aaaaa\naaaaa\n")
        val b = Rope.create("bbbbb\nbbbbb\nbbbbb\n")
        val c = Rope.create("ccccc\nccccc\nccccc\nccccc\n")
        val ab = a.concat(b)
        assertEquals("aaaaa\naaaaa\nbbbbb\nbbbbb\nbbbbb\n", ab.toString())
        assertEquals(ab.concat(c).toString(), a.concat(b.concat(c)).toString())
        assertEquals(5, ab.newlines)
    }

    @Test
    fun insertAt() {
        val one = Rope.create("123456789\n123456789\n123456789\n")
        val i = one.insertAt(17, "abc")
        assertEquals("123456789\n1234567abc89\n123456789\n", i.toString())
        val a = Rope.create("aaaa")
        val b = Rope.create("bbbb")
        val c = Rope.create("cccc")
        val d = Rope.create("dddd")
        val abcd = a.concat(b).concat(c.concat(d))
        assertEquals("[[{'aaaa'} -- {'bbbb'}] -- [{'cccc'} -- {'dddd'}]]", abcd.inspect())
        val abcda = abcd.insertAt(2, "!!!!")
        assertEquals("[{'aa!!!!'} -- [[{'aa'} -- {'bbbb'}] -- [{'cccc'} -- {'dddd'}]]]",
            abcda.inspect())
        val abcdb = abcd.insertAt(7, "!!!!")
        assertEquals("[[{'aaaa'} -- {'bbb!!!!'}] -- [{'b'} -- [{'cccc'} -- {'dddd'}]]]",
            abcdb.inspect())
        val abcdc = abcd.insertAt(10, "!!!!")
        assertEquals("[[[{'aaaa'} -- {'bbbb'}] -- {'cc!!!!'}] -- [{'cc'} -- {'dddd'}]]", abcdc.inspect())
        val abcdd = abcd.insertAt(13, "!!!!")
        assertEquals("[[[{'aaaa'} -- {'bbbb'}] -- [{'cccc'} -- {'d!!!!'}]] -- {'ddd'}]", abcdd.inspect())
        // Check that none of these changed the original rope.
        assertEquals("[[{'aaaa'} -- {'bbbb'}] -- [{'cccc'} -- {'dddd'}]]", abcd.inspect())
    }

    @Test
    fun testAppend() {
        val one = Rope.create("1111")
        val two = Rope.create("2222")
        val onea = one.append("abc")
        assertEquals("1111abc", onea.toString())
        assertEquals("1111", one.toString())
        val oneb = one.append("def")
        assertEquals("1111def", oneb.toString())
        assertEquals("1111", one.toString())
        val twoa = one.concat(two).append("abc")
        assertEquals("11112222abc", twoa.toString())
        assertEquals("1111abc", onea.toString())
        assertEquals("1111", one.toString())
    }

    @Test
    fun testSplit() {
        val one = Rope.create("123456789\n123456789\n123456789\n")
        val (onel, oner) = one.split(10)
        assertEquals("123456789\n", onel.toString())
        assertEquals("123456789\n123456789\n", oner.toString())

        val twoa = Rope.create("abcdefghi\n")
        val twob = Rope.create("jklmnopqr\n")
        val two = twoa.concat(twob)
        assertEquals("[{'abcdefghi\n" +
                "'} -- {'jklmnopqr\n" +
                "'}]", two.inspect())
        val (twoll, twolr) = two.split(5)
        assertEquals("abcde",twoll.toString())
        assertEquals("fghi\njklmnopqr\n", twolr.toString())
        val (tworl, tworr) = two.split(15)
        assertEquals("abcdefghi\njklmn", tworl.toString())
        assertEquals("opqr\n", tworr.toString())
    }

    @Test
    fun testDeleteRange() {
        val one = Rope.create("1234")
        val two = Rope.create("2345")
        val three = Rope.create("3456")
        val four = Rope.create("4567")
        val five = Rope.create("5678")
        val all = one.concat(two).concat(three).concat(four).concat(five)

        val (c1, r1) = all.deleteRange(5, 7)
        assertEquals("34", c1.toString())
        assertEquals("123425345645675678", r1.toString())

        val (c2, r2) = all.deleteRange(3, 11)
        assertEquals("42345345", c2.toString())
        assertEquals("123645675678", r2.toString())
    }

    @Test
    fun copyRange() {
        val one = Rope.create("1234")
        val two = Rope.create("2345")
        val three = Rope.create("3456")
        val four = Rope.create("4567")
        val five = Rope.create("5678")
        val all = one.concat(two).concat(three).concat(four).concat(five)

        val c1 = all.copyRange(5, 7)
        assertEquals("34", c1.toString())

        val c2 = all.copyRange(3, 11)
        assertEquals("42345345", c2.toString())

        assertEquals("12342345345645675678", all.toString())
    }

    @Test
    fun testRebalance() {
        val a = Rope.create("abcdefghijklmnopqrstuvwxyz")

        val glob = a.concat(a).concat(a).concat(a).concat(a)
        val globber = glob.concat(glob).concat(glob).concat(glob)
        val globbest = globber.concat(globber).concat(globber).concat(globber).concat(globber).concat(globber).concat(globber).concat(globber).concat(globber).concat(globber).concat(globber)

        assertEquals("18: (17: (16, 8), 8: (7, 5))", globbest.image())
        val bal = globbest.rebalance()
        assertEquals("4: (3, 3)", bal.image())
        assertEquals(globbest.toString(), bal.toString())
    }
}