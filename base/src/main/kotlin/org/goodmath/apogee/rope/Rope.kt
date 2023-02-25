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

import org.goodmath.apogee.common.ApogeeException
import java.lang.StringBuilder
import kotlin.streams.toList

/**
 * A rope is a string-like data structure that's optimized for a functional
 * approach to text manipulation by tools like a text editor. Ropes are
 * immutable binary tree structures, where the left branch of any node contains
 * text that appears earlier than the right branch.b
 */
sealed class Rope {
    private val DEEP_ROPE = 20
    companion object {
        private final val PREFERRED_NODE_SIZE: Int = 1024

        /**
         * Create a rope from a string.
         */
        fun create(s: String): Rope {
            val chars = s.chars().toList().map { Char(it) }.toMutableList()
            return Leaf(chars, 0, chars.size)
        }

        /**
         * Create a well-balanced rope from a long string. This is used mainly
         * for rebalancing ropes. It's not as efficient as red-black style pivots,
         * but ropes tend to accumulate odd patterns, which make it difficult
         * to achieve balance by straightforward pivots. Instead, it's better to
         * just do it this way.
         */
        fun createBalancedRope(s: String): Rope {
            if (s.length > PREFERRED_NODE_SIZE) {
                val splitPoint = s.length / 2
                val left = s.substring(0, splitPoint)
                val right = s.substring(splitPoint)
                return createBalancedRope(left).concat(createBalancedRope(right))
            } else {
                return create(s)
            }
        }

        val empty = create("")
    }

    abstract val length: Int
    abstract val newlines: Int

    /**
     * Check if the tree is in balance.
     */
    open fun isBalanced(): Boolean = true

    open fun rebalance(): Rope = this

    /**
     * Combine two ropes
     */
    fun concat(other: Rope): Rope {
        val result = Internal(this, other)
        return if (result.depth > DEEP_ROPE && !result.isBalanced()) {
            result.rebalance()
        } else {
            result
        }
    }

    /**
     * Insert a string at a position within a rope.
     * @param idx the index of the position where the string should be inserted.
     * @param str the string to insert.
     * @return the new rope
     * @throws if the index isn't valid.
     */
    fun insertAt(idx: Int, str: String): Rope {
        val (l, r) = split(idx)
        return l.append(str).concat(r)
    }

    /**
     * Insert a rope at a position within this rope.
     * @param idx the index of the position where the rope should be inserted.
     * @param r the rope to insert.
     * @return the new rope
     * @throws if the index isn't valid.
     */
    fun insertAt(idx: Int, r: Rope): Rope {
        val (before, after) = split(idx)
        return before.concat(r).concat(after)
    }

    /**
     * Insert a character at a position within a rope.
     * @param idx the index of the position where the string should be inserted.
     * @param str the string to insert.
     * @return the new rope
     * @throws if the index isn't valid.
     */
    fun insertAt(idx: Int, c: Char): Rope {
        val (l, r) = split(idx)
        return l.append(c).concat(r)
    }

    /**
     * Append a string onto the end of a rope.
     */
    abstract fun append(s: String): Rope

    abstract fun append(c: Char): Rope

    /**
     * Split a rope into two sub-ropes at a position.
     */
    abstract fun split(index: Int): Pair<Rope, Rope>

    /**
     * Delete a range of characters from a rope.
     */
    fun deleteRange(start: Int, end: Int): Pair<Rope, Rope> {
        if (start > end) {
            throw ApogeeException("Invalid range; start after end")
        }
        val (before, including) = split(start)
        val (cut, after) = including.split(end - start)
        return Pair(cut, before.concat(after))
    }

    fun copyRange(start: Int, end: Int): Rope {
        if (start > end) {
            throw ApogeeException("Invalid range; start after end")
        }
        val (_, including) = split(start)
        val (cut, _) = including.split(end - start)
        return cut
    }

    abstract val depth: Int

    /**
     * Render the tree structure of a rope as a string.
     * Provided for debugging purposes.
     */
    abstract fun inspect(): String

    /**
     * Render information about the depth and balance of a rope.
     * Provided for debugging purposes.
     */
    abstract fun image(): String

    /**
     * An internal node in a rope, representing the concatenation of its left
     * and right children.
     */
    class Internal(val left: Rope, val right: Rope): Rope() {
        override val depth = Math.max(left.depth, right.depth) + 1
        override val length = left.length + right.length
        override val newlines: Int = left.newlines + right.newlines

        override fun isBalanced(): Boolean {
            return (left.depth <= (2 * right.depth) &&
                right.depth <= (2 * left.depth))
        }

        override fun rebalance(): Rope {
            return Rope.createBalancedRope(toString())
        }

        private fun rotateRight(): Rope {
            return if (left is Internal) {
                val ll = left.left
                val lr = left.right
                return ll.concat(lr.concat(right))
            } else {
                this
            }
        }

        private fun rotateLeft(): Rope {
            return if (right is Internal) {
                val rl = right.left
                val rr = right.right
                return (left.concat(rl)).concat(rr)
            } else {
                this
            }
        }


        override fun append(s: String): Rope {
            return left.concat(right.append(s))
        }

        override fun append(c: Char): Rope {
            return left.concat(right.append(c))
        }

        override fun split(index: Int): Pair<Rope, Rope> {
            if (index < left.length) {
                val (l, r) = left.split(index)
                return Pair(l, r.concat(right))
            } else {
                val (l, r) = right.split(index - left.length)
                return Pair(left.concat(l), r)
            }
        }

        override fun inspect(): String {
            return "[" + left.inspect() + " -- " + right.inspect() + "]"
        }

        override fun image(): String {
            if (!isBalanced()) {
                return "$depth: (${left.image()}, ${right.image()})"
            } else {
                return "$depth: (${left.depth}, ${right.depth})"
            }
        }

        override fun toString(): String {
            return left.toString() + right.toString()
        }

    }

    class Leaf(val contents: MutableList<Char>, val offset: Int, override val length: Int): Rope() {
        override val depth = 1

        override fun append(s: String): Rope {
            // If our character list hasn't been extended,
            // then we can append by extending it.
            if (contents.size == offset + length) {
                s.chars().forEach { c: Int ->
                    contents.add(Char(c));
                }
                return Leaf(contents, offset, length + s.length)
            } else {
                // If it was already extended, then we need to copy it.
                val newContents = mutableListOf<Char>()
                contents.subList(offset, offset + length).forEach { c ->
                    newContents.add(c)
                }
                s.chars().forEach { newContents.add(Char(it)) }
                return Leaf(newContents, 0, length + s.length)
            }
        }

        override fun append(c: Char): Rope {
            if (contents.size == offset + length) {
                contents.add(c);
                return Leaf(contents, offset, length + 1)
            } else {
                val newContents = mutableListOf<Char>()
                contents.subList(offset, offset + length).forEach { ch ->
                    newContents.add(ch)
                }
                return Leaf(newContents, 0, length + 1);
            }
        }

        override fun split(index: Int): Pair<Rope, Rope> {
            if (index < 0 || index > length) {
                throw ApogeeException("Index out of bounds")
            }
            // We can split a leaf without copying the actual characters,
            // by sharing the character list, and setting offset and length.
            val left = Leaf(contents, offset, index)
            val right = Leaf(contents, offset + index, length - index)
            return Pair(left, right)
        }

        override fun inspect(): String {
            return "{'" + toString() + "'}"
        }

        override fun image(): String {
            return "[1]"
        }

        override fun toString(): String {
            val b = StringBuilder()
            contents.subList(offset, offset + length).forEach{ b.append(it) }
            return b.toString()
        }

        override val newlines: Int  =
            contents.subList(offset, offset + length).filter { it == '\n' }.count()

    }

}