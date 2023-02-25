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
package org.goodmath.apogee

import org.goodmath.apogee.rope.Rope
import java.io.FileReader
import java.io.FileWriter
import java.nio.file.Path
import java.util.Stack
import kotlin.io.path.div
import kotlin.io.path.exists
import kotlin.io.path.moveTo
import kotlin.io.path.name

class Buffer(
    var name: String,
    var path: Path,
    private var contents: Rope
) {
    val length: Int
        get() = contents.length

    var cursor: Int = 0
    var selectionEnd: Int? = null
    val undoStack: Stack<Rope> = Stack()

    fun moveBy(distance: Int) {
    }

    fun moveTo(pos: Int) {

    }

    fun moveSelectBy(distance: Int) {

    }

    fun moveSelectTo(pos: Int) {

    }

    fun insert(str: String) {
        val oldContents = contents
        contents = contents.insertAt(cursor, str)
        cursor += str.length
        undoStack.push(oldContents)
    }

    fun insert(c: Char) {
        val oldContents = contents
        contents = contents.insertAt(cursor, c)
        cursor++
        undoStack.push(oldContents)
    }

    fun insert(r: Rope) {
        val oldContents = contents
        contents = contents.insertAt(cursor, r)
        cursor += r.length
        undoStack.push(oldContents)
    }

    fun copy(): Rope {
        if (selectionEnd == null) {
            return Rope.empty
        } else {
            return contents.copyRange(cursor, selectionEnd!!)
        }
    }

    fun cut(): Rope {
        if (selectionEnd == null) {
            return Rope.empty
        } else {
            val old = contents
            val (cut, new) = contents.deleteRange(cursor, selectionEnd!!)
            contents = new
            undoStack.push(old)
            return cut
        }
    }

    fun deleteCharBack() {
        moveBy(-1)
        moveSelectTo(cursor + 1)
        cut()
    }

    fun deleteCharForward() {
        moveSelectTo(cursor + 1)
        cut()
    }

    fun readFromFile(readPath: Path? = null) {
        val old = contents
        val pathToRead = readPath ?: path
        val reader = FileReader(pathToRead.toFile())
        contents = Rope.create(reader.readText())
        reader.close()
        undoStack.push(old)
    }

    private fun backupFileName(path: Path): Path {
        var count = 1
        var b: Path = path.parent / (path.name + "._$count")
        while (b.exists()) {
            count++
        }
        return b
    }

    fun writeToFile(writePath: Path? = null) {
        var pathToWrite = writePath ?: path
        if (pathToWrite.exists()) {
            val backupPath = backupFileName(pathToWrite)
            pathToWrite.moveTo(backupPath)
        }
        val writer = FileWriter(pathToWrite.toFile())

        writer.write(contents.toString())
        writer.close()
    }

    fun undo() {
        contents = undoStack.pop()
    }
}
