package org.goodmath.apog.editor

import java.io.File
/**
  * The internal state of an editor.
  * An editor starts with a file to edit - it could be any file in a filesystem.
  * We try to find a project root in a directory containing the file.
  * A project root is a directory that contains either a VC root (like .git or .hg),
  * or a marker file named ".project". If a file named .project exists, its contents
  * doesn't matter - we'll just assume it marks a project root.
  */

class EditorState(anyFile: File) {

  val rootDir: File = EditorState.findRootContaining(anyFile)

}

object EditorState {
  def findRootContaining(aFile: File): File = {
    if (aFile.isDirectory) {
      val members = aFile.listFiles()
      if (members.contains(new File(aFile, ".git")) || members.contains(new File(aFile, ".hg")) ||
          members.contains(new File(aFile, ".project"))) {
        aFile
      } else {
        val parent = aFile.getParentFile
        if (parent == null) {
          aFile
        } else {
          findRootContaining(parent)
        }
      }
    } else {
      findRootContaining(aFile.getParentFile)
    }
  }
}
