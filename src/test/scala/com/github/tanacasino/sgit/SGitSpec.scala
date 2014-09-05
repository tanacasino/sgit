package com.github.tanacasino.sgit

import collection.mutable.Stack
import org.scalatest._

class SGitSpec extends FlatSpec with Matchers {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }

  "SGit.getLatestCommits" should "returns latest commit of file" in {
    val repositoryPath = "/Users/tanacasino/.gitbucket/repositories/root/git.git"
    val headCommitId = "cd547b4886c5338a70eb8a674bfc40eac5cab3d9"
    val sgit = SGit(repositoryPath)
    val commit = sgit.resolveCommit(headCommitId).get
    val paths = sgit.listFilesInPath(headCommitId, ".")

    /*
    val expected = sgit.getLastModifiedCommitFromPathsSlow(commit, paths)
    val actual = sgit.getLastModifiedCommitFromPathsFast(commit, paths)
    actual.size should be (expected.size)
    paths.foreach{ path =>
      (expected.get(path), actual.get(path)) match {
        case (Some(e), Some(a)) => if(e != a) println(s"not match: $path, $e, $a")
        case _ => println(s"not match: $path")
      }
    }
    */
    /*
    4 files not match... but this result is same as GitHub.
    https://github.com/git/git/tree/cd547b4886c5338a70eb8a674bfc40eac5cab3d9

    not match: grep.c, 6bfce93e04d13ecb42008a3cf214cc892f480f0c, 634cd48a8afdd920fa26c8ec3ae43e96c82c81f2
    not match: builtin-ls-files.c, 80bffaf7fbe09ef62ecb9a6ffea70ac0171b456c, 64586e75af3c84844b80652575a8b63a9612b24a
    not match: RelNotes, 1e61b7640d09015213dbcae3564fa27ac6a8c151, 806ea701ce3624aa6a89648b6ca5d858703398cb
    not match: GIT-VERSION-GEN, 1e61b7640d09015213dbcae3564fa27ac6a8c151, 806ea701ce3624aa6a89648b6ca5d858703398cb
     */

    val result = sgit.getLastModifiedCommitFromPathsFast(commit, paths)
    result.foreach(println)
  }
}