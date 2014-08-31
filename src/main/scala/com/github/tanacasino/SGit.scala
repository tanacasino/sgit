package com.github.tanacasino

import java.io.File

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.treewalk.TreeWalk

import scala.collection.JavaConverters._

object SGit {

  def main(args: Array[String]): Unit = {
    // TODO Move these codes to ScalaTest code from here.
    val repositoryPath = if(args.size == 2) args(0) else "/Users/tanacasino/.gitbucket/repositories/root/git.git"
    val headCommitId = if(args.size == 2) args(1) else "cd547b4886c5338a70eb8a674bfc40eac5cab3d9"

    val sgit = SGit(repositoryPath)
    println(sgit.resolve(headCommitId)) // => AnyOBjectId
    println(sgit.resolve("hoge")) // => null

    println(s"${sgit.countCommit(headCommitId)}")
    sgit.resolve(headCommitId).map { headCommitObjectId =>
      println(s"${sgit.countCommit(headCommitObjectId)}")
      println(s"${sgit.countCommit(headCommitObjectId, 0)}")
    }
  }

  def apply(path: String): SGit = {
    new SGit(path)
  }
}

class SGit(path: String) {
  /**
   * For RevWalk and TreeWalk
   */
  def using[A <% {def release() : Unit}, B](resource: A)(f: A => B): B = {
    try f(resource) finally resource.release
  }

  /**
   * Open git directory and call function with org.eclipse.jgit.api.Git instance.
   */
  def open[T]()(f: Git => T): T = {
    val git = Git.open(new File(path))
    try {
      f(git)
    } finally {
      git.getRepository.close
    }
  }

  def resolve(id: String): Option[ObjectId] = {
    Option(open()(_.getRepository.resolve(id)))
  }

  def resolveCommit(id: String): Option[RevCommit] = {
    resolve(id).map { objectId =>
      open() { git =>
        using(new RevWalk(git.getRepository)) { rw =>
          rw.parseCommit(objectId)
        }
      }
    }
  }

  /**
   * Returns file list of specified commit id
   * @param commitId the commit id
   * @param path the path to search file list. Default . means top dir.
   * @return file list of specified commit id
   */
  def listFiles(commitId: String, path: String = "."): List[String] = {
    // TODO
    List("")
  }

  def countCommit(start: String): Option[Long] = {
    resolve(start).flatMap { id =>
      Option(countCommit(id))
    }
  }

  def countCommit(start: ObjectId, limit: Int = 0): Int = {
    open() { git =>
      val log = git.log.add(start).all
      if (limit > 0) log.setMaxCount(limit)
      log.call.iterator.asScala.map(_ => 1).sum
    }
  }

}
