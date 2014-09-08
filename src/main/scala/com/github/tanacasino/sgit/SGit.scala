package com.github.tanacasino.sgit

import java.io.File

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.treewalk.filter.PathFilter

import scala.collection.JavaConverters._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}


object SGit {
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
   * Returns file list of specified commit id in path
   * @param commitId the commit id
   * @param path the path to search file list. Default . means top dir.
   * @return file list of specified commit id
   */
  def listFilesInPath(commitId: String, path: String = "."): List[String] = {
    open() { git =>
      val repo = git.getRepository
      using(new TreeWalk(repo)) { tw =>
        resolveCommit(commitId) match {
          case Some(id) => tw.addTree(id.getTree)
          case None => println("TODO throw error? or return Nil?"); return Nil
        }

        if (path != ".") {
          tw.setFilter(PathFilter.create(path))
        }
        val cleanPath = path.stripSuffix("/").split("/")
        val depth = if (path != ".") cleanPath.size else 0

        @scala.annotation.tailrec
        def walk(walker: TreeWalk, files: List[String]): List[String] = {
          if(!walker.next()) return files
          if(depth == walker.getDepth) {
            walk(walker, walker.getPathString :: files)
          } else if(walker.isSubtree && walker.getNameString == cleanPath(walker.getDepth)) {
            tw.enterSubtree
            walk(walker, files)
          } else {
            files  // not correct
          }
        }

        walk(tw, List())
      }
    }
  }

  def listAllFiles(commitId: String, path: String = "."): List[String] = {
    open() { git =>
      val repo = git.getRepository
      using(new TreeWalk(repo)) { tw =>
        resolveCommit(commitId).map { id =>
          tw.addTree(id.getTree)
          tw.setRecursive(true)
        }

        @scala.annotation.tailrec
        def list(walk: TreeWalk, files: List[String]): List[String] = {
          if (walk.next) {
            list(walk, walk.getPathString :: files)
          } else {
            files
          }
        }

        list(tw, List())
      }
    }
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

  // NOTE Too much slow version
  def getLastModifiedCommitFromPathsSlow(commit: RevCommit, paths: List[String]): Map[String, String] = {
    open() { git =>
      paths.map { path =>
        val c = git.log.add(commit).setMaxCount(1).addPath(path).call.iterator.next
        (path -> c.getName)
      }.toMap
    }
  }

  // コミットグラフの探索を一回で済ます。そしたら早くなるはず
  // だいぶイケてないコードだけどもろもろ実験して速くできそうだったら綺麗にする
  // TODO blob/tree の一覧を出す処理をキャッシュする
  // TODO すでにコミットが確定したファイル・ディレクトリに対する処理をやめる. blob/tree 一覧出すときもいらないのか・・・ß
  def getLastModifiedCommitFromPathsFast(commit: RevCommit, paths: List[String]): Map[String, String] = {
    open() { implicit git =>
      val ite = git.log.add(commit).call().iterator
      var stop = false
      var result: Map[String, String] = Map()
      val lookupPaths: ArrayBuffer[String] = paths.to[ArrayBuffer]
      //var commitInfoCache: Map[String, Map[String, FileInfo]] = Map()
      val commitInfoCache = scala.collection.mutable.HashMap.empty[String, Map[String, FileInfo]]

      def getCommitInfoListInCache(c: RevCommit, path: String = ".", paths: Seq[String]): Map[String, FileInfo] = {
        if(commitInfoCache.contains(c.getId.getName)) {
          commitInfoCache.get(c.getId.getName).get // ださいコード
         } else {
          val info = getCommitInfoList(c, path, paths)
          commitInfoCache += (c.getId.getName -> info)
          info
        }
      }

      while(ite.hasNext && !stop) {
        val c = ite.next
        if(c.getParentCount >= 2) {
          // merge commit. multiple parents

          val base = getCommitInfoListInCache(c, ".", lookupPaths)
          val parents = c.getParents.map{ parent =>
            getCommitInfoListInCache(parent, ".", lookupPaths)
          }
          var foundPaths = List[String]()
          lookupPaths.foreach { path =>
            val baseInfo = base.get(path)
            val changedThisCommit = parents.map { p =>
              p.get(path)
            }.forall { p =>
              p != baseInfo
            }
            if(changedThisCommit && !result.contains(path)) {
              result += (path -> c.getId.getName)
              foundPaths = path :: foundPaths
            }
          }
          foundPaths.foreach(f => lookupPaths -= f)
        } else if (c.getParentCount == 1) {
          // not merge commit. (only one parent)
          val base = getCommitInfoListInCache(c, ".", lookupPaths)
          val parent = getCommitInfoListInCache(c.getParent(0), ".", lookupPaths)
          val changes = lookupPaths.filter { path =>
            val baseInfo = base.get(path)
            val parentInfo = parent.get(path)
            (baseInfo, parentInfo) match {
              case (Some(a), Some(b)) => a.objectId != b.objectId
              case (Some(a), None) => true
              case (None, Some(b)) => true
              case (None, None) => false
            }
          }
          changes.foreach { change =>
            if(!result.contains(change)) result += (change -> c.getId.getName)
            lookupPaths -= change
          }
        }
        if(!ite.hasNext && result.size != paths.size) {
          // First commit in history.
          lookupPaths.foreach { k =>
            if(!result.contains(k)) {
              result += (k -> c.getId.getName)
            }
          }
        }
        if(result.size == paths.size) {
          // found commit all paths
          stop = true
        }
        //println(s"lookupPaths: ${lookupPaths.size}")
      }
      result
    }
  }

  case class FileInfo(path: String, objectId: String)

  def getCommitInfoList(commit: RevCommit, path: String = ".", paths: Seq[String]): Map[String, FileInfo] = {
    open() { git =>
      val repo = git.getRepository
      using(new TreeWalk(repo)) { tw =>
        tw.addTree(commit.getTree)
        if (path != ".") {
          tw.setFilter(PathFilter.create(path))
        }
        val cleanPath = path.stripSuffix("/").split("/")
        val depth = if (path != ".") cleanPath.size else 0

        @scala.annotation.tailrec
        def walk(walker: TreeWalk, files: Map[String, FileInfo]): Map[String, FileInfo] = {
          if(!walker.next()) return files
          if(depth == walker.getDepth) {
            if(paths.contains(walker.getPathString)) {
              walk(walker,
                   files + (walker.getPathString -> FileInfo(walker.getPathString, walker.getObjectId(0).getName)))
            } else {
              // skip
              walk(walker, files)
            }
          } else if(walker.isSubtree && walker.getNameString == cleanPath(walker.getDepth)) {
            tw.enterSubtree
            walk(walker, files)
          } else {
            files  // not correct
          }
        }

        walk(tw, Map())
      }
    }
  }

}
