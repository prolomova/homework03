package fintech.homework03

import scala.annotation.tailrec
// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait IPrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], value: U): IPrefixTree[K, U]
  def sub(path: Seq[K]): IPrefixTree[K, V]
  def get: V
}

class PrefixTree[K, +V](val subTrees: Map[K, PrefixTree[K, V]], val value: Option[V]) extends IPrefixTree[K, V]{

  override def hashCode(): Int = 41 * (41 * value.hashCode()) + subTrees.hashCode()

  override def equals(a: Any): Boolean = {
    a match {
      case prefixTree: PrefixTree[K, V] => prefixTree.value.getOrElse() == value.getOrElse() &&
        prefixTree.subTrees == subTrees
      case _ => false
    }
  }

  def put[U >: V](path: Seq[K], value: U): IPrefixTree[K, U] = {
    val curTreeLevel = this
    def addValue(curTreeLevel : PrefixTree[K, U], path: Seq[K]): PrefixTree[K, U] ={
      if (path.isEmpty) new PrefixTree[K, U](curTreeLevel.subTrees, Some(value))
      else {
        val nextTreeLevel =
        if (!curTreeLevel.subTrees.contains(path.head))
          new PrefixTree[K, U](Map.empty, None)
        else curTreeLevel.subTrees(path.head)
        new PrefixTree[K, U](curTreeLevel.subTrees.+(path.head -> addValue(nextTreeLevel, path.tail)),
          curTreeLevel.value)
      }
    }
    addValue(curTreeLevel, path)
  }

  def sub(path: Seq[K]): IPrefixTree[K, V] = {
    val curTreeLevel = this
    @tailrec
    def getsubTrees(curTreeLevel : PrefixTree[K, V], path: Seq[K]): IPrefixTree[K, V] ={
      if (path.isEmpty) curTreeLevel
      else {
        if (curTreeLevel.subTrees.contains(path.head)) getsubTrees(curTreeLevel.subTrees(path.head), path.tail)
        else new PrefixTree[K, V](Map.empty, None)
      }
    }
    getsubTrees(curTreeLevel, path)
  }

  def get: V = value.get
}

