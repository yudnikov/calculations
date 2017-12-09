package ru.yudnikov

package object utils {

  def reverseMap[K, V](map: Map[K, V]): Map[V, Iterable[K]] = {
    map.groupBy(_._2).mapValues(_.keys)
  }

}
