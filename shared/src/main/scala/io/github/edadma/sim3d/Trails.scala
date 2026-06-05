package io.github.edadma.sim3d

import scala.collection.mutable.ArrayDeque

/** A bounded ring of recent positions per body, used to draw orbit trails. The
  * animation loop calls [[record]] once per displayed frame; old points fall off
  * the tail once `maxLen` is reached.
  */
final class Trails(n: Int, val maxLen: Int):
  private val bufs = Array.fill(n)(new ArrayDeque[Vec3](maxLen + 1))

  def record(pos: Array[Vec3]): Unit =
    var i = 0
    while i < n do
      val b = bufs(i)
      b.append(pos(i))
      while b.length > maxLen do b.removeHead()
      i += 1

  /** Recent points for body `i`, oldest first. */
  def points(i: Int): collection.IndexedSeq[Vec3] = bufs(i)

  def clear(): Unit =
    var i = 0
    while i < n do
      bufs(i).clear()
      i += 1
