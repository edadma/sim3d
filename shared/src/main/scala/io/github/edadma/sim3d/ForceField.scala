package io.github.edadma.sim3d

/** Computes the acceleration on every body given the current configuration.
  *
  * The integrators depend only on this interface, never on gravity specifically,
  * so swapping in a different interaction (springs, Coulomb, a future contact
  * model for rigid bodies) is a matter of providing another `ForceField`.
  */
trait ForceField:
  /** Write the acceleration of each body into `acc` (length `n`), given the
    * current `pos` and `mass`. Implementations overwrite `acc` entirely.
    */
  def accelerations(pos: Array[Vec3], mass: Array[Double], acc: Array[Vec3]): Unit

/** Newtonian mutual gravity, summed directly over all pairs in O(n^2).
  *
  * A Plummer softening length keeps the `1/r^2` force finite during close
  * encounters: the effective separation is `sqrt(r^2 + softening^2)`, which
  * removes the singularity without noticeably affecting well-separated bodies.
  * Set `softening = 0` for exact point-mass gravity (fine when bodies never
  * approach closely, e.g. the figure-eight choreography).
  */
final class GravityField(val g: Double = 1.0, val softening: Double = 0.0) extends ForceField:
  private val eps2 = softening * softening

  def accelerations(pos: Array[Vec3], mass: Array[Double], acc: Array[Vec3]): Unit =
    val n = pos.length
    var i = 0
    while i < n do
      acc(i) = Vec3.zero
      i += 1

    // Each unordered pair contributes equal-and-opposite accelerations, so we
    // visit j > i once and apply both halves.
    i = 0
    while i < n do
      var j = i + 1
      while j < n do
        val d     = pos(j) - pos(i)
        val r2    = d.lengthSq + eps2
        val invR  = 1.0 / math.sqrt(r2)
        val invR3 = invR * invR * invR
        acc(i) = acc(i) + d * (g * mass(j) * invR3)
        acc(j) = acc(j) - d * (g * mass(i) * invR3)
        j += 1
      i += 1
