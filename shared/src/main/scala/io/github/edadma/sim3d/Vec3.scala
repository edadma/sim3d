package io.github.edadma.sim3d

/** An immutable 3D vector of `Double`s. Doubles as a point or a displacement.
  *
  * Kept deliberately small and allocation-friendly: every operation returns a
  * fresh `Vec3` so the physics code can read like the math it implements. Hot
  * inner loops (e.g. the O(n^2) gravity sum) still go through these, which the
  * JIT/Native optimizer handles well for the body counts we target.
  */
final case class Vec3(x: Double, y: Double, z: Double):
  def +(o: Vec3): Vec3 = Vec3(x + o.x, y + o.y, z + o.z)
  def -(o: Vec3): Vec3 = Vec3(x - o.x, y - o.y, z - o.z)
  def *(s: Double): Vec3 = Vec3(x * s, y * s, z * s)
  def /(s: Double): Vec3 = Vec3(x / s, y / s, z / s)
  def unary_- : Vec3 = Vec3(-x, -y, -z)

  def dot(o: Vec3): Double = x * o.x + y * o.y + z * o.z
  def cross(o: Vec3): Vec3 =
    Vec3(y * o.z - z * o.y, z * o.x - x * o.z, x * o.y - y * o.x)

  def lengthSq: Double = x * x + y * y + z * z
  def length: Double   = math.sqrt(lengthSq)

  /** Unit vector in the same direction; the zero vector maps to itself. */
  def normalized: Vec3 =
    val l = length
    if l == 0.0 then this else this / l

object Vec3:
  val zero: Vec3 = Vec3(0.0, 0.0, 0.0)
