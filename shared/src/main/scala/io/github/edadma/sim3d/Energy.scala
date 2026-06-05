package io.github.edadma.sim3d

/** Conserved-quantity diagnostics. For an isolated gravitational system the
  * total energy and momentum are constants of motion, so watching how far they
  * wander is the cleanest measure of an integrator's quality.
  */
object Energy:
  /** Total kinetic energy, `sum 1/2 m v^2`. */
  def kinetic(s: State): Double =
    var k = 0.0
    var i = 0
    while i < s.n do
      k += 0.5 * s.mass(i) * s.vel(i).lengthSq
      i += 1
    k

  /** Total gravitational potential energy, `-G sum_{i<j} m_i m_j / r_ij`, using
    * the same softening as the force so the two stay consistent.
    */
  def potential(s: State, field: GravityField): Double =
    val eps2 = field.softening * field.softening
    var u    = 0.0
    var i    = 0
    while i < s.n do
      var j = i + 1
      while j < s.n do
        val r = math.sqrt((s.pos(j) - s.pos(i)).lengthSq + eps2)
        u -= field.g * s.mass(i) * s.mass(j) / r
        j += 1
      i += 1
    u

  def total(s: State, field: GravityField): Double = kinetic(s) + potential(s, field)

  /** Total linear momentum — should hold to round-off for every integrator
    * here, since all pair forces are equal and opposite.
    */
  def momentum(s: State): Vec3 =
    var p = Vec3.zero
    var i = 0
    while i < s.n do
      p = p + s.vel(i) * s.mass(i)
      i += 1
    p
