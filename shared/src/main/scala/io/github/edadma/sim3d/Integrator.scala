package io.github.edadma.sim3d

/** A time-stepping scheme for the second-order system `x'' = a(x)`.
  *
  * Each integrator advances `state.pos`/`state.vel` by one step of size `dt`
  * under `field`, mutating in place. They are interchangeable — the simulation
  * holds one and can swap it live — which is the whole point: the same scenario
  * run with [[ExplicitEuler]] versus [[Leapfrog]] makes the value of a *good*
  * (symplectic) integrator visible, because only the symplectic ones keep the
  * energy bounded over long runs.
  */
trait Integrator:
  def name: String

  /** A one-line note on the method's character (order, symplectic or not). */
  def blurb: String

  def step(state: State, dt: Double, field: ForceField): Unit

  /** Convenience: compute accelerations for `pos` into a fresh array. */
  protected final def accelOf(pos: Array[Vec3], mass: Array[Double], field: ForceField): Array[Vec3] =
    val acc = new Array[Vec3](pos.length)
    field.accelerations(pos, mass, acc)
    acc

/** Forward (explicit) Euler: first order, NOT symplectic.
  *
  * Updates position from the *old* velocity. Cheap and famously bad for orbits —
  * it systematically injects energy, so bound orbits spiral outward. Included as
  * the cautionary baseline.
  */
object ExplicitEuler extends Integrator:
  val name  = "Explicit Euler"
  val blurb = "1st order, non-symplectic — energy grows, orbits spiral out"

  def step(s: State, dt: Double, field: ForceField): Unit =
    val acc = accelOf(s.pos, s.mass, field)
    var i   = 0
    while i < s.n do
      val v = s.vel(i)
      s.pos(i) = s.pos(i) + v * dt        // uses velocity from the start of the step
      s.vel(i) = v + acc(i) * dt
      i += 1

/** Semi-implicit (symplectic) Euler: first order, symplectic.
  *
  * Identical cost to explicit Euler but updates velocity first and drifts with
  * the *new* velocity. That one reordering makes it symplectic, so energy stays
  * bounded (it oscillates rather than drifting away).
  */
object SymplecticEuler extends Integrator:
  val name  = "Symplectic Euler"
  val blurb = "1st order, symplectic — energy bounded but noisy"

  def step(s: State, dt: Double, field: ForceField): Unit =
    val acc = accelOf(s.pos, s.mass, field)
    var i   = 0
    while i < s.n do
      val v = s.vel(i) + acc(i) * dt
      s.vel(i) = v
      s.pos(i) = s.pos(i) + v * dt        // drifts with the updated velocity
      i += 1

/** Velocity Verlet / leapfrog (kick–drift–kick): second order, symplectic,
  * time-reversible.
  *
  * The workhorse of gravitational dynamics. Half-kick the velocity, drift the
  * position a full step, recompute accelerations, then half-kick again. Energy
  * error stays bounded and oscillatory for as long as you run it, at the cost of
  * essentially one force evaluation per step.
  */
object Leapfrog extends Integrator:
  val name  = "Velocity Verlet (leapfrog)"
  val blurb = "2nd order, symplectic — the workhorse; energy bounded"

  def step(s: State, dt: Double, field: ForceField): Unit =
    val half = dt * 0.5
    val a1   = accelOf(s.pos, s.mass, field)
    var i    = 0
    while i < s.n do
      s.vel(i) = s.vel(i) + a1(i) * half  // kick
      s.pos(i) = s.pos(i) + s.vel(i) * dt // drift
      i += 1
    val a2 = accelOf(s.pos, s.mass, field)
    i = 0
    while i < s.n do
      s.vel(i) = s.vel(i) + a2(i) * half  // kick
      i += 1

/** Classical fourth-order Runge–Kutta: fourth order, NOT symplectic.
  *
  * Very accurate per step, so it tracks a trajectory beautifully over short
  * spans. But it is not symplectic, so the energy slowly and monotonically
  * drifts over very long integrations — accuracy and conservation are different
  * properties, which this method versus leapfrog makes concrete.
  */
object Rk4 extends Integrator:
  val name  = "Runge–Kutta 4"
  val blurb = "4th order, non-symplectic — accurate but slow energy drift"

  def step(s: State, dt: Double, field: ForceField): Unit =
    val n    = s.n
    val mass = s.mass
    val x0   = s.pos
    val v0   = s.vel

    def accAt(pos: Array[Vec3]): Array[Vec3] = accelOf(pos, mass, field)
    def addScaled(base: Array[Vec3], delta: Array[Vec3], h: Double): Array[Vec3] =
      Array.tabulate(n)(i => base(i) + delta(i) * h)

    // For x'' = a(x): each stage's slope is (velocity, acceleration-at-position).
    val k1x = v0
    val k1v = accAt(x0)
    val k2x = addScaled(v0, k1v, dt / 2)
    val k2v = accAt(addScaled(x0, k1x, dt / 2))
    val k3x = addScaled(v0, k2v, dt / 2)
    val k3v = accAt(addScaled(x0, k2x, dt / 2))
    val k4x = addScaled(v0, k3v, dt)
    val k4v = accAt(addScaled(x0, k3x, dt))

    val h = dt / 6.0
    var i = 0
    while i < n do
      s.pos(i) = x0(i) + (k1x(i) + k2x(i) * 2.0 + k3x(i) * 2.0 + k4x(i)) * h
      s.vel(i) = v0(i) + (k1v(i) + k2v(i) * 2.0 + k3v(i) * 2.0 + k4v(i)) * h
      i += 1

/** Yoshida fourth-order symplectic integrator.
  *
  * A symmetric composition of three leapfrog-style sub-steps whose coefficients
  * cancel the second-order error term. The result is fourth-order accuracy *and*
  * bounded energy — accuracy approaching RK4 while keeping the long-term
  * conservation of leapfrog, for three force evaluations per step.
  */
object Yoshida4 extends Integrator:
  val name  = "Yoshida 4 (symplectic)"
  val blurb = "4th order, symplectic — accurate AND energy bounded"

  // Standard Yoshida coefficients from the triple-jump composition.
  private val cbrt2 = math.cbrt(2.0)
  private val w1    = 1.0 / (2.0 - cbrt2)
  private val w0    = -cbrt2 * w1
  private val c1    = w1 / 2.0
  private val c2    = (w0 + w1) / 2.0
  private val c3    = c2
  private val c4    = c1
  private val d1    = w1
  private val d2    = w0
  private val d3    = w1

  def step(s: State, dt: Double, field: ForceField): Unit =
    drift(s, c1 * dt); kick(s, d1 * dt, field)
    drift(s, c2 * dt); kick(s, d2 * dt, field)
    drift(s, c3 * dt); kick(s, d3 * dt, field)
    drift(s, c4 * dt)

  private def drift(s: State, h: Double): Unit =
    var i = 0
    while i < s.n do
      s.pos(i) = s.pos(i) + s.vel(i) * h
      i += 1

  private def kick(s: State, h: Double, field: ForceField): Unit =
    val a = accelOf(s.pos, s.mass, field)
    var i = 0
    while i < s.n do
      s.vel(i) = s.vel(i) + a(i) * h
      i += 1

object Integrator:
  /** Every integrator, ordered worst → best, for UI cycling and comparisons. */
  val all: Vector[Integrator] =
    Vector(ExplicitEuler, SymplecticEuler, Leapfrog, Rk4, Yoshida4)
