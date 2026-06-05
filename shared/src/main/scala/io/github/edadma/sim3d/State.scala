package io.github.edadma.sim3d

/** The mutable state a force-based integrator advances in time.
  *
  * Three parallel arrays indexed by body: `mass`, position `pos`, velocity
  * `vel`. Integrators mutate `pos`/`vel` in place; `mass` is constant. This is
  * the point-mass model used by the gravitational solver. Rigid-body state
  * (orientation, angular velocity, inertia) will live in a separate structure
  * layered on top of the same integrator machinery later.
  */
final class State(
    val mass: Array[Double],
    val pos: Array[Vec3],
    val vel: Array[Vec3],
):
  require(mass.length == pos.length && pos.length == vel.length, "State arrays must be the same length")

  def n: Int = mass.length

  /** A deep copy — safe to integrate independently of the original. */
  def duplicate(): State = new State(mass.clone(), pos.clone(), vel.clone())

object State:
  def apply(bodies: IndexedSeq[Body]): State =
    new State(
      bodies.map(_.mass).toArray,
      bodies.map(_.pos).toArray,
      bodies.map(_.vel).toArray,
    )

/** A single body's initial conditions, used when describing a scenario. */
final case class Body(mass: Double, pos: Vec3, vel: Vec3)
