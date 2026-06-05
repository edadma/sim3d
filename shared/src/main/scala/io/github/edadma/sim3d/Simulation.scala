package io.github.edadma.sim3d

/** Ties together a [[State]], a [[ForceField]], and an [[Integrator]] and tracks
  * elapsed simulation time. The integrator is a `var` so a front-end can swap
  * methods on the fly to compare them on the same running system.
  */
final class Simulation(
    val state: State,
    val field: ForceField,
    var integrator: Integrator,
    var dt: Double,
):
  var time: Double = 0.0

  /** Advance one step of size `dt`. */
  def step(): Unit =
    integrator.step(state, dt, field)
    time += dt

  /** Advance `k` steps. */
  def steps(k: Int): Unit =
    var i = 0
    while i < k do
      step()
      i += 1
